library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(readr)
library(ggplot2)

# =========================
# CONFIG
# =========================

META_FILE <- "meta_counts.csv"

# IMPORTANT:
# Replace this with the correct Limitless endpoint for deck data.
# Example placeholder:
LIMITLESS_URL <- "https://play.limitlesstcg.com/api/tournaments/6a19c09b47f3797bf6ba6859/standings"

API_KEY <- NULL   # If you have one, put it here.


# =========================
# HELPERS
# =========================

load_meta <- function() {
  if (file.exists(META_FILE)) {
    read_csv(META_FILE, show_col_types = FALSE)
  } else {
    tibble(archetype = character(), count = integer())
  }
}

save_meta <- function(df) {
  write_csv(df, META_FILE)
}

fetch_limitless_decks <- function(limit = 128) {
  if (LIMITLESS_URL == "<PUT_LIMITLESS_DECKS_ENDPOINT_HERE>") {
    warning("Please configure LIMITLESS_URL in app.R")
    return(tibble())
  }

  headers <- list()
  if (!is.null(API_KEY)) headers[["X-Access-Key"]] <- API_KEY

  res <- GET(LIMITLESS_URL, add_headers(.headers = headers), query = list(limit = limit))
  stop_for_status(res)

  json <- content(res, as = "parsed", simplifyVector = TRUE)

  # Expecting list of deck objects with $deck$icons
  if (!"deck" %in% names(json[[1]])) {
    warning("Unexpected JSON structure. Check API endpoint.")
    return(tibble())
  }

  tibble(
    name = sapply(json, function(x) x$deck$name),
    icon = sapply(json, function(x) ifelse(length(x$deck$icons) > 0, x$deck$icons[1], NA))
  )
}

extract_attacker_words <- function(deck_names) {
  words <- unlist(str_split(deck_names, "\\s+"))
  words <- unique(words)
  words <- words[nchar(words) > 2]
  words <- words[!words %in% c("and", "with", "the", "Box", "Control", "Toolbox", "Deck")]
  words
}

detect_archetype <- function(deck_text, icons, attacker_words) {

  # 1) Try icon match
  icon_hits <- sapply(icons, function(ic) str_count(deck_text, fixed(ic, ignore_case = TRUE)))
  if (any(icon_hits > 0)) {
    return(icons[which.max(icon_hits)])
  }

  # 2) Try attacker name match
  attacker_hits <- sapply(attacker_words, function(a) str_count(deck_text, fixed(a, ignore_case = TRUE)))
  if (any(attacker_hits > 0)) {
    return(attacker_words[which.max(attacker_hits)])
  }

  # 3) Fallback
  return("Others")
}


# =========================
# UI
# =========================

ui <- fluidPage(
  titlePanel("Pokémon Decklist Meta Share Tracker"),

  sidebarLayout(
    sidebarPanel(
      h4("Submit Decklist"),
      textareaInput("deck_input", "Paste Decklist (Limitless style)", rows = 15, width = "100%"),
      actionButton("submit_btn", "Submit Decklist", class = "btn-primary"),

      hr(),
      textOutput("status")
    ),

    mainPanel(
      h3("Meta Share"),
      plotOutput("meta_plot"),
      hr(),
      tableOutput("meta_table")
    )
  )
)


# =========================
# SERVER
# =========================

server <- function(input, output, session) {

  meta <- reactiveVal(load_meta())

  # Fetch Limitless decks on startup
  limitless_data <- reactiveVal(tibble())

  observe({
    decks <- fetch_limitless_decks(100)
    limitless_data(decks)
  })

  # Build attacker list
  attacker_words <- reactive({
    decks <- limitless_data()
    if (nrow(decks) == 0) return(character())
    extract_attacker_words(decks$name)
  })

  # Build icon list
  icon_list <- reactive({
    decks <- limitless_data()
    decks$icon[!is.na(decks$icon)]
  })

  output$status <- renderText({
    paste(
      "Loaded attackers:", length(attacker_words()),
      "| Loaded icons:", length(icon_list()),
      "| Meta entries:", nrow(meta())
    )
  })

  # Handle submission
  observeEvent(input$submit_btn, {
    req(input$deck_input)

    arche <- detect_archetype(
      deck_text = input$deck_input,
      icons = icon_list(),
      attacker_words = attacker_words()
    )

    df <- meta()

    if (arche %in% df$archetype) {
      df$count[df$archetype == arche] <- df$count[df$archetype == arche] + 1
    } else {
      df <- bind_rows(df, tibble(archetype = arche, count = 1))
    }

    meta(df)
    save_meta(df)
  })

  # Plot
  output$meta_plot <- renderPlot({
    df <- meta()
    if (nrow(df) == 0) return(NULL)

    df <- df %>% mutate(share = count / sum(count) * 100)

    ggplot(df, aes(x = reorder(archetype, share), y = share)) +
      geom_col(fill = "#4C72B0") +
      coord_flip() +
      labs(x = "Archetype", y = "Meta Share (%)") +
      theme_minimal(base_size = 14)
  })

  # Table
  output$meta_table <- renderTable({
    df <- meta()
    if (nrow(df) == 0) return(NULL)
    df %>% mutate(share = round(count / sum(count) * 100, 1)) %>% arrange(desc(share))
  })
}

shinyApp(ui, server)
