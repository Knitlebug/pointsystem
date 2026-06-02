library(shiny)

# File where data is stored
data_file <- "points.txt"

# Load or initialize data
load_data <- function() {
  if (file.exists(data_file)) {
    read.csv(data_file, stringsAsFactors = FALSE)
  } else {
    data.frame(name = character(), points = numeric(), stringsAsFactors = FALSE)
  }
}

save_data <- function(df) {
  write.csv(df, data_file, row.names = FALSE)
}

ui <- fluidPage(
  titlePanel("Daily Username Points & Token Tracker"),

  sidebarLayout(
    sidebarPanel(
      textInput("username", "Enter Username Abbreviation"),
      actionButton("add_user", "Add Daily Point"),

      hr(),

      sliderInput("challenge_points", "Challenger Points", min = -10, max = 10, value = 1),
      actionButton("apply_challenge", "Apply Challenger Points"),

      hr(),
      h4("Monthly Settings"),

      sliderInput("days_total", "Days in Month", min = 1, max = 31, value = 30),
      sliderInput("challenger_daily", "Avg Challenger Points per Day", min = 0, max = 10, value = 1),

      hr(),
      h4("Max Tokens per Participant"),
      textOutput("max_tokens_display"),

      hr(),
      h4("Price Wall"),
      uiOutput("price_wall")
    ),

    mainPanel(
      h3("Usernames"),
      uiOutput("user_list"),

      hr(),
      h3("Monthly Ranking"),
      tableOutput("ranking")
    )
  )
)

server <- function(input, output, session) {

  # Reactive data frame
  users <- reactiveVal(load_data())

  # Save whenever data changes
  observeEvent(users(), {
    save_data(users())
  })

  # Add daily point (1 point per day)
  observeEvent(input$add_user, {
    req(input$username)
    df <- users()

    if (input$username %in% df$name) {
      df$points[df$name == input$username] <- df$points[df$name == input$username] + 1
    } else {
      df <- rbind(df, data.frame(name = input$username, points = 1))
    }

    users(df)
  })

  # Apply challenger points
  observeEvent(input$apply_challenge, {
    req(input$username)
    df <- users()

    if (input$username %in% df$name) {
      df$points[df$name == input$username] <- df$points[df$name == input$username] + input$challenge_points
    }

    users(df)
  })

  # Token calculation
  tokens <- reactive({
    df <- users()
    if (nrow(df) == 0) return(df)

    # 1 Token = 2 daily points + 1 challenger point
    df$tokens <- floor(df$points / 2)
    df
  })

  # Max tokens per participant
  max_tokens <- reactive({
    days <- input$days_total
    challenger <- input$challenger_daily

    # Max tokens = (2 daily points + 1 challenger point) * days / 2
    floor((days * (2 + challenger)) / 2)
  })

  output$max_tokens_display <- renderText({
    paste(max_tokens(), "tokens")
  })

  # Price wall
  output$price_wall <- renderUI({
    max_t <- max_tokens()

    tagList(
      h4("Small Prize: 1 Token"),
      h4(paste("Mini Acrylic:", max_t, "Tokens")),
      h4(paste("Prize Pack:", max_t * 0.5, "Tokens"))
    )
  })

  # Clickable usernames
  output$user_list <- renderUI({
    df <- tokens()

    if (nrow(df) == 0) return("No users yet.")

    tagList(
      lapply(1:nrow(df), function(i) {
        name <- df$name[i]
        points <- df$points[i]
        tks <- df$tokens[i]

        actionButton(
          inputId = paste0("user_", i),
          label = paste0(name, " (", points, " pts, ", tks, " tokens)"),
          width = "250px"
        )
      })
    )
  })

  # Modify points when clicking a username
  observe({
    df <- users()

    lapply(1:nrow(df), function(i) {
      btn <- paste0("user_", i)

      observeEvent(input[[btn]], {
        showModal(modalDialog(
          title = paste("Modify Points for", df$name[i]),
          sliderInput("modify_slider", "Adjust Points", min = -10, max = 10, value = 1),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_modify", "Apply")
          )
        ))

        observeEvent(input$confirm_modify, {
          df2 <- users()
          df2$points[i] <- df2$points[i] + input$modify_slider
          users(df2)
          removeModal()
        }, ignoreInit = TRUE)
      })
    })
  })

  # Monthly ranking
  output$ranking <- renderTable({
    df <- tokens()
    if (nrow(df) == 0) return(NULL)

    df <- df[order(-df$tokens), ]
    df
  })
}

shinyApp(ui, server)
