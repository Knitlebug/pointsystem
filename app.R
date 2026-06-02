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
  titlePanel("Daily Username Points Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Enter Username Abbreviation"),
      actionButton("add_user", "Add Daily Point"),
      
      hr(),
      
      sliderInput("challenge_points", "Challenger Points", min = -10, max = 10, value = 1),
      actionButton("apply_challenge", "Apply Challenger Points")
    ),
    
    mainPanel(
      h3("Usernames"),
      uiOutput("user_list")
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
  
  # Clickable usernames
  output$user_list <- renderUI({
    df <- users()
    
    if (nrow(df) == 0) return("No users yet.")
    
    tagList(
      lapply(1:nrow(df), function(i) {
        name <- df$name[i]
        points <- df$points[i]
        
        actionButton(
          inputId = paste0("user_", i),
          label = paste0(name, " (", points, " pts)"),
          width = "200px"
        )
      })
    )
  })
  
  # Add/subtract points when clicking a username
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
}

shinyApp(ui, server)
