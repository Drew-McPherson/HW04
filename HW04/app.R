library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

ui <- fluidPage(
  titlePanel("HW04 Shiny App"),
  sidebarLayout(
    sidebarPanel(
      # User selects any variables
      selectInput("vars", "Variables:", 
                  choices = names(mtcars), 
                  selected = c("mpg", "cyl"), multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data_table")),
        tabPanel("Summary", verbatimTextOutput("summary_text")),
        tabPanel("BoxPlot", plotOutput("box_plot")),
        tabPanel("Bar", plotOutput("bar_plot")),
        tabPanel("Histogram", plotOutput("hist_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_data <- reactive({
    mtcars %>% select(all_of(input$vars))
  })
  
  # Automatically classify variables
  variable_types <- reactive({
    df <- selected_data()
    discrete_vars <- names(df)[sapply(df, function(x) is.numeric(x) && n_distinct(x) <= 10)]
    continuous_vars <- names(df)[sapply(df, function(x) is.numeric(x) && n_distinct(x) > 10)]
    
    list(discrete = discrete_vars, continuous = continuous_vars)
  })
  
  output$data_table <- renderTable({
    selected_data()
  })
  
  output$summary_text <- renderPrint({
    df <- selected_data()
    
    # Coerce discrete variables to factors
    for (var in variable_types()$discrete) {
      df[[var]] <- as.factor(df[[var]])
    }
    
    list(
      Continuous = summary(df[sapply(df, is.numeric)]),
      Discrete = summary(df[sapply(df, is.factor)])
    )
  })
  
  output$box_plot <- renderPlot({
    types <- variable_types()
    req(length(types$discrete) > 0, length(types$continuous) > 0)
    
    ggplot(mtcars, aes_string(x = types$discrete[1], y = types$continuous[1])) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("Boxplot of", types$continuous[1], "by", types$discrete[1]),
           x = types$discrete[1], y = types$continuous[1])
  })
  
  output$bar_plot <- renderPlot({
    types <- variable_types()
    req(length(types$discrete) > 0)
    
    ggplot(mtcars, aes_string(x = types$discrete[1])) +
      geom_bar(fill = "coral") +
      labs(title = paste("Barplot of", types$discrete[1]), x = types$discrete[1], y = "Count")
  })
  
  output$hist_plot <- renderPlot({
    types <- variable_types()
    req(length(types$continuous) > 0)
    
    ggplot(mtcars, aes_string(x = types$continuous[1])) +
      geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
      labs(title = paste("Histogram of", types$continuous[1]), x = types$continuous[1], y = "Frequency")
  })
}

shinyApp(ui, server)
