library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

ui <- fluidPage(
  titlePanel("HW04 Shiny App"),
  sidebarLayout(
    sidebarPanel(

# collect the overall variable input
      selectInput("vars", "Variables:", 
                  choices = names(mtcars), 
                  selected = c("mpg", "cyl"), multiple = TRUE),

# Collect the discrete variable input
      selectInput("discrete_var", "Select discrete variable:", 
                  choices = c("cyl", "vs", "am", "gear", "carb")),

# Collect the continuous variable input
      selectInput("continuous_var", "Select continuous variable:", 
                  choices = c("mpg", "disp", "hp", "drat", "wt", "qsec"))
    ),

# Create tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Data", 
                 tableOutput("data_table")),
        tabPanel("Summary", 
                 verbatimTextOutput("summary_text")),
        tabPanel("BoxPlot", 
                 plotOutput("box_plot")),
        tabPanel("Bar", 
                 plotOutput("bar_plot")),
        tabPanel("Histogram", 
                 plotOutput("hist_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_data <- reactive({
    mtcars %>% select(all_of(input$vars))
  })
  
  output$data_table <- renderTable({
    selected_data()
  })
  
  output$summary_text <- renderPrint({
    vars_to_use <- unique(c(input$vars, input$discrete_var))
    df <- mtcars %>% select(any_of(vars_to_use))
    
    # Coerce the discrete variable to factor if it's present
    if (input$discrete_var %in% names(df)) {
      df[[input$discrete_var]] <- as.factor(df[[input$discrete_var]])
    }
    
    list(
      Continuous = summary(df[sapply(df, is.numeric)]),
      Discrete = summary(df[sapply(df, is.factor)])
    )
  })
  
  output$box_plot <- renderPlot({
    req(input$discrete_var, input$continuous_var)
    ggplot(mtcars, aes_string(x = input$discrete_var, y = input$continuous_var)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("Boxplot of", input$continuous_var, "by", input$discrete_var),
           x = input$discrete_var, y = input$continuous_var)
  })
  
  output$bar_plot <- renderPlot({
    req(input$discrete_var)
    ggplot(mtcars, aes_string(x = input$discrete_var)) +
      geom_bar(fill = "coral") +
      labs(title = paste("Barplot of", input$discrete_var), x = input$discrete_var, y = "Count")
  })
  
  output$hist_plot <- renderPlot({
    req(input$continuous_var)
    ggplot(mtcars, aes_string(x = input$continuous_var)) +
      geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
      labs(title = paste("Histogram of", input$continuous_var), x = input$continuous_var, y = "Frequency")
  })
}

shinyApp(ui, server)
