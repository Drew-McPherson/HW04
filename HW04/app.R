library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("HW04 Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("vars", "Variables:", 
                  choices = names(mtcars), 
                  selected = c("mpg", "cyl"), multiple = TRUE),
      selectInput("discrete_var", "Select discrete variable:", 
                  choices = names(mtcars)[sapply(mtcars, function(x) is.factor(x) || is.integer(x))]),
      selectInput("continuous_var", "Select continuous variable:", 
                  choices = names(mtcars)[sapply(mtcars, is.numeric)])
    ),
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


shinyApp(ui, server)
