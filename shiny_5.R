# lab - shiny_5

library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
    label = "Choose a number", 
    value = 25, min = 1, max = 100),
  plotOutput("hist"),
  dataTableOutput("values")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
  
  output$values <- renderDataTable({
    as.data.frame(rnorm(input$num))
    
  })
}

shinyApp(ui = ui, server = server)

