library(shiny)
ui <- fluidPage(
  titlePanel("Click one of the videos below to explore different styles of beer!"),
  #sidebarLayout(
   # mainPanel(
      h1("German Beer"),  #german beer
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/SMu_bAaag_8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      h1("Dark Beers"), #dark beers
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/b6wyb82O8o8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      h1("Other Beers"), #other
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/HUPEv6gHr1w" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      h1("Low Alcohol Beer"), #low alcohol beer
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/w4AeXU5IOa8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
      h1("Craft Beer Overview"), #high level overview
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/P75SvA344QI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  #  )
 # )
)
server <- function(input, output, session) {
}
shinyApp(ui, server)
