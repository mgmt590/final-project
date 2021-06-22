library(shiny)
library(dplyr)

setwd("/Users/nilofer/Downloads")
br = read.csv("beer_review_clustered.csv")

ui <- fluidPage(
    # inputs
    selectizeInput(inputId='BeerNames',"Please add 3-5 of your favorite beers below", choices = NULL, multiple = TRUE),
    selectInput(inputId="goal", label="What are you trying to achieve?", choices= c("Get Drunk", "Day Drink", "No Preference"), multiple=FALSE),
    
    # outputs
    textOutput("result"),
    textOutput("user"),
    tableOutput("ChosenBeers")

    )
server <- function(input, output, session) {
    updateSelectizeInput(session, 'BeerNames', choices = sort(as.character(sort(unique(br$beer_name)))), server = TRUE)
    
    output$result <- renderText({
        paste("You chose", input$BeerNames)
    })
    
    output$ChosenBeers <- renderTable ({
        #br[which(br$beer_name %in% input$BeerNames),]
        # br %>%
        #     filter(br$beer_name == input$BeerNames)
        user_beers <- input$BeerNames
        chosen_clusters <- br %>%
            filter(br$beer_name == user_beers) %>%
            group_by(beer_name) %>%
            distinct(cluster16)
        
        getmode <- function(v) {
            uniqv <- unique(v)
            uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        
        user_cluster <- getmode(chosen_clusters$cluster16)
        
        user_category <- br %>%
            filter(br$beer_name == user_beers & br$cluster16 == user_cluster) %>%
            distinct(Category)
        
        user = as.numeric(user_cluster)
        pref = as.character(user_category)
        bang = br %>% 
            filter(cluster16 == user, Category == pref, review_overall >= 3) #, Categroy = pref
        
        bang_list = floor(runif(10, min =1, max = nrow(bang)))
        
        beer_rec = bang[bang_list,]
    })



    output$user <- renderText({
        user_beers <- input$BeerNames
        chosen_clusters <- br %>%
            filter(br$beer_name == user_beers) %>%
            group_by(beer_name) %>%
            distinct(cluster16)
        
        getmode <- function(v) {
            uniqv <- unique(v)
            uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        
        user_cluster <- getmode(chosen_clusters$cluster16)
        
        user_category <- br %>%
            filter(br$beer_name == user_beers & br$cluster16 == user_cluster) %>%
            distinct(Category)
        
        user = as.numeric(user_cluster)
        pref = as.character(user_category)
        paste("Your cluster", user, "Your pref", pref)
    })

}
shinyApp(ui, server)
