library(shiny)
library(dplyr)
library(bslib)

br = read.csv("beer_review_clustered.csv")

ui <- fluidPage(
  #adds color teheme
  theme = bs_theme(version=4,bootswatch = "minty"),
  
  titlePanel("Beer Find-R"),
  
  #inputs on left hand side
  sidebarPanel(
    selectizeInput(inputId='BeerNames',"Please add 3-5 of your favorite beers below", choices = NULL, multiple = TRUE),
    selectInput(inputId="goal", label="What are you trying to achieve?", choices= c("Get Drunk", "Day Drink", "Drink something within my comfort zone", "Try a new flavor"), multiple=FALSE),
    actionButton("run","Show Reccomended Beers"),
    textInput(inputId="zip", label="What is your zip code?"),
    actionButton("map","Find Beers Near Me")
  ),
  #output panel for different tabs
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Beer Reccomendations", textOutput("result"),textOutput("user"),tableOutput("ChosenBeers")),
                tabPanel("Help Finding Beer", plotOutput("gmap"))
    )
  )
  
)
server <- function(input, output, session) {
  # render the beer choices in the server code for efficiency
  updateSelectizeInput(session, 'BeerNames', choices = sort(as.character(sort(unique(br$beer_name)))), server = TRUE)
  
  # render the selections
  output$result <- renderText({
    paste("You chose", input$BeerNames)
  })
  
  # render the bangers tables
  observeEvent(input$run,{
    output$ChosenBeers <- renderTable ({
      # get names of the beers and find the distinct clusters each beer is in
      user_beers <- input$BeerNames
      chosen_clusters <- br %>%
        filter(br$beer_name == user_beers) %>%
        group_by(beer_name) %>%
        distinct(cluster16)
      
      # define a mode function
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      
      # get the mode of the distinct clusters and assign the user to that cluster
      user_cluster <- getmode(chosen_clusters$cluster16)
      
      # assign the user to a category based off the beers they chose and the cluster they are in
      user_category <- br %>%
        filter(br$beer_name == user_beers & br$cluster16 == user_cluster) %>%
        distinct(Category)
      
      # coerce the cluster and category to correct types
      user = as.numeric(user_cluster)
      pref = as.character(user_category)
      
      # determine the banger recommendations based off of user cluster and category
      if (input$goal == "Drink something within my comfort zone"){
        bang = br %>% 
          filter(cluster16 == user, Category == pref, review_overall >= 3) %>%
          group_by(beer_name) %>%
          arrange(desc(review_overall)) %>%
          filter(row_number()==1) %>%
          select(-c(review_profilename, Category, cluster5, cluster10, cluster16))
        
        beer_rec = head(bang, n = 10)
      } else if (input$goal == "Try a new flavor"){
        risk = br %>% 
          filter(cluster16 == user, Category != pref, review_overall > 3) %>%
          group_by(beer_name) %>%
          arrange(desc(review_overall)) %>%
          filter(row_number()==1) %>%
          select(-c(review_profilename, Category, cluster5, cluster10, cluster16))
        
        beer_rec = head(risk, n = 10)
      } else if (input$goal == "Get Drunk"){
        drunk = br %>%
          filter(cluster16 == user, Category == pref, beer_abv >= 7) %>%
          group_by(beer_name) %>%
          arrange(desc(review_overall)) %>%
          filter(row_number()==1) %>%
          select(-c(review_profilename, Category, cluster5, cluster10, cluster16))
        
        drunk_backup = br %>%
          filter(beer_abv>=7) %>%
          group_by(beer_name) %>%
          arrange(desc(review_overall)) %>%
          filter(row_number()==1) %>%
          select(-c(review_profilename, Category, cluster5, cluster10, cluster16))
        
        drunk_bind <- rbind(drunk, drunk_backup)
        
        beer_rec = head(drunk_bind, n = 10)
        } else if (input$goal == "Day Drink") {
          sober = br %>%
            filter(cluster16 == user, Category == pref,  beer_abv <= 4) %>%
            group_by(beer_name) %>%
            arrange(desc(review_overall)) %>%
            filter(row_number()==1) %>%
            select(-c(review_profilename, Category, cluster5, cluster10, cluster16))
          
          sober_backup = br %>%
            filter(beer_abv<=4) %>%
            group_by(beer_name) %>%
            arrange(desc(review_overall)) %>%
            filter(row_number()==1) %>%
            select(-c(review_profilename, Category, cluster5, cluster10, cluster16))
          
          sober_bind <- rbind(sober, sober_backup)

          beer_rec = head(sober_bind, n = 10)
        }
      
      
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
  })
  
  observeEvent(input$map,{
    output$gmap <- renderPlot({
      key = ${{secrets.GMAPS}}
      
      postcode = input$zip
      
      API_request <- "https://maps.googleapis.com/maps/api/geocode/json?address="
      API_request <- paste(API_request,postcode,paste("&key=",key,sep=""),sep="")
      
      address <- rjson::fromJSON(file=API_request)
      
      if(address$status != "OK"){
        stop("Google cannot find beers in your area")
      }
      
      latitude <- address$results[[1]]$geometry$location$lat
      longitude <- address$results[[1]]$geometry$location$lng
      
      nearby_API_request <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
      radius_key <- "&radius=1500&keyword=beer"
      
      API_request <- paste(nearby_API_request,"location=", paste(latitude,longitude,sep=","), radius_key, paste("&key=",key,sep=""),sep="")
      
      beer_address <- rjson::fromJSON(file = API_request, unexpected.escape = "keep")
      
      if(beer_address$status != "OK"){
        stop("Google cannot find beers in your area")
      }
      
      library(data.table)
      rm(beer.details)
      beer.details <- rbindlist(lapply(beer_address$results, function(x) data.frame(as.numeric(x$geometry$location$lat[[1]]),
                                                                                    as.numeric(x$geometry$location$lng[[1]]),
                                                                                    as.character(x$name[[1]]),
                                                                                    as.character(x$vicinity[[1]]))))
      
      setnames(beer.details, c("Latitude","Longitude","Name","Address"))
      
      library(ggmap)
      register_google(key = key)
      map <- get_map(location = c(longitude,latitude), zoom = 14, maptype = "roadmap", scale = 2)
      ggmap(map) +
        geom_point(data = beer.details, aes(x = Longitude, y = Latitude, colour = Name),
                   size = 8, alpha = 0.3, shape = 19)
    })})
}
shinyApp(ui, server)
