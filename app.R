library(ggplot2)
library(plyr)
library(dplyr)
library(shiny)
library(RMariaDB)
library(caret)
library(sqldf)
library(stringr)
library(shinydashboard)
library(data.table)
library(bslib)
library(ggmap)
# set memory limits
options(java.parameters = "-Xmx64048m") # 64048 is 64 GB

#set wd and pull in dataset
getwd()
setwd("/home/lammm/Final Project Team 3")
review_data <- read.csv("beer_reviews.csv")


#removes any reviews by users who only have 1 total review
review_data <- review_data %>% group_by(review_profilename) %>% filter((n() > 1))
#removes Brewery ID and Beer ID, which are not needed for this analysis
review_data <- subset(review_data, select = -c(brewery_id, review_time))

#Code from prior groups final project submission. Creates a parent beer style column, and categories beers based on names in the sub-style column that already exists in the dataset
review_data$Category<-"Ale"
review_data$Category<-ifelse(str_detect(review_data$beer_style,regex("Lager|Pils|Pilsener|Pilsner",ignore_case = TRUE,dotall=TRUE)),"Lager",as.character(review_data$Category))
review_data$Category<-ifelse(str_detect(review_data$beer_style,regex("Low Alcohol",ignore_case = TRUE,dotall=TRUE)),"Low Alcohol Beer",as.character(review_data$Category))
review_data$Category<-ifelse(str_detect(review_data$beer_style,regex(c("Other|Cider|Shandy|Fruit|Spiced|Saison"),ignore_case = TRUE,dotall=TRUE)),"Other",as.character(review_data$Category))
review_data$Category<-ifelse(is.na(review_data$Category),"Other",as.character(review_data$Category))
review_data$Category<-ifelse(str_detect(review_data$beer_style,regex(c("Berliner|Weissbier|Doppel|Dunkel|lsch|Altbier|Gose|Witbier|Oktoberfest|bier|Tripel|Hefeweizen|Radler|Belgian|Bier|Maibock|Dubbel|Quad"),ignore_case = TRUE,dotall=TRUE)),"Traditional Germanic",as.character(review_data$Category))

#changes column classes
review_data$Category <- as.factor(review_data$Category)
review_data$beer_style <- as.factor(review_data$beer_style)
review_data$beer_name <- as.factor(review_data$beer_name)
review_data$brewery_name <- as.factor(review_data$brewery_name)
review_data$review_profilename <- as.factor(review_data$review_profilename)


#create subset dataframe, with fields used to predict missing ABV's
abvdata <- sqldf("select beer_beerid, beer_style,  beer_abv, review_aroma, review_palate, review_taste from review_data")

#shows how many missing values are in the beer_abv column before prediction model
sum(is.na(review_data$beer_abv))


#one-hot encodes factor variables. This takes 2ish minutes to run
dummies <- dummyVars(beer_abv ~ .,data=abvdata)
ex <- data.frame(predict(dummies, newdata=abvdata))
names(ex) <-gsub("\\.","",names(ex))
abvdata <- cbind(abvdata$beer_abv,ex)
names(abvdata)[1] <- "beer_abv"
rm(dummies,ex)

#removes zero variables. This takes 2ish minutes to run
nzv <- nearZeroVar(abvdata, saveMetrics = TRUE)
abvdata <- abvdata[, c(TRUE,!nzv$zeroVar[3:ncol(abvdata)])]

rm(nzv)

#standardizes numeric columns, such as the beer ratings. This runs for about a minute
preProcValues <- preProcess(abvdata[,3:ncol(abvdata)], method = c("range"))
abvdata <- predict(preProcValues, abvdata)

rm(preProcValues)

# intial test for predictive model accuracy
# complete_review_data <- abvdata[which(!is.na(abvdata["beer_abv"])),]
# missing_review_data <- abvdata[which(is.na(abvdata["beer_abv"])),]
# intrain <- createDataPartition(y=complete_review_data$beer_abv,p=.80,list=F)
# train <- complete_review_data[intrain,]
# test <- complete_review_data[-intrain,]

#trains model with linear regression. This can take up to 5 minutes to run
#ctrl <- trainControl(method="cv",number=5,classProbs = F, summaryFunction = defaultSummary, allowParallel=T)
#m1<- train(beer_abv ~ . - beer_beerid, data = train, method = "lm", trControl = ctrl,na.action = na.pass)

#results from the intial testing.
# defaultSummary(data=data.frame(obs=train$beer_abv, pred=predict(m1, newdata=train))
#                , model=m1)
# RMSE  Rsquared       MAE 
# 1.3585522 0.6579420 0.8551176 
# 
# 
# defaultSummary(data=data.frame(obs=test$beer_abv, pred=predict(m1, newdata=test))
#                , model=m1)
# RMSE  Rsquared       MAE 
# 1.3466819 0.6606197 0.8526245 

#Once model accurary was determined, this code splits train/test data. Train data is all beers with an ABV. Test data is missing ABV
train <- abvdata[which(!is.na(abvdata["beer_abv"])),]
test <- abvdata[which(is.na(abvdata["beer_abv"])),]

#trains model with linear regression. This can take up to 5 minutes to run
ctrl <- trainControl(method="cv",number=5,classProbs = F, summaryFunction = defaultSummary, allowParallel=T)
m1<- train(beer_abv ~ . - beer_beerid, data = train, method = "lm", trControl = ctrl,na.action = na.pass)


#generates predictions and fills in missing ABV in primary dataframe, with predictions from the model. Uses Beer ID to match the two datasets
preds <- data.frame(predict(m1, newdata=test,na.action=na.pass))
preds<- mutate(preds,"beer_beerid"=as.numeric(rownames(preds)))
review_data[which(is.na(review_data$beer_abv)),"beer_abv"]<-ifelse(as.integer(preds$predict.m1..newdata...test.)>0,as.integer(preds$predict.m1..newdata...test.),review_data$beer_abv)


rm(ctrl, m1, preds, abvdata, test, train)

#shows how many missing values are in the beer_abv column after prediction model
sum(is.na(review_data$beer_abv))

#removes beer_ID, and the detailed review info from the dataset, since it is not needed after the prediction model, and writes the file into a CSV
review_data <- subset(review_data, select = -c(beer_beerid, review_aroma, review_appearance, review_taste, review_palate))


#load the data

beers = as.data.frame(review_data)
conv = as.data.frame(rep(NA,nrow(beers)))



fac = c(1,3,4,5,7)
num = c(2,6)
beertitle = colnames(beers)
fac_name = colnames(beers[,fac])
num_name = colnames(beers[,num])


#convert numeric
for (i in 1:length(num_name)){
  hold = as.data.frame(as.numeric(unlist(beers[num_name[i]])))
  names(hold) = num_name[i]
  conv = cbind(conv,hold)
}



#convert factors
for (j in 1:length(fac_name)){
  hold = as.data.frame(as.factor(unlist(beers[fac_name[j]])))
  names(hold) = fac_name[j]
  conv = cbind(conv,hold)
}



####total cleanup

beerchange = conv[-1]
beerchange = na.omit(beerchange)



####prepare
beercluster_a = beerchange['review_overall']
beercluster_b = beerchange['beer_abv']
beercluster = cbind(beercluster_a,beercluster_b)


rm(beercluster_a, beercluster_b)

####K test for optimal outputs
mse_test = data.frame()



for (x in 1:15){
  kmean_user = kmeans(x=beercluster, centers=x, iter.max=15)
  
  mse_test = rbind(mse_test, cbind(x, kmean_user$tot.withinss))
}



#####look for the elbow

ggplot(data=mse_test, aes(x=x, y=V2)) + geom_point()




#####calculate the beers into k groups (5), we can change the code in centers for the user sensitivity (5,10,16)
quads = kmeans(x=beercluster, centers=5, iter.max=15)
bc = as.data.frame(quads$cluster)
names(bc) = "cluster5" 
beerchange = cbind(beerchange,bc)
br = beerchange



#####calculate the beers into k groups (10), we can change the code in centers for the user sensitivity (5,10,16)
quads = kmeans(x=beercluster, centers=10, iter.max=15)
bc = as.data.frame(quads$cluster)
names(bc) = "cluster10" 
beerchange = cbind(beerchange,bc)
br = beerchange



#####calculate the beers into k groups (16), we can change the code in centers for the user sensitivity (5,10,16)
quads = kmeans(x=beercluster, centers=16, iter.max=15)
bc = as.data.frame(quads$cluster)
names(bc) = "cluster16" 
beerchange = cbind(beerchange,bc)
br = beerchange


#export to csv
write.csv(br,"beer_review_clustered.csv", row.names = FALSE,)

br = read.csv("beer_review_clustered.csv")

ui <- fluidPage(
  dashboardPage(skin="red",
                
                dashboardHeader(
                  title= span(
                    "Beer Find-R",
                    style = "font-family: Tahoma; font-weight: bold"
                  ),
                  titleWidth = "200px"
                ),  
                
                #inputs on left hand side
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Beer Reccomendations", tabName = "beers", icon = icon("th-list")),
                    menuItem("Find Beer", tabName = "map", icon = icon("map-marker"))
                  )
                  
                ),
                #output panel for different tabs
                dashboardBody(
                  tabItems(
                    tabItem(tabName="beers",
                            fluidRow(
                              box(title="Please expand viewing window for best experience", tableOutput("ChosenBeers")),
                              box(
                                title="Beer Inputs",
                                selectizeInput(inputId='BeerNames',"Please add 2-3 of your favorite beers below", choices = NULL, multiple = TRUE),
                                selectInput(inputId="goal", label="What are you trying to achieve?", choices= c("Drink something within my comfort zone", "Try a new flavor", "Get Drunk", "Day Drink"), multiple=FALSE),
                                actionButton("run","Show Reccomended Beers")
                              )
                            )  
                    ),
                    
                    tabItem(tabName="map", 
                            fluidRow(
                              box(title="Please expand viewing window for best experience. An error below means there is no beer in your area.", plotOutput("gmap")),
                              box(
                                title="Help Finding Beer- Please wait 5-10 second after clicking Find Beers button for map output",
                                textInput(inputId="zip", label="What is your zip code?"),
                                actionButton("map","Find Beers Near Me")
                              )
                            )  
                    )
                  )
                )
  )
)
server <- function(input, output, session) {
  # render the beer choices in the server code for efficiency
  updateSelectizeInput(session, 'BeerNames', choices = sort(as.character(sort(unique(br$beer_name)))), server = TRUE)
  
  
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
    
  })
  
  observeEvent(input$map,{
    output$gmap <- renderPlot({
      key <- Sys.getenv("GMAPS")
      
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
      
      
      rm(beer.details)
      beer.details <- rbindlist(lapply(beer_address$results, function(x) data.frame(as.numeric(x$geometry$location$lat[[1]]),
                                                                                    as.numeric(x$geometry$location$lng[[1]]),
                                                                                    as.character(x$name[[1]]),
                                                                                    as.character(x$vicinity[[1]]))))
      
      setnames(beer.details, c("Latitude","Longitude","Name","Address"))
      
      register_google(key = key)
      map <- get_map(location = c(longitude,latitude), zoom = 14, maptype = "roadmap", scale = 2)
      ggmap(map) +
        geom_point(data = beer.details, aes(x = Longitude, y = Latitude, colour = Name),
                   size = 8, alpha = 0.3, shape = 19)
    })})
}
shinyApp(ui, server)
