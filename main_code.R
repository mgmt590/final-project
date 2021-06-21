library(ggplot2)
library(plyr)
library(dplyr)
library(shiny)
library(RMariaDB)
library(caret)
library(sqldf)
library(stringr)

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
write.table(review_data,"beersComplete.csv",sep=",")

########################
########################
########################


#load the data
getwd()
setwd("C:/Users/thepr/OneDrive - purdue.edu/MGMT 590 R for Analytics/Project")
beers<-read.csv("beersComplete.csv", header=TRUE, sep=",",row.names=NULL)

#clusterData <-reactive({
#  km<-kmeans(beersData()[,c("IBU","ABV")],centers=7,iter.max=15)
#  bc<-mutate(beersData(),"Group"=km$cluster)

beers = as.data.frame(beers)
conv = as.data.frame(rep(NA,nrow(beers)))

fac = c(1,5,6,9,11)
num = c(2,3,4,7,8,10)
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


####K test for optimal outputs
mse_test = data.frame()

for (x in 1:15){
  kmean_user = kmeans(x=beercluster, centers=x, iter.max=15)
  
  mse_test = rbind(mse_test, cbind(x, kmean_user$tot.withinss))
}



#####look for the elbow
library(ggplot2)
ggplot(data=mse_test, aes(x=x, y=V2)) + geom_point()


#####calculate the beers into k groups (5), we can change the code in centers for the user sensitivity (5,10,16)
quads = kmeans(x=beercluster, centers=16, iter.max=15)
bc = as.data.frame(quads$cluster)
names(bc) = "cluster16" 
beerchange = cbind(beerchange,bc)
br = beerchange

#export to scv
write.csv(br,"beer_review_clustered", row.names = FALSE)


###BANGER BEERS

user = 8 #HARDCODED, this will be the user 
pref = "Traditional Germanic" ###Category

library(dplyr)

bang = br %>% 
  filter(cluster16 == user, Category == pref, review_overall > 4) #, Categroy = pref

bang_list = floor(runif(10, min =1, max = nrow(bang)))

beer_rec = bang[bang_list,]
beer_rec

###RISKY BEERS

user = 2 #HARDCODED, this will be the user 
pref = "Traditional Germanic" ###Category

risk = br %>% 
  filter(cluster == user, Category != pref, review_overall > 4) #, Categroy = pref

risk_list = floor(runif(10, min =1, max = nrow(risk)))

beer_rec = risk[risk_list,]
beer_rec

#####get drunk & crunk

user = 4 #HARDCODED
pref = "Traditional Germanic"

drunk = br %>%
  filter(cluster16 == user, Category == pref, beer_abv > 7)

drunk_list = floor(runif(10, min=1, max = nrow(drunk)))

beer_rec = drunk[drunk_list,]
if (is.na(beer_rec)){ #checks to see if anything is returned NA
  print("lol good luck getting drunk")
  drunk = br %>%
    filter(beer_abv > 7)
  
  drunk_list = floor(runif(10, min=1, max = nrow(drunk)))
  
  beer_rec = drunk[drunk_list,]
}
beer_rec

#####low abv
user = 4 #HARDCODED
pref = "Traditional Germanic"

sober = br %>%
  filter(cluster16 == user, Category == pref,  beer_abv < 4) #

sober_list = floor(runif(10, min=1, max = nrow(sober)))

beer_rec = sober[sober_list,]
if (is.na(beer_rec)){ 
  #checks to see if anything is returned NA
  print("lol good luck staying sober")
  sober = br %>%
    filter(beer_abv < 4)
  
  sober_list = floor(runif(10, min=1, max = nrow(sober)))
  
  beer_rec = sober[sober_list,]
}

beer_rec


########################
########################
########################

ui <- fluidPage(
  
  #Shiny app Title
  tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #DD4529;  color:white; }
  ")),
  #setBackgroundColor(color=c("white","#ffaa80"),gradient=c("linear"),direction="bottom"),
  title="Beer FindeR",
  
  fluidRow(titlePanel( div(column(width = 7, offset=3,tags$img(src = "Logo2.PNG", height="100%", width="100%",align="center"))))),
  
  headerPanel(''),
  
  ############ Sidebar
  sidebarPanel(
    width=3,
    setSliderColor(c("#DD4529","#DD4529","#DD4529"),c(1,2,3)),
  ),
  ########### Main Tabs 
  mainPanel(
    tabsetPanel(type = "tabs",
                #tags$style(type='text/css',"color:black"),
                tabPanel("Beer Suggestion App"),
                HTML("<h4 style='color:#DD4529'>","Choose your favorite beers below","</h4>"),
                selectInput(inputId='BeerNames', label= "Please add 3-5 of your favorite beers below", choices=sort(as.character(sort(unique(br$beer_name)))), multiple = TRUE, selectize=TRUE),
                selectInput(InputID="goal", label="What are you trying to achieve?", choices= c("Get Drunk", "Day Drink", "No Preference"), multiple=FALSE),
                tableOutput("PreferredBeerTable")
                
    ),
    
    
    tabPanel("All Data"),
    HTML("<h4 style='color:#DD4529'>","Explore all beers' data below:","</h4>"),
    
    actionButton("ExportButton", "Export table data (csv)"),
    textInput("FileName","Choose a name for export file (optional)"),
    textOutput("text1")
  )
)

################### Server

server <- function(input, output,session) {
  
  ################ Reactive data
  beersData <- reactive({
    beers<- beers[which(beers$Category %in% input$Category & beers$Style %in% input$Style & beers$IBU>=input$IBUMin[1] & beers$IBU<=input$IBUMin[2] & beers$ABV>=input$ABVMax[1] & beers$ABV<=input$ABVMax[2] & beers$Brewery %in% input$Brewery & beers$State %in% input$State),]
  })
  
  
  preferredBeers <- reactive({
    perfectbeer
  })
  
  #need to edit with reviewer clusters instead
  clusterData <-reactive({
    km<-kmeans(beersData()[,c("IBU","ABV")],centers=7,iter.max=15)
    bc<-mutate(beersData(),"Group"=km$cluster)
    bc
  })
  
  ################### Update inputs based on selections 
  
  observeEvent(input$Y,{
    if (input$Y=="Category") {
      updateSelectInput(session,"ModelType",choices=c("rf","rpart"))
    }
    
    else if (input$Y=="Style") {
      updateSelectInput(session,"ModelType",choices=c("rpart"))
    }
    
    else if (input$Y=="IBU"|input$Y=="ABV"){
      updateSelectInput(session,"ModelType",choices=c("lm","glm"))
    }})
  
  observeEvent(input$BeerName1,{
    beersinput <- beers[which(beers$Name %in% input$BeerName1),]
    perfectbeer <-cbind(mean(beersinput$ABV), mean(beersinput$IBU))
    distances=pointDistance(perfectbeer,beers[which(!(beers$Name %in% input$BeerName1)),c("ABV","IBU")], lonlat =FALSE)
    ranked=sort(distances, index.return =TRUE)
    preferred <-beers[ranked$ix[1:5],]
  })
  
  
  
  observeEvent(input$Brewery,{
    if (!is.null(beersData())) {
      updatePickerInput(session,"Category",choices=sort(unique(as.character(beersDataNoCat()$Category))),selected=as.character(beersDataNoCat()$Category[1:length(beersDataNoCat()$Category)])) }
  })
  
  observeEvent(input$Category,{
    if (!is.null(beersData())) {
      updatePickerInput(session,"Style",choices=sort(unique(as.character(beersDataNoStyle()$Style))),selected=as.character(beersDataNoStyle()$Style[1:length(beersDataNoStyle()$Style)])) 
      updateSelectInput(session,"BeerName1",choices=sort(unique(as.character(beersDataNoStyle()$Name)))) }
  })
  
  
  ################ Table Outputs 
  output$PreferredBeerTable <- renderTable ({
    beers[which(beers$Name %in% input$BeerName1),]
  })
  
  output$R2<- renderTable({
    if (input$ModelType == 'glm'){
      Out<-postResample(caretData()[9],caretData()[input$Y])
      dfOut<-data.frame("Metric"=c("RMSE","RSquared","MAE"),"Value"=Out)
    }})
  
  output$table1 <- renderDataTable({
    data.frame(beersData())
  })
  
  output$clusterFavoriteText <- renderText({
    d<-clusterData()[which(clusterData()$Name %in% input$BeerName1),"Group"]
    d<-data.frame(table(d))
    n<-as.matrix(d[order(-d$Freq),])
    m<-n[1,1]
    m
  })
  
  output$Drunk <-renderDataTable({
    head(arrange(beersData(),desc(ABV)),3)
  },options = list(lengthChange = FALSE))
  
  output$preferred <- renderDataTable({
    beersinput <- beers[which(beers$Name %in% input$BeerName1),]
    perfectbeer <-cbind(mean(beersinput$ABV), mean(beersinput$IBU))
    distances=pointDistance(perfectbeer,beersData()[,c("ABV","IBU")], lonlat =FALSE)
    ranked=sort(distances, index.return =TRUE)
    preferred <-beersData()[ranked$ix[1:5],]
    data.frame(preferred)
  },options = list(lengthChange = FALSE))
  
  output$tableRanked1 <- renderDataTable({
    head(arrange(beersData(),desc(IBU)),10)
  },options = list(lengthChange = FALSE))
  
  output$tableRanked2 <- renderDataTable({
    head(arrange(beersData(),desc(ABV)),10)
  },options = list(lengthChange = FALSE))
  
  output$ClusterTable<-renderDataTable(
    clusterData())
  
  output$CaretTable<- renderDataTable({
    data.frame(caretData())
  })
  
  
  
  beersinput <- beers[which(beers$Name %in% input$BeerName1),]
  perfectbeer <-cbind(mean(beersinput$ABV), mean(beersinput$IBU))
  distances=pointDistance(perfectbeer,beersData()[,c("ABV","IBU")], lonlat =FALSE)
  ranked=sort(distances, index.return =TRUE)
  preferred <-beersData()[ranked$ix[1:150],]
  
  t<-data.frame(table(preferred$State))
  names(t)<-c("state","values")
  t$values<- (t$values-min(t$values))*100/max(t$values)
  t$state<-trimws(as.character(t$state))
  MergedStates <- inner_join(MainStates, t, by = "state")
  ggplot()+geom_polygon( data=MergedStates, aes(x=long, y=lat, group=group, fill = values), color="white", size = 0.2) + labs(fill="Score") + scale_fill_gradient(low="#FDED56",high="#7C1502") + theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
})


output$BeerPlot <- renderDataTable({
  m<-melt(beers[which(beers$Name==input$BeerName),c("Name","IBU","ABV")], id.vars="Name")
  ggplot(m,aes(x=variable,y=value,fill=variable))+geom_col()
})

############# Buttons
observeEvent(input$ExportButton, {
  if (is.null(input$FileName)){
    write.csv(tableSelectedData(),"ExportedTable.csv")
  }
  else {
    write.csv(tableSelectedData(),paste(input$FileName,".csv"))
  }
})

}

shinyApp(ui = ui, server = server)
