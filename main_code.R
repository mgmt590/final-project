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

#splits train/test data. Train data is all beers with an ABV. Test data is missing ABV
train <- abvdata[which(!is.na(abvdata["beer_abv"])),]
test <- abvdata[which(is.na(abvdata["beer_abv"])),]

#trains model with linear regression. This can take up to 5 minutes to run
ctrl <- trainControl(method="cv",number=5,classProbs = F, summaryFunction = defaultSummary, allowParallel=T)
m1<- train(beer_abv ~ . - beer_beerid, data = train, method = "lm", trControl = ctrl,na.action = na.pass)

#generates predictions and fills in missing ABV in primary dataframe, with predictions from the model. Uses Beer ID to match the two datasets
preds <- data.frame(predict(m1, newdata=test,na.action=na.pass))
preds<- mutate(preds,"beer_beerid"=as.numeric(rownames(preds)))
review_data[which(is.na(review_data$beer_abv)),"beer_abv"]<-ifelse(as.integer(preds$predict.m1..newdata...test.)>0,as.integer(preds$predict.m1..newdata...test.),review_data$beer_abv)


rm(ctrl, m1, preds, test, train)

#shows how many missing values are in the beer_abv column after prediction model
sum(is.na(review_data$beer_abv))


#removes beer_ID from the dataset, since it is not needed after the prediction modek, and writes the file into a CSV
review_data <-dplyr::select(review_data,c(-"beer_beerid"))
write.table(review_data,"beersComplete.csv",sep=",")


