

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
