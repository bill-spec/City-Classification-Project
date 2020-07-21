library(tidyverse)
library(tidygeocoder)
library(randomForest)
library(ggmap)

#Reading in file

boston <- read.csv("C:/Users/Billy/Desktop/hipHop/hiphop_boston.csv")
boston

#Cleaning Address

boston <- boston %>% filter(Area != "") %>% filter(Street != "" | Cross.Street != "") %>% filter(Hse.No != "")

#Finding the top 50 geographic areas

boston$Area <- tolower(boston$Area)
tbl <- boston %>% group_by(Area) %>% summarise(count = n()) %>% arrange(desc(count))
listOfAreas <- tbl[1:50,]$Area
listOfAreas

boston <- boston %>% filter(Area %in% listOfAreas) 
bostonAddress <- boston %>% unite(Address, c("Hse.No","Street","Area"), sep = " ",remove = FALSE)
bostonAddress$Address <- paste(bostonAddress$Address, " , MA")
bostonAddress

#Geocoding 

geocodeFileFull <- tidygeocoder::geocode(.tbl = bostonAddress, address = Address, method = "osm", lat = latitude, long = longitude)
geocodeFile <- geocodeFileFull
write.csv(geocodeFile, file = "C:/Users/Billy/Desktop/Storage/OSMgeocodedCSV.csv", row.names = FALSE)
geocodeFile <- read.csv("C:/Users/Billy/Desktop/Storage/OSMgeocodedCSV.csv") 

#Cleaning and Filtering for outliers

geocodeFile <- geocodeFile %>% filter(!is.na(latitude))
geocodeFile$Area <- as.factor(geocodeFile$Area)
geocodeFile <- geocodeFile %>% filter(latitude < 43) %>% filter(latitude > 42) %>% filter(longitude > -71.75)
geocodeFile

#Cleaning data for modeling

bostonArea <- geocodeFile

bostonArea$Price.type <- tolower(bostonArea$Price.type)
bostonArea$Price.type <- as.factor(bostonArea$Price.type)

bostonArea$Type <- tolower(bostonArea$Type)

bostonArea$Column <- tolower(bostonArea$Column)
bostonArea$Type <- as.factor(bostonArea$Type)
bostonArea$Total.rooms <- as.integer(bostonArea$Total.rooms)

geocodeFile <- bostonArea

#5 is the mean of totalrooms and is used to replace all NA total rooms 

geocodeFile <- geocodeFile %>% mutate(Total.rooms = replace_na(Total.rooms,5))
geocodeFile
str(geocodeFile)

#Model attempt
#Splitting data

bostonArea <- geocodeFile 
set.seed(343)

shuf <- sample(nrow(bostonArea))
bostonArea <- bostonArea[shuf,]
train <- sample(nrow(bostonArea)*.8)
bostonTrain <- bostonArea[train,]
bostonTest <- bostonArea[-train,]

length(unique(bostonArea$Area))
length(unique(bostonTrain$Area))
length(unique(bostonTest$Area))

#Buidling the model

bag <- randomForest(Area ~ latitude + longitude + Year + Total.rooms + Price.type, data = bostonTrain, ntree = 1000, mtry = 2, importance = TRUE)
yhat.rf <- predict(bag, newdata = bostonTest)
misclassRate <- mean(yhat.rf != bostonTest$Area)
misclassRate


#Confusion Matrix

#code not here

#Cross Validation 


shuf <- sample(nrow(bostonArea))
bostonArea <- bostonArea[shuf,]
ind1 <- c(1, 1311, 2621, 3931, 5241, 6551, 7861, 9171, 10481, 11791)
ind2 <- c(1310, 2620, 3930, 5240, 6550, 7860, 9170, 10480, 11790, 13100)
rates <- c(1:10)
for(i in 1:10){
  tempTrain <- bostonArea[-(ind1[i]:ind2[i]),]
  tempTest <- bostonArea[ind1[i]:ind2[i],]
  
  bag <- randomForest(Area ~ latitude + longitude + Year + Total.rooms, data = tempTrain, ntree = 1000, mtry = 2, importance = TRUE)
  yhat.rf <- predict(bag, newdata = tempTest)
  rates[i] <- mean(yhat.rf != tempTest$Area)
}
mean(rates)
rates


#Mapping

k <- "AIzaSyDNMoGGfgl9f5KtGCnp9BegNb7ZDqPu7gg"
register_google(key = k)

map <- get_map(location = 'Boston', zoom = 10, maptype = "terrain-background", source = 'google', color = 'color')
ggmap(map) + geom_point(data = bostonArea, mapping = aes(x = longitude, y = latitude, color = factor(Area)), size = 0.001)







