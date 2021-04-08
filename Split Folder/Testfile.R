
library(tidyverse)

load(file = "dallasGeocodedNotFound.RData")
geocodedDallasNotFound = geocodedDallasNotFound %>% select(-c(X,X.1))
geocodedDallasNotFound


load(file = "dallasGeocoded2.RData")
dallasGeocoded

data = plyr::rbind.fill(dallasGeocoded,geocodedDallasNotFound)
data





dataframe = data
loadPackages()
splitDataList = splitData(dataframe)

dataWithLocations = splitDataList[[1]]
dataWithoutLocations = splitDataList[[2]]

dataWithLocations = cleanData(dataWithLocations)

forestList = callForest(dataWithLocations,locationCount)

dataWithLocationsCalculated = forestList[[1]]

returnData = mergeData(dataWithLocationsCalculated,dataWithoutLocations)

cleanedData = callSingleForest(data,25)
cleanedData


cleanedData = callTripleForest(data,25)
cleanedData

cleanedData = callSingleandTriple(data,25)
cleanedData




load(file = "cleanBoston.RData")
bostonData

load(file = "bostonTop50.RData")
bostonAddressTop50


data = bostonData
data

str(data)
data$location = data$Area

cleanedData = callSingleForest(data,25)
cleanedData

cleanedData = callTripleForest(data,25)
cleanedData

cleanedData = callSingleandTriple(data,25)
cleanedData


