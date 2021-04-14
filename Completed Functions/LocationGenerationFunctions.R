

###########################################################################################
# Data Requirements #
###########################################################################################

#location to be predicted on needs to be'location'
#column name for housing type needs to be 'Type'
#column name for  year needs to be 'Year'
#column name for the price type needs to be 'Price.type'
#column name for the number of bedrooms needs to be 'Bedrooms'
#column name for total rooms needs to be 'Total.rooms'
#column name for pay frequency needs to be 'Frequency'
#column name for sale price needs to be 'Sale.price'
#column name for rent price needs to be 'Rent.price'


###########################################################################################
# Notes #
###########################################################################################
#These functions handle non-geocoded rows, the entire dataset should be passed through.

#The order of the rows is maintained when the data is returned.

#Functions that call the random forest return multiple objects within their lists, 
#the aggregated functions below only return the dataframe. Check the documentation of 
#the random forest calls to see how to return that data.



#Calls a single forest and returns the organized dataframe
callSingleForest <- function(dataframe, locationCount){
  
  
  loadPackages()
  splitDataList = splitData(dataframe)
  
  dataWithLocations = splitDataList[[1]]
  dataWithoutLocations = splitDataList[[2]]
  
  dataWithLocations = cleanData(dataWithLocations)
  
  forestList = callForest(dataWithLocations,locationCount)
  
  dataWithLocationsCalculated = forestList[[1]]
  
  returnData = mergeData(dataWithLocationsCalculated,dataWithoutLocations)
  return(returnData)
}


#Calls a three forests and returns the organized dataframe
callTripleForest <- function(dataframe, locationCount){
  
  loadPackages()
  splitDataList = splitData(dataframe)
  
  dataWithLocations = splitDataList[[1]]
  dataWithoutLocations = splitDataList[[2]]
  
  dataWithLocations = cleanData(dataWithLocations)
  
  forestList = callAllForests(dataWithLocations,locationCount)
  dataWithLocationsCalculated = forestList[[1]]
  
  returnData = mergeData(dataWithLocationsCalculated,dataWithoutLocations)
  return(returnData)
}


#Calls all forests  and returns the organized dataframe
callSingleandTriple <- function(dataframe, locationCount){
  loadPackages()
  splitDataList = splitData(dataframe)
  
  dataWithLocations = splitDataList[[1]]
  dataWithoutLocations = splitDataList[[2]]
  
  dataWithLocations = cleanData(dataWithLocations)
  
  forestList = callAllForests(dataWithLocations,locationCount)
  dataWithLocationsCalculated = forestList[[1]]
  
  forestList = callForest(dataWithLocationsCalculated,locationCount)
  dataWithLocationsCalculated = forestList[[1]]
  
  returnData = mergeData(dataWithLocationsCalculated,dataWithoutLocations)
  return(returnData)
}


callNPeriodForest <- function(dataframe, locationCount = 20, splitBy){
  loadPackages()
  splitDataList = splitData(dataframe)
  
  dataWithLocations = splitDataList[[1]]
  dataWithoutLocations = splitDataList[[2]]
  
  dataWithLocations = cleanData(dataWithLocations)
  
  forestSplitData = forestSplit(dataWithLocations, locationCount, splitBy) 
  
  dataWithLocationsCalculated = forestSplitData
  
  returnData = mergeData(dataWithLocationsCalculated,dataWithoutLocations)
  return(returnData)
  
}







###########################################################################################
# Helper Functions for the above functions #
###########################################################################################




#calls and installs the packages used in these functions.
#if already installed, the install call is ignored.

loadPackages <- function(){
  
  # Package names
  packages <- c("tidyverse","randomForest")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  load(file = "Capitalization.RData")
}



#Creates an index column to maintain order upon completion. 

#Divides the dataframe in two conditional on if we have geocoding information or not

splitData <- function(dataframe){
  
  #create an index to restore later
  dataframe$index = c(1:nrow(dataframe))  
  
  #first return is with location, second return is without
  data = list()  
  data[[1]] = dataframe %>% filter(!is.na(latitude))%>% filter(!is.na(longitude))  
  data[[2]] = dataframe %>% filter(is.na(latitude))%>% filter(is.na(longitude))
  return(data)
}


#Merge the two dataframes of geocoded and nongeocoded back together
#New columns from the algorithm are filled with NAs in the 
#nongeocoded rows.

#Returned in order of the index assignment from the start.

mergeData <- function(dataframe1, dataframe2){
  
  #create an index to restore later
  
  data = plyr::rbind.fill(dataframe1, dataframe2)
  data %>% arrange(index)  
  
  return(data)
}



#Clean data method that creates the capitalized price
#and estimates the total number of rooms per home via simple regression.

#Returns cleaned data for the main functions.

cleanData <- function(dataFrame){
  
  #Capitalize the price into one column
  dataFrame <- capitalizeRent(dataFrame)
  dataFrame$capitalizedPrice[is.na(dataFrame$capitalizedPrice)] = median(dataFrame$capitalizedPrice[!is.na(dataFrame$capitalizedPrice)])
  
  
  #clean some columns (ensure factors and lowercase inputs)
  dataFrame$location <- as.factor(dataFrame$location)
  
  dataFrame$Price.type <- tolower(dataFrame$Price.type)
  dataFrame$Price.type <- gsub('income', 'rent', dataFrame$Price.type)
  dataFrame$Price.type <- as.factor(dataFrame$Price.type)
  
  dataFrame$Type <- tolower(dataFrame$Type)
  dataFrame$Type <- as.factor(dataFrame$Type)
  
  dataFrame$Column <- tolower(dataFrame$Column)
  
  
  
  #calculate the number of rooms via regression 
  data = dataFrame %>% filter(Bedrooms != "" & Total.rooms != "") #obs with both for model
  
  lmmodel <- lm(Total.rooms ~ Bedrooms, data)
  
  coeff = lmmodel$coefficients
  names(coeff) <- NULL
  multiple = coeff[2]
  
  #add the estimated rows
  estimation = data.frame(dataFrame$Total.rooms, dataFrame$Bedrooms, Total.rooms.estimation = NA) %>% mutate(Total.rooms.estimation = ifelse(!is.na(dataFrame.Total.rooms), dataFrame.Total.rooms,round(dataFrame.Bedrooms*multiple))) 
  
  #fill the rest in with the median
  medianOfTotal = median(estimation$Total.rooms.estimation[which(!is.na(estimation$Total.rooms.estimation))])
  
  estimation = estimation %>% mutate(Total.rooms.estimation = ifelse(is.na(Total.rooms.estimation), medianOfTotal,as.double(Total.rooms.estimation))) 
  
  dataFrame = cbind(dataFrame, "Total.rooms.estimation"= estimation[,3])
  
  
  dataFrame$location = as.character(dataFrame$location)
  
  return(dataFrame)

}


capitalizeRent <- function(dataFrame){
  
  dataFrame$Frequency <- as.character(dataFrame$Frequency)
  dataFrame$numericFrequency <- ifelse(grepl(pattern = "m",dataFrame$Frequency), 12, 
                                       ifelse(grepl(pattern = "year",dataFrame$Frequency), 1,
                                              ifelse(grepl(pattern = "ann",dataFrame$Frequency), 1,
                                                     ifelse(grepl(pattern = "week",dataFrame$Frequency),52, 12))))
  
  load(file = "Capitalization.RData")
  dataFrame <- dataFrame %>% left_join(capitalizationRate, by = c("Year" = "year"))
  
  dataFrame$Sale.price <- as.numeric(gsub(",","",dataFrame$Sale.price))
  dataFrame$Rent.price <- as.numeric(gsub(",","",dataFrame$Rent.price))
  
  dataFrame$capitalizedPrice <- ifelse( (!is.na(dataFrame$Sale.price) & (dataFrame$Sale.price != "") ), 
                                        dataFrame$Sale.price, 
                                        dataFrame$numericFrequency*dataFrame$cap*dataFrame$Rent.price)
  dataFrame$capitalizedPrice <- round(dataFrame$capitalizedPrice,2)
  return(dataFrame)
}




#Calls a sinlge random forest for the entire period. 

#The nunber of top locations can be modified by is default 20.

#Returns a list containing:

#[[1]] dataset with the calculatedColumn prediction
#[[2]] a vector of the top locations being considered 
#[[3]] the randon forest object that was built
#[[4]] estimated misclassification rate 
#[[5]] only the preidcted rows

callForest <- function(dataframe, numberOfLocations = 20){
  
  dataframe$index <- c(1:nrow(dataframe)) #create an index to restore later
  
  
  filteredData = dataframe
  
  topNLocations <- filteredData %>% group_by(location) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(location != "")
  topNLocations <- topNLocations[1:numberOfLocations,]$location
  
  filteredData = filteredData %>% filter(location %in% topNLocations) #filter for locations in the top N
  
  filteredData$location = as.character(filteredData$location) #Swap from character to factor to get the correct number of 
  filteredData$location = as.factor(filteredData$location)    #factors, can cause a model crash.
  
  
  
  ##Call the Classifier##  
  
  modelData <- filteredData    #take a random sample to test the model
  set.seed(343)
  shuf <- sample(nrow(modelData))
  modelData <- modelData[shuf,]
  train <- sample(nrow(modelData)*.8)
  modelTrain <- modelData[train,]
  modelTest <- modelData[-train,]
  
  
  bag <- randomForest(location ~ latitude + longitude + Year + Total.rooms.estimation + capitalizedPrice, data = modelTrain, ntree = 1000, importance = TRUE)           #build the model to test with a misclass rate
  yhat.rf <- predict(bag, newdata = modelTest)
  misclassRate <- mean(yhat.rf != modelTest$location)
  
  #Train the full model
  model = randomForest(location ~ latitude + longitude + Year +Total.rooms.estimation + capitalizedPrice, data = modelData, ntree = 1000, mtry = 2, importance = TRUE)
  
  
  toPredict = dataframe %>% filter(!(location %in% topNLocations))  #Filter the data that needs to be predicted 
  
  
  yhat.rf <- predict(model, newdata = toPredict)    #Predict using the full model and add the new column
  predicted <- cbind(toPredict,yhat.rf)
  
  predicted$calculatedColumn = predicted$yhat.rf
  predicted = predicted %>% select(-yhat.rf)
  
  dataInTopNLocations = dataframe %>% filter(location %in% topNLocations)
  #dataInTopNLocations$calculatedColumn = "Given"  
  dataInTopNLocations$calculatedColumn = dataInTopNLocations$location
  
  fullDataSet = rbind(predicted, dataInTopNLocations)
  
  fullDataSet = fullDataSet %>% arrange(index)
  
  
  returnList = list()
  returnList[[1]] = fullDataSet
  returnList[[2]] = topNLocations
  returnList[[3]] = model
  returnList[[4]] = misclassRate
  returnList[[5]] = predicted
  
  return(returnList)
  
}




#Calls a three random forests for the each period. 0<1940, 1940<1970, <1970<2100

#The nunber of top locations can be modified by is default 20.

#Returns a list containing:

#[[1]] dataset with the calculatedColumni prediction (i = 1,2,3)
#[[2]] a list of the vectors of the top locations being considered in each period 
#[[3]] a list of the randon forest objects that were built in each period
#[[4]] a list of the estimated misclassification rates in period 
#[[5]] a list of only the preidcted rows in each period

callAllForests <- function(dataframe, numberOfLocations = 20){
  
  #split by periods
  dataframe$period1 <- ifelse(dataframe$Year <= 1940, 1,0)
  dataframe$period2 <- ifelse(dataframe$Year >= 1940 & dataframe$Year <= 1970, 1,0)
  dataframe$period3 <- ifelse(dataframe$Year >= 1970, 1,0)
  
  dataframe$calculatedColumnPeriod1 <- 'a'
  dataframe$calculatedColumnPeriod2 <- 'b'
  dataframe$calculatedColumnPeriod3 <- 'c'
  
  dataframe$calculatedColumnPeriod1 <- as.character(dataframe$calculatedColumnPeriod1)
  dataframe$calculatedColumnPeriod2 <- as.character(dataframe$calculatedColumnPeriod2)
  dataframe$calculatedColumnPeriod3 <- as.character(dataframe$calculatedColumnPeriod3)   
  
  dataFrameList = list()
  filteredData1 = dataframe %>% filter(period1 == 1)
  filteredData2 = dataframe %>% filter(period2 == 1)
  filteredData3 = dataframe %>% filter(period3 == 1)
  
  dataFrameList[[1]] = filteredData1
  dataFrameList[[2]] = filteredData2
  dataFrameList[[3]] = filteredData3
  
  topLocationsList = list()
  modelList = list()
  misclassList = list()
  predictedList = list()
  
  for(i in c(1:3)){  
    #get top locations from period i
    
    filteredData = dataFrameList[[i]]
    
    topData <- filteredData %>% group_by(location) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(location != "")
    topData <- topData[1:20,]$location
    topLocationsList[[i]] = topData
    
    #filter for locations in the top 20
    filteredData = filteredData %>% filter(location %in% topLocationsList[[i]])
    
    filteredData$location = as.character(filteredData$location)
    filteredData$location = as.factor(filteredData$location)
    
    #call classifier
    
    #take a random sample to test the model
    modelData <- filteredData
    set.seed(343)
    shuf <- sample(nrow(modelData))
    modelData <- modelData[shuf,]
    train <- sample(nrow(modelData)*.8)
    modelTrain <- modelData[train,]
    modelTest <- modelData[-train,]
    
    #build the model to test with a misclass rate
    bag <- randomForest(location ~ latitude + longitude + Year +Total.rooms.estimation, data = modelTrain, ntree = 1000, importance = TRUE)
    yhat.rf <- predict(bag, newdata = modelTest)
    misclassRate <- mean(yhat.rf != modelTest$location)
    misclassList[[i]] = misclassRate
    
    #Train the full model
    modelList[[i]] = randomForest(location ~ latitude + longitude + Year +Total.rooms.estimation , data = modelData, ntree = 1000, mtry = 2, importance = TRUE)
    
    #Filter the data that needs to have a predicion
    toPredict = dataFrameList[[i]] %>% filter(!(location %in% topLocationsList[[i]]))
    
    #Predict using the full model and add the new column
    yhat.rf <- predict(modelList[[i]], newdata = toPredict)
    predicted <- cbind(toPredict,yhat.rf)
    
    
    if(i==1){
      predicted$calculatedColumnPeriod1 = predicted$yhat.rf
      predicted = predicted %>% select(-yhat.rf)
      predicted1 = predicted
      predictedList[[i]] = predicted
      
      dataInTopArea = dataFrameList[[i]] %>% filter(location %in% topLocationsList[[i]])
      dataInTopArea$calculatedColumnPeriod1 = "Given"  
      
      firstPeriod = rbind(predicted, dataInTopArea)
      fullDataSet = firstPeriod
      
      
    }else if(i==2){
      predicted$calculatedColumnPeriod2 = predicted$yhat.rf
      predicted = predicted %>% select(-yhat.rf)
      predicted1 = predicted
      predictedList[[i]] = predicted
      
      dataInTopArea = dataFrameList[[i]] %>% filter(location %in% topLocationsList[[i]])
      dataInTopArea$calculatedColumnPeriod2 = "Given"  
      
      secondPeriod = rbind(predicted, dataInTopArea)
      fullDataSet = rbind(fullDataSet,secondPeriod)
      
    }else if(i==3){
      predicted$calculatedColumnPeriod3 = predicted$yhat.rf
      predicted = predicted %>% select(-yhat.rf)
      predicted1 = predicted
      predictedList[[i]] = predicted
      
      dataInTopArea = dataFrameList[[i]] %>% filter(location %in% topLocationsList[[i]])
      dataInTopArea$calculatedColumnPeriod3 = "Given"  
      
      thirdPeriod = rbind(predicted, dataInTopArea)
      fullDataSet = rbind(fullDataSet, thirdPeriod)    
      
    }else{
      
    }
    
  }  
  
  fullDataSet = fullDataSet %>% arrange(index)
  
  #correct duplicate rows from the <= >= on the border years
  fullDataSet = correctDups(fullDataSet)
  
  returnList = list()
  returnList[[1]] = fullDataSet
  returnList[[2]] = topLocationsList
  returnList[[3]] = modelList
  returnList[[4]] = misclassList
  returnList[[5]] = predictedList
  
  return(returnList)
}




#If no duplicates exist this may crashe

#Helper function that corrects for the overlap of the the periods
#from the function call above

correctDups <- function(dataframe){
  
  n_occur <- data.frame(table(dataframe$index))
  
  repeats = dataframe[dataframe$index %in% n_occur$Var1[n_occur$Freq > 1],]
  
  shift <- function(x,n){
    c(x[-(seq(n))], rep(NA,n))
  }
  
  firstJoin = repeats %>% filter(period1 == 1 & period2 == 1)
  
  firstJoin$calculatedColumnPeriod2 = shift(firstJoin$calculatedColumnPeriod2, 1)
  
  firstJoin = firstJoin %>% filter(row_number() %% 2 == 1)
  
  secondJoin = repeats %>% filter(period2 == 1 & period3 == 1)
  
  secondJoin$calculatedColumnPeriod3 = shift(secondJoin$calculatedColumnPeriod3, 1)
  secondJoin = secondJoin %>% filter(row_number() %% 2 == 1)
  
  correctDups = rbind(firstJoin, secondJoin)
  
  toFilter = correctDups$index
  
  dataframe = dataframe %>% filter(!(index %in% toFilter))
  
  dataframe = rbind(dataframe, correctDups)
  
  dataframe = dataframe %>% arrange(index)
  
  return(dataframe)
}



#Return a dataset with n + 1 calculated columns based on n based on splitBy and m locations based on locationCount
forestSplit <- function(dataframe, locationCount, splitBy){
  
  tempData = dataframe #temp frame so that we can manipulate willy nilly
  
  yearMin = max(dataframe$Year)
  yearMax = min(dataframe$Year)
  
  periodLength = yearMax - yearMin
  periodIncremenet = ceiling(periodLength/splitBy)
  
  yearSplitVector = c()
  cur = yearMin
  
  for(i in c(1:(splitBy))){
    
    cur = cur + yearIncrement 
    
    yearSplitVector[i] = cur
    
    
  }
  
  

  for(j in c(1:(splitBy))){
    
    dataInSet = tempData %>% filter(Year <= yearSplitVector[j])           #filter both sides
    dataOutOfSet = tempData %>% filter(Year > yearSplitVector[j]) 
    
    forestCall = callForest(dataInSet, locationCount)     #call a forest and return based on the data in the set 
    newData = forestCall[1]
    
    
    columnToAdd = newData$calculatedColumn          #add the column to the data and rename it periodBelow i (the year curoff)
    dataOutOfSet[ , ncol(dataOutOfSet) + 1] <- columnToAdd                  
    colnames(dataOutOfSet)[ncol(dataOutOfSet)] <- paste0("periodBelow", j)    
    
    
    tempData = plyr::rbind.fill(dataOutOfSet,dataInSet)           #add the temp data
    
    
  }
  
  
  return(tempData)
  

  
}
