


#This make use of gganimate. https://gganimate.com/articles/gganimate.html

#set the of the city that you are plotting before running any functions.
#Either do it in this file or the file you are generating in


map <- get_map(location = 'Dallas', zoom = 10, maptype = "terrain-background", source = 'google', color = 'color')

anim = callRender(dallasData)

animate(anim, renderer = magick_renderer(), duration = 50, fps = 10)

#write to desktop with the following call

anima_save('filename', animation = last_animation())

#the filename needs to be specified and the animation will use the last animation rendered. Link https://gganimate.com/reference/anim_save.html



#installs and/or loads packages for the animations and also calls the google API to use the ggmap package

loadPackagesAnimations <- function(){
  
  # Package names
  packages <- c("tidyverse","gganimate","ggmap","tidygeocoder","png","gifski","magick")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Google API Key
  k <- "AIzaSyDNMoGGfgl9f5KtGCnp9BegNb7ZDqPu7gg"
  register_google(key = k)
  
  
}


#gets the top areas between the given years
getTop <- function(dataframe, minYear = 0, maxYear = 2100, countOfLocations = 20){
  
  topData <- dataframe %>% group_by(location) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(location != "")
  topData <- topData[1:countOfLocations,]$location
  
  #filter for locations in the top 20
  dataframe = dataframe %>% filter(location %in% topData)
  
  dataframe = dataframe %>% filter(Year >= minYear)
  datafream = dataframe %>% filter(Year <= maxYear)
  
  return(dataframe)
}

#This returns an animation based on the min/max year and the count of locations in the top period 
callRender <- function(dataframe, minYear = 0, maxYear = 2100, countOfLocations = 20){
  
  dataFrameTop = getTop(dataframe, minYear, maxYear, countOfLocations)
  anim = ggmap(map) + geom_point(data = dataFrameTop, mapping = aes(x = longitude, y = latitude, color = factor(location)), size = 0.1) + theme(legend.title = element_blank())+
    labs(title = "Year {frame_time}")+
    transition_time(Year)+
    enter_fade()+
    exit_fade() 
  
  return(anim)
}






