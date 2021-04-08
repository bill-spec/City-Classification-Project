


#set the of the city that you are plotting before running any functions



#



map <- get_map(location = 'Dallas', zoom = 10, maptype = "terrain-background", source = 'google', color = 'color')






#call b










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



