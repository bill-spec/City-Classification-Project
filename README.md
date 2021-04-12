# City-Classification-Project

The 'Completed Functions' folder is where the completed, one click, functions for the project are held. These are both for classification and animations. The rest of the folders are previous iterations of this process, including geocoding.

***

## Completed Functions Folder 


### Location Generation Functions.R

This file contains the functions that classify the locations using a random forest. The requirements of the dataframe to be passed through are given in the file.

### AnimationFunction.R

This file is for animating maps using the gganimate package. They can take a while to load. They also do not do much gganimate is already very intuitive. This also uses my own google API key... it is from a spam email so no worries (I think).

### Driver File.Rmd

This file tests the functions using the dallas dataset that has already been geocoded. This is a markdown file to make the viewing easier for a user. To run this file you must have already loaded the two function files above (i.e. upon startup run them once so that the functions are loaded in your Rstudio environment)


***

## Everything Else

Each city is self contained in its own folder and follows the same workflow (geocode, clean, train forest, test forest, return data and wrtie to desktop)

Each one of the above steps is contained in its own .Rmd file and the names are the same as the names above.






