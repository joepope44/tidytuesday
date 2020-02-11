library(tidyverse)
library(leaflet)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

sf_trees %>%
  summarize_all(n_distinct)

glimpse(sf_trees)


# leaflet 
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-122.4341, lat=37.7987, popup="The birthplace of R")
m 

