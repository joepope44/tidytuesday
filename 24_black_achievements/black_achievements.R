source("./setup.R")
library(streamgraph)

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

# Group by decade and category 
decades <- firsts %>% 
  mutate(decade = year - year %% 10) %>% 
  group_by(decade, category) %>% 
  count(sort = TRUE) %>% 
  ungroup() 

# Stacked Bar Chart
decades %>% 
  ggplot(aes(fill = factor(category), x = decade, y = n)) + 
  geom_col()
  
p <- streamgraph(
  decades, key="category", value="n", date="decade"
  ) 

# TO DO 
# use african color palette. All black background? 
# Annotate to show key dates. 
# July 4th 1776. Civil War. WWI. WWII. MLK and Civil Rights. 

