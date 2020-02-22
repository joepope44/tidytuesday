library(tidyverse)
library(here)
library(emojifont)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

str(food_consumption)

summary(food_consumption)

food_consumption%>% group_by(food_category) %>% summarise(mean(co2_emmission))
  
food_cats <- unique(food_consumption$food_category)

animal <- food_cats[1:7]

plant_based <- food_cats[8:11]

country <- readr::read_csv(here("wk7_food_consumption", "country_lookup.csv"))

df <- food_consumption %>%
  mutate(food_category = factor(food_category)) %>% 
  group_by(country, food_category) %>%
  summarise(consumption = mean(consumption)) %>% 
  ungroup() %>% 
  left_join(country, by = c("country" = "Country")) %>% 
  group_by(Region, food_category) %>% 
  summarize(consumption = mean(consumption)) %>% 
  ungroup() %>% 
  filter(Region != 'NA' & Region != 'Arab States') %>% 
  mutate(animal_flag = ifelse(food_category %in% animal,
                              "Animal-Based", "Plant-Based"))


ggplot(df, aes(food_category, consumption, fill = factor(animal_flag))) + 
  geom_col(show.legend = FALSE, color = "black", size = 0.25) + 
  scale_fill_manual(values = c("#CC6666", "#009E73")) + 
  facet_grid(animal_flag ~ Region, drop = TRUE, scales = "free_y") + 
  coord_flip() + 
  xlab(NULL) + 
  ylab(NULL) + 
  labs(caption = "\nJoseph Pope | @joepope44") +
  ggtitle("Food Consumption Per Region", subtitle = "(kg/person/year)") + 
  scale_y_continuous(breaks = c(0,100,200)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        plot.margin = margin(10, 10, 10, 50),
        text = element_text(family = "Rockwell"),
        plot.title = element_text(size = 18,
                                  margin = margin(b=2))) 

