source("./setup.R")
options(scipen = 999)

tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types

country_totals <- tuesdata$country_totals


solar <- energy_types %>% 
  filter(type == "Solar") %>% 
  mutate(yoy_2017 = (`2017` / `2016` - 1) * 100, 
         yoy_2018 = (`2018` / `2017` - 1) * 100,
         total = `2016` + `2017` + `2018`) %>% 
  arrange(desc(total))

         