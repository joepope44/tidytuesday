source("./setup.R")
options(scipen = 999)
library(geofacet)

tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types

country_totals <- tuesdata$country_totals


solar <- energy_types %>% 
  filter(type == "Solar") %>% 
  mutate(yoy_2017 = round((`2017` / `2016` - 1) * 100, 2),
         yoy_2018 = round((`2018` / `2017` - 1) * 100, 2),
         total = `2016` + `2017` + `2018`) %>% 
  filter(total > 10 & `2018` > 0) %>% 
  arrange(desc(total)) 

         
solar_rates <- solar %>% 
  select(country, country_name, yoy_2017, yoy_2018) %>% 
  mutate(country_name = ifelse(country == "UK", "United Kingdom", country_name),
         `2017` = yoy_2017, `2018` = yoy_2018) %>% 
  select(-yoy_2017, -yoy_2018) %>% 
  pivot_longer(names_to = "Year", values_to = "YoY Rate", cols = c(`2017`, `2018`))

solar_rates2 <- energy_types %>% 
  filter(type == "Solar" & `2018` > 0) %>% 
  select(-type, -level) %>% 
  mutate(country_name = ifelse(country == "UK", "United Kingdom", country_name)) %>% 
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "solar_power")

ggplot(solar_rates2, aes(year, solar_power)) + 
  geom_col() + 
  facet_geo(~ country_name, grid = "europe_countries_grid1")
 
