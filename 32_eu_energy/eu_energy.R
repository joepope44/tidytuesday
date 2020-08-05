source("./setup.R")
options(scipen = 999)
library(geofacet)

# load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types

country_totals <- tuesdata$country_totals

# find the total energy generated per conutry, per year 
energy_totals <- energy_types %>%
  filter(level == "Level 1") %>% 
  select(-level) %>%
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "total_power") %>%
  group_by(country, year) %>%
  summarise(total = sum(total_power)) %>%
  ungroup()

# clean up text and join energy totals so we can find % of solar power
solar_rates <- energy_types %>%
  filter(type == "Solar") %>%
  select(-type, -level) %>%
  mutate(
    country_name = case_when(country_name == "North Macedonia" ~ "N. Macedonia",
                       country_name == "Bosnia & Herzegovina" ~ "Bosnia & H.",
                       country == "UK" ~ "UK",
                       TRUE ~ country_name)
    ) %>%
  pivot_longer(cols = c(`2016`, `2017`, `2018`), names_to = "year", values_to = "solar_power") %>%
  left_join(energy_totals, by = c("country" = "country", "year" = "year")) %>%
  mutate(solar_rate = round(solar_power / total * 100, 2))

# import image of solar power in EU 
img <- png::readPNG("32_eu_energy/solar_eu.png")

#create ggplot object 
p <- ggplot(solar_rates, aes(year, solar_rate, fill = solar_rate)) +
  geom_col() +
  facet_geo(~country_name, grid = europe_countries_grid2 %>% mutate(name = str_trunc(name, 11))) +
  theme_solarized() +
  scale_x_discrete(labels = c('16','17','18')) + 
  scale_y_continuous(breaks = c(0,8,16)) +
  scale_fill_viridis_c(option = "plasma") + 
  labs(
    title = "Solar as % of Total Power Generated in the EU (2016-2018)",
    subtitle = "A Surprising Lack of Solar Power in Southern Europe\n",
    caption = "Joseph Pope | @joepope44",
    x = "",
    y = ""
  ) 
  
# modify theme 
p <- p + 
  theme(
    text = element_text(family = "Optima"),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(size = rel(.9)),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
    )
p

# failed attempt to include image and plot 
# cowplot::ggdraw() + 
#   cowplot::draw_plot(p) +
#   cowplot::draw_image(img)



