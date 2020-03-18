library(tidyverse)
library(here)
library(janitor)
library(ggalt)


rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

rainfall2 <- rainfall %>%
  mutate(date = lubridate::ymd(paste(year, month, day, sep=''))) %>%
  select(-year, -month, -day)

temp2 <- temperature %>%
  mutate(city_name = str_to_title(city_name),
         site_name = str_to_title(site_name)) %>%
  spread(temp_type, temperature) %>%
  rename(min_ = min, max_ = max, date_ = date) %>%
  mutate(month_year = lubridate::floor_date(date_, unit = 'year')) %>%
  group_by(month_year, site_name) %>%
  summarize(max_ = mean(max_), 
            min_ = mean(min_))

min_year <- lubridate::year(min(temp2$month_year))
max_year <- lubridate::year(max(temp2$month_year))

theme_set(theme_minimal(base_family = "Roboto"))
gg <- ggplot(temp2, aes(x=min_, xend=max_, y = month_year)) + 
  coord_flip() + 
  ggalt::geom_dumbbell(color="#e3e2e1", 
                       colour_x = "blue", 
                       colour_xend = "red") + 
  # geom_smooth(aes(group=1), method = "lm", se = FALSE) +
  facet_wrap(~ site_name) + 
  labs(title="Max and Min Temperature in Australia",
       subtitle = paste0({min_year}," to ", {max_year}),
       x=expression("Temperature"*~degree*C), 
       y=NULL, caption="Joseph Pope") + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.subtitle = element_text(hjust=0.5, size = 10),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

plot(gg)


ggplot(temp2, aes(month_year, max_, color = site_name)) + 
  geom_smooth()

ggplot(temp2, aes(month_year, min_, color = site_name)) + 
  geom_smooth()
