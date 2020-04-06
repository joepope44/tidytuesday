source("setup.R")
library(ggrepel)
library(gganimate)
library(transformr)
library(gifski)

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# deciding which dataset to use. want to try gganimate here. 
beer_states %>% 
  filter(state != "total" & year == '2016') %>% 
  ggplot(aes(factor(year), barrels, group = state)) + 
  geom_text(aes(label = state)) + 
  facet_grid(type ~ ., scales = "free_y")

brewer_size %>% 
  mutate(brewer_size = fct_reorder2(brewer_size, year, n_of_brewers)) %>% 
  filter(!brewer_size %in% c("Total", "Zero Barrels")) %>% 
  ggplot(aes(year, n_of_brewers)) + 
  geom_line(aes(color = brewer_size))

df <- brewing_materials %>% 
  filter(!str_detect(material_type, "Total"), type != "Other") %>% 
  mutate(type = case_when(
    type == "Malt and malt products" ~ "Malt", 
    type == "Rice and rice products" ~ "Rice", 
    type == "Barley and barley products" ~ "Barley", 
    type == "Wheat and wheat products" ~ "Wheat",
    type == "Sugar and syrups" ~ "Sugars",
    grepl("Hops", type) ~ "Hops",
    TRUE ~ type)) %>% 
  select(-contains("_type")) %>% 
  mutate(date = lubridate::ymd(str_c(year, month, "01", sep = "-")))

#organize data and remove Hops, which has strange behavior when combined
df2 <- df %>% 
  filter(type != "Hops") %>% 
  group_by(type, date) %>% 
  summarize(month_current = sum(month_current), 
            month_prior_year = sum(month_prior_year)) %>% 
  mutate(label = ifelse(date == max(date), 
                      type, "")) %>% 
  na.omit()

# gganimate per year? facet by type?  
p <- df2 %>% 
  ggplot(aes(date, month_current, color = type, group = type)) + 
  ggplot2::scale_color_viridis_d() + 
  geom_line(alpha = .6) + 
  geom_smooth(method = "loess", se = FALSE) +
  # geom_label_repel(aes(label = label)) + 
  facet_wrap(~ type, scales = "free_y") + 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma_format(scale = 10e-6, accuracy = 1, suffix = "M")) + 
  coord_cartesian(xlim=as.Date(c("2008-01-01","2017-01-01"))) + 
  theme_clean() + 
  theme(legend.position = "none") + 
  labs(title = "Beer Materials Plummet in 2016", subtitle = "In Numbers of Barrels", caption = "Joseph Pope | @joepope44")+ 
  xlab("") + 
  ylab("") 
  
p2 <- p + 
  transition_reveal(date) + 
  transition_reveal(stat(x))

# saving didn't work? had to open in html then save from there...
anim_save("animated_brew.gif")

#spread the good news 
rtweet::post_tweet(media = "14_beer_brewing/anim_brew.gif",
                   status = "Fun with gganimate and brewery materials!. #rstats #tidytuesday https://github.com/joepope44/tidytuesday/tree/master/14_beer_brewing")


