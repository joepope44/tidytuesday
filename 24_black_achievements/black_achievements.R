source("./setup.R")
library(streamgraph)
library(ggtext)

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
  geom_area() + 
  ggdark::dark_theme_bw() + 
  scale_fill_brewer(palette = "Dark2")


firsts <- firsts %>% 
  mutate(
    gender = ifelse(str_detect(gender, "Female"), "F", "M"),
    gender = as.factor(gender)
  )

first_firsts <- firsts %>% 
  group_by(gender, category) %>% 
  summarise(minYear = min(year), minName = person[which.min(year)]) %>% 
  ungroup() %>% 
  mutate(
    gender = ifelse(str_detect(gender, "Female"), "F", "M"),
    gender = as.factor(gender)
  )

# Showcase the first man and woman selected
first_firsts %>% 
  ggplot(aes(x = minYear, y = fct_rev(category), color = gender)) + 
  geom_point(size = 3) +
  geom_jitter(data = firsts, aes(x = year, y = fct_rev(category)), alpha = 0.4, size = .8, shape = 23, height = .2) + 
  #  Civil War
  geom_vline(xintercept = 1865, colour = "grey50", alpha = 0.5) +
  geom_text(
    aes(x = 1865, y = "Arts & Entertainment"), 
    label = "Civil War", color = "grey50", 
    size = 3, vjust = 0, hjust = 0, nudge_x = 5, nudge_y = 1, show.legend = FALSE
  ) + 
  # Civil Rights Act of 1964
  geom_vline(xintercept = 1964, colour = "grey50", alpha = 0.5) +
  geom_text(
    aes(x = 1964, y = "Arts & Entertainment"), 
    label = "Civil Rights Act", color = "grey50", 
    size = 3, vjust = 0, hjust = 0, nudge_x = 5, nudge_y = 1, show.legend = FALSE
  ) + 
  ggdark::dark_theme_minimal() + 
  scale_x_continuous(breaks = seq(1730, 2010, by = 20),
                     labels = paste0(seq(1730, 2010, by = 20))) + 
  labs(
    title = "Notable <span style = 'color: #00b0f6;'>Male</span> and <span style = 'color: #f8766d;'>Female</span> African-Americans By Field", 
    subtitle = "Firsts in their field are highlighted", 
    x = "", 
    y = "", 
    caption = "Joseph Pope | @joepope44"
  ) + 
  theme(
    text = element_text(family = "Franklin Gothic Book"), 
    legend.title = element_blank(), 
    legend.position = "right",
    plot.subtitle = element_text(margin = margin(0,0,6,0)),
    plot.title.position = "plot",
    plot.title = element_markdown(family = "Franklin Gothic Medium", size = 16),
  ) + 
  coord_cartesian(clip = "off")


# # Stacked Area Chart - didn't work very well. Not sure why
# decades %>%   
#   arrange(decade) %>% 
#   mutate(cumsum = cumsum(n)) %>% 
#   ggplot(aes(x = decade, y = cumsum, fill = category, group = category)) + 
#   geom_area() + 
#   ggdark::dark_theme_bw() + 
#   scale_fill_brewer(palette = "Dark2")

# Streamgraph, html widget
# p <- streamgraph(
#   decades, key="category", value="n", date="decade", offset = "wiggle", interactive = FALSE, 
#   ) %>% 
#   sg_fill_brewer("Dark2") %>% 
#   sg_axis_x(20, "year", "%Y")