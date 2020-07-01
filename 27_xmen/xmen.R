source("./setup.R")
options(scipen = 999)

tuesdata <- tidytuesdayR::tt_load(2020, week = 27)

char <- tuesdata$characters

# replace NA with 0 
# 
# char[is.na(char)] <- 0

char2 <- char %>% 
  select(character, rendered_unconcious:number_of_kills_non_humans ) %>% 
  group_by(character) %>% 
  summarize_all(.funs = sum) %>% 
  ungroup() %>% 
  mutate(character = str_extract(character, "[^=]+"),
         character = fct_reorder(character, number_of_kills_non_humans))

char2 %>% 
  filter(number_of_kills_non_humans > 0) %>% 
  ggplot(aes(number_of_kills_non_humans, character, fill = character)) + 
  geom_col(show.legend = FALSE) + 
  scale_x_log10(label = scales::comma) + 
  scale_fill_viridis_d(option = "inferno", direction = -1, begin = .1, end = 1) + 
  labs(
    title = "Jean Grey as Dark Phoenix, Destoyer of Worlds", 
    subtitle = "X-Men With More Than One Non-human Kill Shown", 
    caption = "Joseph Pope | @joepope44",
    x = "",
    y = ""
  ) + 
  ggthemes::theme_clean() + 
  theme(
    text = element_text(family = "Comic Sans MS"),
    plot.title = element_text(face = "bold"),
    rect = element_rect(fill = "#ADA258")
  )
  

