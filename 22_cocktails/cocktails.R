source("./setup.R")
library(widyr)
library(ggraph)
library(igraph)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load(2020, week = 22)

cocktails <- tuesdata$cocktails

boston <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


boston2 <- boston %>% 
  mutate(
    ingredient = tolower(ingredient), 
    label = case_when(
      str_detect(ingredient, "tequila|mezcal") ~ "tequila",
      str_detect(ingredient, "rum") ~ "rum", 
      str_detect(ingredient, "vodka") ~ "vodka", 
      str_detect(ingredient, "whiskey|bourbon|scotch|whisky") ~ "whiskey", 
      str_detect(ingredient, "gin") ~ "gin",
      # str_detect(ingredient, "juice") ~ "juice",
      # str_detect(ingredient, "vermouth") ~ "vermouth",
      # str_detect(ingredient, "brandy|port|sherry") ~ "sweet wine",
      # str_detect(ingredient, "syrup|sugar|schnapps|creme|agave") ~ "sugar",
      # str_detect(ingredient, "bitters") ~ "bitters",
      # str_detect(ingredient, "liqueur|triple sec|grenadine") ~ "liqueur",
      # str_detect(ingredient, "egg") ~ "egg",
      TRUE ~ "other"
    )
  )

boston2 %>% 
  widyr::pairwise_count(item = ingredient, feature = row_id) %>% 
  arrange(desc(n)) 

ingredient_pairs <- boston2 %>%
  add_count(ingredient) %>%
  filter(n >= 5) %>%
  pairwise_cor(ingredient, name, sort = TRUE)

top_cors <- ingredient_pairs %>%
  head(150)

ingredient_info <- boston2 %>%
  group_by(label, ingredient) %>% 
  summarise(n = n()) %>% 
  filter(ingredient %in% top_cors$item1) %>%
  # filter(str_detect(ingredient ,"mezcal")) %>% 
  ungroup() %>% 
  mutate(label = factor(label)) %>% 
  select(ingredient, n, label)

cols <- c(
  "gin" = "#44AF69", #greenish
  "rum" = "#DD403A", #red
  "vodka" = "#75DDDD", #blue
  "other" = "white",  
  "tequila" = "#FCAB10", #orange
  "whiskey" = "#D9B26F" #tan
)


p <- top_cors %>% 
  head(200) %>%
  graph_from_data_frame(vertices = ingredient_info) %>% 
  ggraph::ggraph(layout = "fr") +
  # geom_edge_link() +
  geom_edge_bend(edge_alpha = .3, strength = .5, colour = "white") +
  geom_node_text(aes(label = name, family = "Courier", colour = label), repel = TRUE) +
  geom_node_point(aes(colour = label, size = ifelse(label == "other", 3, 4))) + 
  guides(size = FALSE) + 
  labs(
    title = "#TidyTuesday...After Dark", 
    subtitle = "Top 100 Cocktail Correlation Pairs by Selected Alcohol Types",
    x = "", 
    y = "",
    caption = "Joseph Pope : @joepope44"
  ) + 
  scale_color_manual(values = cols)

p + theme(
  text = element_text(family = "Betty Noir", color = "white"),
  rect = element_rect(fill = "black"),
  panel.background = element_rect(fill = "black"), 
  plot.title = element_text(size = 30),
  plot.subtitle = element_text(size = 20, margin = margin(0,0,4,0)), 
  legend.title = element_blank(),
  legend.position = "bottom", 
  legend.key = element_rect(fill = "black"),
  legend.text = element_text(size = 14),
  plot.caption = element_text(size = 10),
  )

