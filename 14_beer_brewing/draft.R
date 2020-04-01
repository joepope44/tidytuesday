source("setup.R")
library(ggrepel)

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

df2 <- df %>% 
  group_by(type, date) %>% 
  summarize(month_current = sum(month_current), 
            month_prior_year = sum(month_prior_year)) %>% 
  mutate(label = ifelse(date == max(date), 
                      type, ""))

# gganimate per year? facet by type?  
df2 %>% 
  ggplot(aes(date, month_current, color = type)) + 
  geom_line() + 
  geom_label_repel(aes(label = label))


