source('setup.R')
library(ggalt)

tuesdata <- tidytuesdayR::tt_load(2020, week = 23)

marbles <- tuesdata$marbles

data <- marbles %>% 
  mutate(race_type = ifelse(grepl("Q", race), "qualifier", "race")) %>% 
  group_by(race_type, site) %>% 
  summarise(avg_time_lap = 
              mean(avg_time_lap, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(avg_time_lap)) 

marbles %>% 
  mutate(race_type = ifelse(grepl("Q", race), "qualifier", "race")) %>%
  group_by(race, race_type, site, team_name) %>% 
  summarise(avg_time_lap = mean(avg_time_lap)) %>% 
  ungroup() %>% 
  ggplot(aes(avg_time_lap, site)) +
  geom_point(aes(color = race_type)) + 
  # coord_flip() + 
  facet_grid(~ team_name)
  # geom_violin(aes(fill = race_type))

# Check for home field advantage
# marbles %>% 
#   mutate(race_type = ifelse(grepl("Q", race), "qualifier", "race")) %>%
#   filter(race_type == "race") %>% 
#   ggplot(aes(avg_time_lap, site)) +
#   geom_point(aes(color = host, size = ifelse(host == "Yes", 4, 3)))

#fastest and slowest team members
speed <- marbles %>% 
  mutate(race_type = ifelse(grepl("Q", race), "qualifier", "race")) %>%
  filter(race_type == "race") %>% 
  group_by(team_name, marble_name) %>% 
  summarise(fastest_avg_lap = round(min(avg_time_lap), 2)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  filter(!team_name %in% c("Team Primary", "Limers", "Midnight Wisps")) %>% 
  arrange(desc(fastest_avg_lap))

speed2 <- speed %>% 
  arrange(team_name, -fastest_avg_lap) %>% 
  group_by(team_name) %>% 
  mutate(
    diff = lag(fastest_avg_lap, order_by = team_name) - fastest_avg_lap,
    diff = replace_na(diff, 0)
  ) %>% 
  ungroup() %>% 
  mutate(team_name = fct_reorder(team_name, diff, sum))
  


speed2 %>% 
  ggplot(aes(
    y = team_name, 
    x = fastest_avg_lap, 
    label = marble_name
    )) + 
  geom_point() + 
  geom_line() + 
  geom_label(aes(fill = team_name, family = "Garamond"), show.legend = FALSE) + 
  scale_x_continuous(expand = c(.1, .1)) + 
  labs(
    title = "Marble Racing: Fastest and Slowest Team Members",
    subtitle = "By Fastest Average Lap Time (in seconds)",
    caption = "Joseph Pope | @joepope44", 
    y = NULL, 
    x = NULL
  ) + 
  theme_economist() + 
  theme(
    text = element_text(family = "Garamond"),
    plot.title = element_text(margin = margin(4,0,6,0)), 
    plot.title.position = "plot"
  )


