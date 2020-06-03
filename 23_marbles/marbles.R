source('setup.R')

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
  group_by(race, race_type, site, marble_name, host) %>% 
  summarise(fastest_time_lap = 
              min(avg_time_lap, na.rm = TRUE),
            slowest_time_lap = 
              max(avg_time_lap, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(fastest_time_lap, slowest_time_lap)) +
  geom_point(aes(color = race_type)) + 
  # coord_flip() + 
  facet_grid(site ~ host)
  # geom_violin(aes(fill = race_type))

# Check for home field advantage
marbles %>% 
  mutate(race_type = ifelse(grepl("Q", race), "qualifier", "race")) %>%
  filter(race_type == "race") %>% 
  ggplot(aes(avg_time_lap, site)) +
  geom_point(aes(color = host, size = ifelse(host == "Yes", 4, 3)))
