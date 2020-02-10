library(tidyverse)
library(magrittr)
library(teamcolors)
library(extrafont)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')


games %<>%
  mutate(week = as.numeric(week))

attendance %<>%
  drop_na(week)
 
df <- attendance %>%
  inner_join(games, by = c(
      'year' = 'year',
      'week' = 'week',
      'team_name' = 'home_team_name',
      'team' = 'home_team_city')) 

df2 <- df %>%
  mutate(game_date = lubridate::mdy(str_c(date, ', ', as.character(year))),
         home_team_pt_diff = ifelse(home_team == winner, 
                                    pts_win - pts_loss, 
                                    pts_loss - pts_win)) %>%
  group_by(home_team, year) %>%
  mutate(home_team_pt_diff_lag = 
           lag(home_team_pt_diff),
         max_att = max(weekly_attendance),
         perc_of_max = weekly_attendance / max_att,
         mean_att = mean(weekly_attendance),
         perc_from_mean = round((weekly_attendance - mean_att) / mean_att, 2)) %>%
  ungroup()
  

# df2 %>%
#   subset(year > 2010) %>%
#   ggplot(aes(week, perc_from_mean, group=year)) + 
#   geom_line() + 
#   facet_wrap(~ home_team) 
  
# df2$home_team == teamcolors$sportslogos_name

# add in NFL team colors from teamcolors package
nfl_teamcolors_df <- teamcolors %>%
  filter(league == 'nfl') %>%
  as_tibble()

# nfl_teamcolors <- as.character(nfl_teamcolors_df$primary)
# names(nfl_teamcolors) <- as.character(nfl_teamcolors_df$name)

df3 <- df2 %>%
  left_join(nfl_teamcolors_df, by = c('home_team' = 'sportslogos_name'))
  

# df3 %>%
#   group_by(home_team, year, division) %>%
#   summarize(perc_from_mean = 
#               sum((weekly_attendance - mean_att) / mean_att)) %>%
#   ungroup() %>%
#   mutate(pos = perc_from_mean >= 0) %>%
#   ggplot(aes(year, perc_from_mean, fill = pos)) + 
#   geom_col(position = "identity", color = "black", size = 0.25) + 
#   facet_grid(division ~ home_team, scales = "free_y", drop = TRUE, as.table = TRUE) + 
#   # scale_y_continuous(labels = scales::percent(1)) + 
#   guides(fill = FALSE) + 
#   theme(axis.text=element_blank())
  
# potential final graph, with facet wrap by division 

# Final Graph -------------------------------------------------------------


df4 <- df3 %>%
  filter(!home_team %in% c('San Diego Chargers', 'St. Louis Rams')) %>%
  group_by(home_team, year, division, primary, secondary) %>%
  summarize(weekly_attendance = round(mean(weekly_attendance), 0)) %>%
  ungroup() %>%
  select(home_team, year, division, primary, secondary, weekly_attendance)
  
p <- ggplot(df4, aes(year, weekly_attendance)) + 
  geom_bar(stat = "identity", width = .8, 
           aes(color = primary, fill = secondary)) + 
  facet_wrap(division ~ home_team, ncol = 4) + 
  guides(fill = FALSE, color = FALSE, size = FALSE) + 
  scale_fill_identity(aesthetics = c("color", "fill")) + 
  scale_y_continuous(name = "Yearly Average Attendance\n",
                     labels = scales::number_format(scale = .001, 
                                                   suffix = "K")) +
  scale_x_continuous(name = "\nYear",
                     breaks = c(2000,2010,2019)) + 
  theme(
    text = element_text(family = "Rockwell"),
    axis.title = element_text(size = 9),
    strip.text = element_text(margin = margin(b=5)),
    strip.text.x = element_text(face = "bold", size = 9),
    strip.text.y = element_text(size = 6),
    strip.background = element_rect(fill = "lightblue", 
                                    size = 1, colour = "lightblue"),
    axis.ticks = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "light grey")  
  ) + 
  ggtitle("Weekly Attendance by Team and Division", 
          subtitle = "2000 - 2019")
p

