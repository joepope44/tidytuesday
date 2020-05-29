source("./setup.R")
library("ggtext")
library("ggrepel")
library("gghighlight")

tuesdata <- tidytuesdayR::tt_load('2020-05-19')

vb_matches <- tuesdata$vb_matches

glimpse(vb_matches)

wins <- bind_rows(
  vb_matches %>% 
    group_by(player = w_player1, gender) %>% 
    tally(),
  vb_matches %>% 
    group_by(player = w_player2, gender) %>% 
    tally()) 

wins <- wins %>% 
  group_by(player, gender) %>% 
  summarise(wins = sum(n)) %>% 
  ungroup()

losses <- bind_rows(
  vb_matches %>% 
    group_by(player = l_player1, gender) %>% 
    tally(),
  vb_matches %>% 
    group_by(player = l_player2, gender) %>% 
    tally())

losses <- losses %>% 
  group_by(player, gender) %>% 
  summarise(losses = sum(n)) %>% 
  ungroup()

winloss <- wins %>% 
  inner_join(losses, by = c("player", "gender")) %>% 
  mutate(ratio = wins / (losses + wins)) %>% 
  arrange(desc(wins))



winloss %>% 
  mutate(
    gender = ifelse(gender == "M", "Male", "Female"),
    label = ifelse(player %in% winloss$player[1:2], player, "")
    ) %>% 
  ggplot(aes(wins, losses, label = label)) +
  geom_point(
    alpha = 0.3, 
    aes(
      color = ifelse(wins > 1000, 'grey4', 'red'), 
      size = ifelse(wins > 1000, 6, 4))
    ) + 
  geom_abline(slope = 1, linetype = 3) + 
  labs(
    title = "**Win/Loss in Beach Volleyball**",
    subtitle = "Top Players per Gender Highlighted, Slope Shows .500 Win Rate", 
    y = "Losses", 
    x = "Wins",
    caption = "Joseph Pope | @joepope44"
    ) +
  facet_wrap(~ gender) + 
  ggthemes::theme_solarized_2() + 
  theme(
    panel.background = element_rect(fill = "navajowhite3"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_markdown(),
    strip.text = element_markdown(face = "bold"),
    text = element_text(family = "Tahoma"), 
    legend.position = "none"
    ) + 
  ggrepel::geom_label_repel(vjust = 3)
# + 
  # gghighlight::gghighlight(
  #   wins > 1000,   
  #   unhighlighted_params = 
  #     list(size = 1, colour = alpha("grey", 0.4))
  #   ) + 



