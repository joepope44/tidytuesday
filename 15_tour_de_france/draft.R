source("setup.R")
library(gghighlight)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 15)

tdf_winners <- tuesdata$tdf_winners

tdf_stages <- tuesdata$tdf_stages

stages <- tuesdata$stage_data

tdf_winners %>% 
  drop_na(c(distance, time_overall)) %>% 
  mutate(avg_speed = distance / time_overall) %>%  # km / hr 
  ggplot(aes(edition, avg_speed, color = winner_name)) + 
  geom_point(size = 3, alpha = .5) + 
  scale_color_manual(values = "#F6D958") + 
  gghighlight::gghighlight(winner_name == "Lance Armstrong", use_direct_label = FALSE) + 
  geom_smooth(se = FALSE) + 
  ggthemes::theme_few(base_family = "Calibri") + 
  labs(title = "<b style='color:#F6D958; font-size:22px;'>Lance Armstrong</b> Avg. Speed vs. All Tour De France Winners\n",
       subtitle = "Armstrong Era : 1999-2005", 
       caption = "Joseph Pope | @joepope44") + 
  ylab("Average Speed, km/hr") + 
  xlab("Edition of the Tour\n") +
  theme(legend.position = "none", 
        plot.title=element_markdown(size = 12), 
        panel.background = element_rect(fill = "#2C4097"), 
        plot.title.position = "plot")
  
#spread the good news 
rtweet::post_tweet(media = "15_tour_de_france/tour_france.png",
                   status = "Exploring Lance Armstrong's dominant run in the 2000s! #rstats #tidytuesday https://github.com/joepope44/tidytuesday/tree/master/15_tour_de_france")


