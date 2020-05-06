source("setup.R")
library(ggdark)
library(ggrepel)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 16)

polls <- tuesdata$polls

rankings <- tuesdata$rankings

table(polls$critic_country)


font_add_google("Alegreya Sans", "aleg")

era_levels = c("70's", "80's", "90's", "00's", "10's")

# how many critics were in the poll? 
n <- length(unique(polls$critic_name))

# list of artists to investigate further
artists_of_note <- c("The Notorious B.I.G.", "2Pac")


r2 <- rankings %>% 
  filter(n1 > 0) %>% 
  mutate(top3 = n1 + n2 + n3, 
         decade = as.factor(year - year %% 10), 
         era = str_c(str_sub(decade, start = -2), "'s"),
         era = fct_relevel(era, era_levels), 
         artist_label = ifelse(artist %in% artists_of_note & 
                                 n1 > 1,
                               str_c(artist, "\n", title),
                                     ""))

p <- r2 %>% 
  ggplot(aes(n1, points, color = era, label = artist_label)) + 
  # geom_point() +
  geom_jitter(width = .1, alpha = 0.6, size = 3) + 
  ggdark::dark_theme_bw() +
  scale_x_continuous(breaks = seq(1:10), limits = c(.5, 10)) +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150)) + 
  labs(title = "The weak or the **strong**, who got it goin' on?", 
       subtitle = 'Biggie Smalls\' "Juicy" runaway most #1 votes by 107 music critics.\nComparing to 2Pac\'s "Dear Mama".',
       caption = "Data: BBC Music\nJoseph Pope | @joepope44") + 
  xlab("Total #1 Ranked Votes") + 
  ylab("Total Points, All Rankings") + 
  ggrepel::geom_label_repel(show.legend = FALSE, direction = "x", segment.alpha = .5, nudge_x = c(-4, 4), family = "Garamond") +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        text = element_text(family = "Garamond"), 
        plot.title = element_markdown(size = 20, margin = margin(0,0,2,0)),
        plot.title.position = "plot",
        plot.subtitle = element_text(margin = margin(2,0,10,0)),
        axis.title.x = element_text(margin = margin(10,0,0,0)),
        panel.grid.minor = element_blank()) 

#spread the good news 
rtweet::post_tweet(media = "16_rap/hiphop.png",
                   status = "A little East Coast vs West Coast this week. Focusing on who critics picked with their #1 pick. #rstats #tidytuesday")
