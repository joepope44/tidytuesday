source("setup.R")
library(schrute)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

mydata <- schrute::theoffice

#count episodes per season 
mydata %>% 
  group_by(season, episode) %>% 
  summarize(ratings = mean(imdb_rating, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  count()

#' create df of average rating per writer
#' split out episodes where there are multiple credited writers to rows

ratings <- mydata %>% 
  group_by(season, episode, episode_name, writer) %>% 
  summarize(ratings = mean(imdb_rating, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(season) %>% 
  mutate(writer = strsplit(as.character(writer), ";")) %>% 
  unnest(writer) %>% 
  arrange(desc(ratings)) %>% 
  drop_na(ratings)

#count epsidoes per writer 
#
writer_counts <- ratings %>% 
  group_by(writer, season, episode) %>% 
  summarize(ratings = mean(ratings, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(writer) %>% 
  count() %>% 
  arrange(n)

df <- ratings %>% 
  left_join(writer_counts, 
            by = "writer") %>% 
  rename(written_episodes = n)

subtitle <- expression(paste("Which writers and seasons were favored by fans of ", italic("The Office"), "? (Min 2 episodes)"))

df %>% 
  filter(written_episodes > 1) %>% 
  ggplot(aes(fct_reorder(writer, ratings, median), ratings)) + 
  # geom_boxplot(outlier.size = 1, alpha = 0) + 
  geom_point(aes(color = season), size = 3) + 
  geom_point(shape = 1, size = 3, colour = "black") + 
  coord_flip() + 
  scale_color_viridis_d(option = "cividis", direction = -1) + 
  ggthemes::theme_few() +
  theme(text = element_text(family = "Helvetica", color = "white"),
        axis.text = element_text(color = "white"), 
        legend.text = element_text(color = "white"), 
        rect = element_rect(fill = "#1F2126"), 
        legend.key = element_rect(fill = "#1F2126"),
        plot.title = element_text(family = "Helvetica Bold",
                                  size = 24, face = "bold",
                                  color = "white"),
        plot.subtitle = element_text(margin = margin(2, 0, 4, 0)), 
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "#1F2126"),
        panel.background = element_rect(fill = "gray90"),
        panel.grid.major.x = element_line(color = "lightskyblue4", 
                                          linetype = "dotted")) + 
  labs(title = "The Office", 
       subtitle = subtitle,
       caption = "Joseph Pope | @joepope44",
       color = "Season") + 
  xlab("") + 
  ylab("Average IMDB Ratings")

#spread the good news 
rtweet::post_tweet(media = "12_the_office/office.png",
                   status = "Sometimes I'll start a graph and I don't even know where it's going. I just hope I find it along the way. #rstats #tidytuesday https://github.com/joepope44/tidytuesday/tree/master/12_the_office")
