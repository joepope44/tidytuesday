source("setup.R")
library(schrute)
library(ggthemes)

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
  rename(written_epsidoes = n)


df %>% 
  filter(written_epsidoes > 1) %>% 
  ggplot(aes(fct_reorder(writer, ratings, median), ratings)) + 
  # geom_boxplot(outlier.size = 1, alpha = 0) + 
  geom_point(aes(color = season), size = 2.5) + 
  coord_flip() + 
  scale_color_viridis_d(option = "cividis", direction = -1, ) + 
  ggthemes::theme_few() +
  theme(text = element_text(family = "Helvetica", color = "white"),
        axis.text = element_text(color = "white"), 
        legend.text = element_text(color = "white"), 
        rect = element_rect(fill = "#1F2126"), 
        legend.key = element_rect(fill = "#1F2126"),
        plot.title = element_text(family = "Helvetica Bold",
                                  size = 20, face = "bold",
                                  color = "white"),
        plot.background = element_rect(fill = "#1F2126"),
        panel.background = element_rect(fill = "gray90"),
        panel.grid.major.x = element_line(color = "lightskyblue4", 
                                          linetype = "dotted")) + 
  labs(title = "The Office", 
       subtitle = '"Sometimes I\'ll Start a Graph and I Don\'t Even Know Where It\'s Going."\n Ratings per Writer of Episode, minimum two episodes',
       caption = "Joseph Pope | @joepope44",
       color = "Season") + 
  xlab("") + 
  ylab("Average IMDB Ratings")

