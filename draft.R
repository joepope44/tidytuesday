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


ratings <- mydata %>% 
  group_by(season, episode, episode_name, writer) %>% 
  summarize(ratings = mean(imdb_rating, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(season) %>% 
  mutate(writer = strsplit(as.character(writer), ";")) %>% 
  unnest(writer) %>% 
  arrange(desc(ratings)) %>% 
  drop_na(ratings)

ratings %>% 
  ggplot(aes(fct_reorder(writer, ratings, median), ratings)) + 
  geom_boxplot(outlier.fill = "black") + 
  coord_flip() + 
  geom_jitter(aes(color = season)) + 
  ggthemes::theme_few() +
  theme(text = element_text(family = "Helvetica"),
        plot.title = element_text(family = "Helvetica Bold",
                                  size = 20, face = "bold")) + 
  labs(title = "The Office", 
       subtitle = "Sometimes I'll Start a Graph and \nI Don't Even Know Where It's Going",
       caption = "Joseph Pope | @joepope44") + 
  xlab("") + 
  ylab("Average IMDB Ratings")

