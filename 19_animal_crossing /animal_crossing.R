source('setup.R')
library(tidyverse)
library(textmineR)
library(tidymodels)
library(textrecipes)

user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

word_count <- user_reviews %>% 
  unnest_tokens(word, text, ) %>% 
  count(user_name, word, sort = TRUE)

total_count <- word_count %>% 
  group_by(user_name) %>% 
  summarise(total = sum(n))

user_words <- left_join(word_count, total_count)

df <- user_words %>% 
  anti_join(stop_words) %>% 
  bind_tf_idf(word, user_name, n)  %>% 
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(n >= 3)

df <- df %>% 
  left_join(select(user_reviews, user_name, grade), by = "user_name") 

df %>% 
  filter(grade == 0 | grade == 10) %>% 
  filter(!word %in% c("con", "que", "es", "de", "una")) %>% 
  group_by(grade) %>% 
  top_n(10, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = grade)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ grade, scales = "free") + 
  coord_flip() + 
  labs(x = NULL, y = "TF IDF") 
  

# Pairwise ----------------------------------------------------------------

word_pairs <- user_words %>% 
  filter(!word %in% stop_words$word) %>%
  pairwise_count(word, user_name, sort = TRUE)

island <- word_pairs %>% 
  filter(item1 == "island" | item2 == "island")


# Tidymodels --------------------------------------------------------------

review_rec <- recipe(grade ~ text, data = user_reviews) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_tokenfilter(text, max_tokens = 500) %>%
  step_tfidf(text) %>%
  step_normalize(all_predictors())

review_prep <- prep(review_rec)

review_prep %>%  juice()
 