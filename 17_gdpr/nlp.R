source('draft.R')
library(tidytext)


summary_tokens <- clean %>% 
  unnest_tokens(word, summary, to_lower = TRUE, strip_punct = TRUE) %>% 
  anti_join(stop_words)

df <- summary_tokens %>% 
  count(article_num, word, sort = TRUE)

df2 <- df %>% 
  group_by(article_num) %>% 
  summarise(total = sum(n))
  
art_words <- left_join(df, df2) %>% 
  filter(is.na(as.numeric(word)))

art_words2 <- art_words %>%
  bind_tf_idf(article_num, word, n) %>% 
  select(-total) %>%
  arrange(desc(tf_idf))

art_words2 %>%
  filter(article_num %in% c(5, 6, 32, 13, 15),
         n > 1) %>% 
  arrange(desc(tf_idf)) %>%
  # mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(article_num) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = article_num)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~article_num, scales = "free") +
  coord_flip()


# td_idf ------------------------------------------------------------------


clean %>% 
  unnest_tokens(word, summary, to_lower = TRUE) %>% 
  filter(is.na(as.numeric(word))) %>% 
  anti_join(stop_words) %>% 
  bind_tf_idf(word, id, n)


# pairwise ----------------------------------------------------------------


library(widyr)

word_pairs <- summary_tokens %>% 
  filter(!str_detect(word, "^[0-9]+$")) %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

word_pairs

hist(word_pairs$n, breaks = 50)

library(igraph)
library(ggraph)

set.seed(1234)

word_pairs %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr", ) +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()



# textminer ---------------------------------------------------------------

library(textmineR)

# create a document term matrix 
dtm <- CreateDtm(doc_vec = gdpr_violations$summary, # character vector of documents
                 doc_names = gdpr_violations$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = TRUE, # Turn off status bar for this demo
                 cpus = 4) # default is all available cpus on the system

dtm <- dtm[,colSums(dtm) > 2]
dtm

set.seed(12345)

model <- FitLdaModel(dtm = dtm, 
                     k = 10,
                     iterations = 500, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 

str(model)

plot(model$log_likelihood, type = "l")

# probabilistic coherence, a measure of topic quality
# this measure can be used with any topic model, not just probabilistic ones
summary(model$coherence)

hist(model$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)

head(t(model$top_terms))

     
# Get the prevalence of each topic
# You can make this discrete by applying a threshold, say 0.05, for
# topics in/out of docuemnts. 
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# prevalence should be proportional to alpha
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")


# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 2)
model$labels

head(model$labels)
# label_1             
# t_1 "health_data"       
# t_2 "personal_details"  
# t_3 "data_protection"   
# t_4 "affected_person"   
# t_5 "personal_data"     
# t_6 "video_surveillance"

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)

model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]

ms <- model$summary



ms %>% 
  ggplot(aes(coherence, prevalence, label = label_1)) + 
  geom_point() + 
  ggrepel::geom_label_repel(force = 10) + 
  hrbrthemes::theme_ipsum() + 
  labs(title = "Topic Modeling of GDPR Summary Textual Data", subtitle = "Topics appear to either be common or well suited to the topic", x = "Coherence", y = "Prevalence", caption = "Data from privacyaffairs.com\nViz by Joseph Pope | @joepope44") 
  





# construct the matrix of term counts to get the IDF vector
tf_mat <- TermDocFreq(dtm)

hist(tf_mat$term_freq, breaks = 30)

# TF-IDF and cosine similarity
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)


summary(cdist)

hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 10)

plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 10, border = "red")

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})


