source("setup.R")

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

table(gdpr_text$chapter_title)

gdpr_violations %>% 
  ggplot(aes(name, price, group = name)) + 
  geom_col() + 
  coord_flip()

violations <- gdpr_violations %>% 
  mutate(date = lubridate::mdy(date), name = factor(name)) %>% 
  filter(lubridate::year(date) > '2018') 

clean %>% 
  group_by(article_num) %>% 
  tally() %>% 
  arrange(desc(n))

# aggregate gdpr text
text <- gdpr_text %>% 
  group_by(chapter_title, article, article_title) %>% 
  slice(1) %>% 
  ungroup()


# extract all articles involved and create new rows per each article 
clean <- violations %>% 
  mutate(article_num = str_extract_all(string = article_violated, 
                                      pattern = "(?<=Art. )(\\d+)")) %>% 
  unnest(article_num) %>% 
  mutate(article_num = as.integer(article_num)) %>% 
  left_join(select(text, article, article_title, chapter_title), by = c("article_num" = "article"))
  
# not interesting 
violations %>% 
  ggplot(aes(date, price, color = name, group = name)) + 
  geom_line() + 
  guides(color = FALSE) + 
  facet_wrap(~ name, scales = "free")

# mode of controllers/companies 
gdpr_violations %>% 
  group_by(controller) %>% 
  tally(id) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  ggplot(aes(fct_reorder(controller, n), n, fill = controller)) + 
  geom_col() + 
  coord_flip() + 
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("")
  
# mode of controllers/companies 
gdpr_violations %>% 
  group_by(authority) %>% 
  tally(id) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  ggplot(aes(fct_reorder(authority, n), n, fill = authority)) + 
  geom_col() + 
  coord_flip() + 
  theme(legend.position = "none") + 
  xlab("") + 
  ylab("")

