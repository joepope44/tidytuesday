source("setup.R")
library(ggdark)

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

# filter age data to specific bins 
age_df <- tbi_age %>% 
  filter(age_group %in% c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74")) 

# create sequential bins for age demo 
age_df$age_group2 <- forcats::fct_relevel(age_df$age_group, c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74"))

# remove two uninteresting injury types 
age_df2 <- age_df %>% 
  filter(!str_detect(injury_mechanism, "mechanism")) %>% 
  
# plot and format the viz 
ggplot(age_df2, aes(age_group2, rate_est, fill = type)) + 
  geom_col() + 
  facet_grid(type ~ injury_mechanism, scales = "free", 
             labeller = label_wrap_gen(width = 16, multi_line = TRUE)) + 
  guides(fill = FALSE) + 
  dark_mode(theme_fivethirtyeight()) + 
  theme(text = element_text(family = "Helvetica", color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(linetype = 3), 
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.subtitle = element_text(margin = margin(2, 0, 4, 0))) + 
  labs(title = "Tramautic Brain Injury: Observed Cases in 2014", subtitle = "Intentional Self-harm Most Fatal Type of Incident, Regardless of Age", caption = "\nJoseph Pope | @joepope44") + 
  xlab("Age Group") + 
  ylab("Rate per 100K People") 

#spread the good news 
rtweet::post_tweet(media = "13_cdc_brain_injury/brain_injury.png",
                   status = "Expirementing with ggdark and facet grids for this week's look at tramautic brain injury data. #rstats #tidytuesday https://github.com/joepope44/tidytuesday/tree/master/13_cdc_brain_injury")
