source('setup.R')
library(ggExtra)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2020-04-28')

grosses <- tuesdata$grosses

start_time <- tuesdata$`pre-1985-starts`

agg <- grosses %>% 
  group_by(show) %>% 
  summarise(overall = sum(weekly_gross_overall, na.rm = TRUE), 
            weekly = sum(weekly_gross, na.rm = TRUE), 
            share = weekly / overall, 
            pct_capacity = mean(pct_capacity, na.rm = TRUE),
            start_date = min(week_ending, na.rm = TRUE),
            max_date = max(week_ending, na.rm = TRUE), 
            weeks_running = n()) %>% 
  ungroup() %>% 
  arrange(desc(weekly)) 

shares <- agg %>% 
  filter(weekly >= 100000 & weeks_running >= 10) %>% 
  arrange(desc(share)) %>% 
  top_n(5, share) %>% 
  pull(show)

agg <- agg %>% 
  mutate(labels = ifelse(show %in% shares, show, ""))

p <- agg %>% 
  filter(weekly >= 100000 & weeks_running >= 10) %>% 
  ggplot(aes(share, weekly, label = labels)) +
  geom_point(alpha = 0.5, 
             color = "skyblue3") + 
  scale_y_log10(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = seq(0, .1, .02)) + 
  ggthemes::theme_tufte() + 
  labs(title = "Which Plays Earned the Most with Greatest Share of Weekly Gross Revenue?", 
       subtitle = "Minimum 10 Week Run and $100K total gross revenue", 
       x = "\nMarket Share of Total Weekly Grosses",
       y = "Total Weekly Gross",
       caption = "Data: Playbill\nViz: Joseph Pope | @joepope44") + 
  theme(plot.subtitle = ggtext::element_markdown(), 
        plot.title.position = "plot", 
        plot.caption.position = "plot",
        plot.margin = margin(10, 4, 4, 4)) + 
  ggrepel::geom_text_repel(size = 3)
  
p

# ggMarginal(p, type="histogram", fill = "grey90", color = "white")


