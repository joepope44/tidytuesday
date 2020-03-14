library(tidyverse)
library(cowplot)
library(extrafont)
library(showtext)

font_add_google("Abril Fatface")

showtext_auto()
loadfonts()

#pull in all data sources 

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

# join data and clean names
df <- dplyr::left_join(tuition_cost, diversity_school, 
                   by = c("name", "state")) %>%
  janitor::clean_names()

# create df with just women and men enrollment 
gender <- df %>% 
  filter(category == 'Women') %>% 
  pivot_wider(names_from = category, values_from = enrollment) %>% 
  mutate(male = total_enrollment - Women,
         female = Women) %>% 
  select(-Women)

# create df with just white and non-white enrollment 
race <- df %>% 
  filter(category %in% c('White', 'Total Minority')) %>% 
  pivot_wider(names_from = category, values_from = enrollment) %>%
  select(white = White, total_minority = `Total Minority`)
  
df1 <- bind_cols(gender, race) 

#race 
race_plot <- df1 %>% 
  ggplot(aes(white, total_minority, color = type)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_cowplot() + 
  coord_cartesian(expand = FALSE,
    xlim = c(0, 41000)) +
  scale_y_continuous(label = scales::unit_format(scale = .001, suffix = "K"),
                     limits = c(0,50000)) +
  scale_x_continuous(label = scales::unit_format(scale = 1e-3, suffix = "K"),
                     limits = c(0,40000)) +
  theme(legend.position = "none", 
        text = element_text(family = "Franklin Gothic Medium"), 
        plot.title = element_text(margin = margin(0,0,12,0))) + 
  xlab("White") + 
  ylab("Non-White") + 
  ggtitle(label = "By Race")

#gender 
gender_plot <- df1 %>% 
  ggplot(aes(male, female, color = type)) + 
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_cowplot() + 
  coord_cartesian(expand = FALSE,
                  xlim = c(0, 41000)) +
  scale_y_continuous(label = scales::unit_format(scale = .001, suffix = "K"),
                     limits = c(0,50000)) +
  scale_x_continuous(label = scales::unit_format(scale = 1e-3, suffix = "K"),
                     limits = c(0,40000)) +
  ggtitle("By Gender") + 
  theme(legend.position = "none", 
        text = element_text(family = "Franklin Gothic Medium"), 
        plot.title = element_text(margin = margin(0,0,12,0))) + 
  xlab("Male") + 
  ylab("Female") 

title <- ggdraw() + 
  draw_label(
    "For Profit Schools are the Most Diverse",
    fontface = 'bold',
    x = 0, 
    hjust = 0, 
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

#create sharted legend 
legend <- get_legend(
  race_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.title = element_blank())
)

# create row of the race and gender plots 
plot_row <- plot_grid(race_plot, gender_plot,
                      align = "h", axis = "bt")

#plot title, race and gender plots and legend
plot_grid(title, plot_row, legend,
          ncol=1,
          rel_heights = c(0.1, 1, 0.1))

