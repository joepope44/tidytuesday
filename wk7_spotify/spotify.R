library(tidyverse)
library(ggridges)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


# EDA ---------------------------------------------------------------------

glimpse(spotify_songs)

str(spotify_songs)

spotify_songs %>%
  summarise_all(n_distinct)

# key of D: key = 2 
hist(spotify_songs$key)

# minor key: mode = 0 
hist(spotify_songs$mode)

# Convert numeric keys to A - G keys 
df <- spotify_songs %>%
  mutate(clean_mode = ifelse(mode == 1, "Major", "Minor"),
         clean_key = case_when(
           key == 0 ~ "C", 
           key == 1 ~ "C#", 
           key == 2 ~ "D",
           key == 3 ~ "D#",
           key == 4 ~ "E",
           key == 5 ~ "F",
           key == 6 ~ "F#",
           key == 7 ~ "G",
           key == 8 ~ "G#",
           key == 9 ~ "A",
           key == 10 ~ "A#",
           key == 11 ~ "B",
           TRUE ~ "-"),
         keymode = str_c(clean_key, clean_mode, sep = " "),
         isDmin = ifelse(keymode == "D Minor", 1, 0)) %>%
  filter(!is.na(keymode))

#' valence = sadness of music. A measure from 0.0 to 1.0 describing the musical 
#' positiveness conveyed by a track. Tracks with high valence sound more positive 
#' (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative 
#' (e.g. sad, depressed, angry).

table(df$keymode)

df %>%
  group_by(clean_key, keymode, isDmin) %>%
  count() %>%
  ggplot(aes(x = fct_rev(reorder(keymode, clean_key)), y = n,
             fill = isDmin)) +
  geom_col()  + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ylab("# of Songs") + 
  xlab("") + 
  ggtitle("D Minor vs. Other Songs", "")

table(df$playlist_genre)

# explore genre breakdown 
df %>%
  group_by(clean_key, keymode, isDmin, playlist_genre) %>%
  count() %>%
  ggplot(aes(x = fct_rev(reorder(keymode, clean_key)), y = n,
             fill = isDmin)) +
  geom_col()  + 
  coord_flip() + 
  facet_wrap(~ playlist_genre) + 
  guides(fill = FALSE) + 
  ylab("# of Songs") + 
  xlab("") + 
  ggtitle("D Minor vs. Other Songs", "by Genre")

by_keymode <- df %>%
  group_by(clean_key, keymode, isDmin, playlist_genre) %>%
  summarize(valence = mean(valence)) %>%
  ungroup()


# boxplot for Rock only 
df %>% 
  filter(playlist_genre == 'rock') %>%
  ggplot(aes(x = fct_reorder(keymode, valence, fun = median, desc = TRUE),
             y = valence , fill = isDmin)) +
  geom_boxplot() + 
  coord_flip() + 
  guides(fill = FALSE) + 
  ylab("Sadness Score") + 
  xlab("") + 
  ggtitle("D Minor vs. Other Songs", "Rock only")


# GGRidges ----------------------------------------------------------------

a <- ifelse(df$isDmin == 1, "red", "dark grey")

df2 <- df %>% mutate(keymode = 
                fct_reorder(keymode, valence, fun = median, desc = TRUE))

# with ggridges
p <- df2 %>% 
  filter(playlist_genre == 'rock') %>%
  ggplot(aes(x = valence, 
             y = fct_reorder(keymode, valence, fun = median, desc = TRUE),
             fill = factor(isDmin))) +
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) + 
  guides(fill = FALSE) + 
  ylab("") + 
  xlab("Sadness Score (Valence)") + 
  theme_ridges(font_size = 12, font_family = "Palatino") + 
  scale_fill_manual(values = c("dark grey", "royal blue")) + 
  coord_cartesian(xlim = c(0,1))
  
p

st <- "Nigel Tufnel:
It's part of a trilogy, a musical trilogy I'm working on in D minor which 
is the saddest of all keys, I find. People weep instantly when they hear it, 
and I don't know why.        -- Nigel Tufnel, This is Spinal Tap"

st <- paste0(strwrap(st, 140), sep="", collapse="\n")

p <- p + theme(panel.grid.major = element_line(linetype = "blank"), 
          panel.grid.minor = element_line(linetype = "blank"), 
          panel.background = element_rect(fill = NA),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(size = 10)) +
  theme(plot.title=element_text(face="bold", size = 20, 
                                margin = margin(b = 15))) + 
  theme(plot.subtitle=element_text(margin=margin(b=15), 
                                   size = 10)) + 
  theme(plot.caption=element_text(margin=margin(t=15),
                                  face="italic", size=10)) + 
  ggtitle(label = "D Minor is actually the 4th saddest key in Rock", 
            subtitle = st) + 
  labs(caption = "Joseph Pope | @joepope44") 

ggsave("wk3_spotify/spotify.png", p)
