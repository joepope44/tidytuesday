---
title: "measles"
author: "Joseph Pope"
date: "2/25/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)

```


```{r data}

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

```
```{r EDA}

# count of school type 
measles %>%
  group_by(type) %>% 
  tally()

measles %>% 
  group_by(mmr) %>% 
  tally()

measles %>% 
  group_by(overall) %>% 
  tally()

m2 <- measles %>% 
  filter(mmr != -1 & overall != -1) 

```


```{r}
m2 %>% 
  ggplot(aes(mmr, overall, alpha = .2)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~ state)
```


```{r}
m3 <- measles %>% 
  mutate(xrel_cnt = round(xrel * enroll / 100, 0),
         xmed_cnt = round(xmed * enroll / 100, 0), 
         xper_cnt = round(xper * enroll / 100, 0)) %>% 
  group_by(name, type, city, county, state) %>% 
  summarize(enroll = round(mean(enroll), 0),
            xrel_cnt = sum(xrel_cnt, na.rm = TRUE), 
            xmed_cnt = sum(xmed_cnt, na.rm = TRUE),
            xper_cnt = sum(xper_cnt, na.rm = TRUE)) %>% 
  ungroup()
  
```

```{r}
m4 <- m3 %>% 
  group_by(state) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    ungroup() %>% 
  rename("personal" = xper_cnt, 
         "medical" = xmed_cnt, 
         "religious" = xrel_cnt) %>% 
  filter(enroll != 0) %>% 
  mutate(enroll = enroll - personal - medical - religious) %>% 
  gather(key = "category", value = "val", 
  "religious", "medical", "personal" , "enroll")
```

```{r}

m4 %>% 
  ggplot(aes(factor(state), enroll)) + 
  geom_col(position = position_stack())
```


