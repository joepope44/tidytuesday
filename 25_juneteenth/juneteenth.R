source("setup.R")
library(ggmap)
# library(maps)
# library(geosphere)
library(ggrepel)


# Load All Data
tuesdata <- tidytuesdayR::tt_load("2020-06-16")
routes <- tuesdata$slave_routes

# Find unique counts
routes %>%
  group_by(ship_name, captains_name) %>%
  count() %>%
  arrange(desc(n))

routes %>%
  group_by(place_of_purchase) %>%
  count(sort = TRUE)

df1 <- routes %>%
  group_by(port_origin, place_of_purchase, port_arrival) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(
    port_origin = str_remove_all(port_origin, ", port unspecified"),
    place_of_purchase = str_remove_all(place_of_purchase, ", port unspecified"),
    port_arrival = str_remove_all(port_arrival, ", port unspecified")
  ) %>%
  drop_na() %>%
  filter(port_origin != "Africa." & place_of_purchase != "Africa.")

places_of_purchases <- df1 %>%
  group_by(place_of_purchase) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  top_n(20) %>%
  ggmap::mutate_geocode(place_of_purchase)

# Use geocode to find lat/long for cities as listed
arrivals <- df1 %>%
  group_by(port_arrival) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  filter(n > 10) %>%
  ggmap::mutate_geocode(port_arrival)

# Find most common routes from point of purchase to arrival destination
df2 <- df1 %>%
  group_by(place_of_purchase, port_arrival) %>%
  count(sort = TRUE) %>%
  ungroup()

# Join destination and arrivals. "pp" is point of purchase.
df3 <- df2 %>%
  left_join(places_of_purchases %>% select(-n), by = "place_of_purchase") %>%
  left_join(arrivals %>% select(-n), by = "port_arrival", suffix = c(".pp", ".arr"))


# Build Map ---------------------------------------------------------------

# remove NA from lat/long
df4 <- df3 %>%
  drop_na() %>%
  filter(
    place_of_purchase != port_arrival,
    !str_detect(port_arrival, "unspec"),
    lon.pp != lon.arr,
    lat.pp != lat.arr
  )

# Use data.rds so we don't ping google API too often
write_rds(df4, "25_juneteenth/data.rds")
df4 <- read_rds("25_juneteenth/data.rds")

# Create map of Atlantic Ocean region
atlantic <- ggmap::get_map(source = "stamen", maptype = "watercolor", crop = FALSE, location = c(lon = -20, lat = 10), zoom = 3)

p <- ggmap(atlantic) +
  geom_curve(
    data = df4,
    aes(
      x = lon.pp, xend = lon.arr, y = lat.pp, yend = lat.arr,
      alpha = n
    ),
    color = "darkred", inherit.aes = FALSE,
    arrow = arrow(type = "closed", length = unit(0.3, "cm"), angle = 20)
  ) +
  geom_point(
    data = places_of_purchases,
    aes(x = lon, y = lat),
    alpha = 0.5, fill = "darkred", size = 4, shape = 21
  ) +
  geom_text_repel(
    data = places_of_purchases, size = 4, family = "Trattatello",
    aes(x = lon, y = lat, label = place_of_purchase)
  ) +
  coord_quickmap() +
  theme_void() +
  guides(fill = FALSE, alpha = FALSE)


p +
  labs(
    title = "Common Routes of Slave Trade, 1514 - 1866",
    subtitle = "From Places of Purchase",
    caption = "Joseph Pope | @joepope44"
  ) +
  theme(
    text = element_text(family = "Trattatello", size = 14)
  )
