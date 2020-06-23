source("setup.R")
library(ggmap)
library(maps)
library(geosphere)
library(ggrepel)


# Load All Data 
tuesdata <- tidytuesdayR::tt_load('2020-06-16')
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

# World map is available in the maps packag

# Formula to plot travel route 

plot_route = function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}


# remove NA from lat/long 
df4 <- df3 %>% 
  drop_na() %>% 
  filter(
    place_of_purchase != port_arrival,
    !str_detect(port_arrival, "unspec"),
    lon.pp != lon.arr,
    lat.pp != lat.arr
    )

write_rds(df4, "25_juneteenth/data.rds")

# No margin
par(mar=c(0,0,0,0))

# World map
map('world',
    col="khaki4", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-70,70), xlim=c(-130,40)
)

# Plot Places of Purchase
points(x=df3$lon.pp, y=df3$lat.pp, col="slateblue", cex=2, pch=20)

# Plot Arrivals 
points(x=df3$lon.arr, y=df3$lat.arr, col="firebrick", cex=2, pch=20)

# add every route:
for(i in 1:nrow(df4)){
  plot_route(df4$lon.pp[i], df4$lat.pp[i], df4$lon.arr[i], df4$lat.arr[i], col="skyblue", lwd=1)
}

# add points and names of cities
points(x=data$long, y=data$lat, col="slateblue", cex=2, pch=20)
text(rownames(data), x=data$long, y=data$lat,  col="slateblue", cex=1, pos=4)



# GGPLOT METHOD -----------------------------------------------------------

df4 <- read_rds("25_juneteenth/data.rds")

# data <- map_data("world", ylim=c(-70,70), xlim=c(-130,30))
# data <- map_data("world")
# 
# data %>% filter(`region` != "Antarctica") %>%
#   ggplot(aes(long, lat, group = group)) +
#   geom_polygon(fill="black", color = "white", size=0.15) +
#   geom_curve(data = df4, 
#              aes(
#                x = lon.pp, xend = lon.arr, y = lat.pp,  yend = lat.arr,
#                alpha = n), 
#              size=0.05,
#              color = "white", inherit.aes = FALSE
#              ) +
#   theme_void() +
#   theme(plot.background=element_rect(fill="black"), legend.position="none") +
#   coord_equal()




atlantic <- get_map(source="stamen", maptype="watercolor", crop=FALSE, location = c(lon = -20, lat = 10), zoom = 3)


p <- ggmap(atlantic) + 
  # geom_polygon(fill="black", color = "white", size=0.15) +
  geom_curve(
    data = df4, 
             aes(
               x = lon.pp, xend = lon.arr, y = lat.pp,  yend = lat.arr,
               alpha = n), 
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
  # theme(plot.background=element_rect(fill="black"), legend.position="none") +
  guides(fill = FALSE, alpha = FALSE)


p + 
  labs(
    title = "Common Routes of Slave Trade, 1514 - 1866",
    subtitle = "From Places of Purchase", 
    caption = "Joseph Pope | @joepope44"
  )  + 
  theme(
    text = element_text(family = "Trattatello", size = 14)
  )
