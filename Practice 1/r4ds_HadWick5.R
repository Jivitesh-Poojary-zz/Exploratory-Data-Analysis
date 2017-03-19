# MAPS

library(ggplot2)
library(maps)
library(mapproj)

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

# Map of New Zealand
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# Map of United States
usMapEnv <- map_data("usa")
ggplot(usMapEnv, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# Map of United States with coord map
usMapEnv <- map_data("usa")
ggplot(usMapEnv, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_map()

# Map of Italy
itMapEnv <- map_data("italy")
ggplot(itMapEnv, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# Map of France
frMapEnv <- map_data("france")
ggplot(frMapEnv, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()
