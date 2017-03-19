load("lattice.RData")
library(ggplot2)
library(MASS)
library(tidyr)

ggplot(livestock, aes(x = count, y = country, color = livestock.type)) + 
  geom_point() + 
  theme(axis.text.y = element_text(size = 7))

ggplot(livestock, aes(x = log10(count), y = country)) + 
  geom_point() + 
  facet_wrap(~livestock.type, ncol = 3) + 
  theme(axis.text.y = element_text(size = 7))

ggplot(livestock, aes(x = log10(count), y = livestock.type)) + 
  geom_point() + 
  facet_wrap(~country, ncol = 4) + 
  theme(axis.text.y = element_text(size = 7))

country.alpha = factor(livestock$country, sort(levels(livestock$country)))
livestock.alpha = data.frame(livestock.type = livestock$livestock.type, 
                             country = country.alpha, 
                             count = livestock$count)

ggplot(livestock.alpha, aes(x = log10(count), y = country)) + 
  geom_point() + 
  facet_wrap(~livestock.type, ncol = 3) + 
  theme(axis.text.y = element_text(size = 7))



#------------------------------
# Fitting a model
#------------------------------
livestock.lm = lm(log10(count) ~ livestock.type + country, data = livestock)
dummy.coef(livestock.lm)$livestock.type
dummy.coef(livestock.lm)$country

# maxit is for maximum iterations
livestock.rlm = rlm(log10(count) ~ livestock.type + country, psi = psi.bisquare, 
                    maxit = 50, data = livestock)
dummy.coef(livestock.rlm)$livestock.type
dummy.coef(livestock.rlm)$country

# Spread technique is the opposite of gather
livestock.wide = livestock %>% spread(livestock.type, count)
row.names(livestock.wide) = livestock.wide$country
livestock.wide = livestock.wide[, -1]
livestock.mp = medpolish(log10(livestock.wide))

plot(livestock.mp)

livestock.mp$col

animal.effect = sort(dummy.coef(livestock.rlm)$livestock.type)
animal = factor(names(animal.effect), levels = names(animal.effect))
animal.effect.df = data.frame(animal, animal.effect)
ggplot(animal.effect.df, aes(x = animal.effect, y = animal)) + 
  geom_point()

country.effect = sort(dummy.coef(livestock.rlm)$country)
country = factor(names(country.effect), levels = names(country.effect))
country.effect.df = data.frame(country, country.effect)
ggplot(country.effect.df, aes(x = country.effect, y = country)) + 
  geom_point()

# BELOW TWO GRAPHS ARE NOT INFORMATIVE AND NEED REVISION
livestock.rlm.df = data.frame(livestock, .fitted = fitted.values(livestock.rlm), 
                              .resid = residuals(livestock.rlm))
livestock.rlm.df$country = factor(livestock$country, levels = names(country.effect))
livestock.rlm.df$livestock.type = factor(livestock$livestock.type, levels = names(animal.effect))
ggplot(livestock.rlm.df, aes(x = log10(count), y = country)) + geom_point() + 
  facet_wrap(~livestock.type, ncol = 3) + theme(axis.text.y = element_text(size = 7))

ggplot(livestock.rlm.df, aes(x = log10(count), y = livestock.type)) + 
  geom_point() + 
  facet_wrap(~country, ncol = 4) + 
  theme(axis.text.y = element_text(size = 7))



#------------------------------
# Fitting a model
#------------------------------
ggplot(livestock.rlm.df, aes(x = .fitted, y = country)) + 
  geom_point() + 
  facet_wrap(~livestock.type, ncol = 3) + theme(axis.text.y = element_text(size = 7))

ggplot(livestock.rlm.df, aes(x = .fitted, y = livestock.type)) + 
  geom_point() + 
  facet_wrap(~country, ncol = 4) + 
  theme(axis.text.y = element_text(size = 7))

# Sheep plot has more spread
# Turkey is an outlier for pigs
ggplot(livestock.rlm.df, aes(x = .resid, y = country)) + 
  geom_point() + 
  facet_wrap(~livestock.type, ncol = 3) + 
  theme(axis.text.y = element_text(size = 7))

ggplot(livestock.rlm.df, aes(x = .resid, y = livestock.type)) + 
  geom_point() + 
  facet_wrap(~country, ncol = 4) + 
  theme(axis.text.y = element_text(size = 7))



#------------------------------
# Maps
#------------------------------
sheep.df = subset(livestock.rlm.df, livestock.type == "Sheep")
names(sheep.df)

# Change 'country' column name to 'region'
names(sheep.df)[2] = "region"
library(maps)
map.world = map_data(map = "world")
# Pretend it's the Eighties again
map.world$region[map.world$region == "UK"] = "United Kingdom"
map.world$region[map.world$region == "Czech Republic"] = "Czechoslovakia"
map.world$region[map.world$region == "Slovakia"] = "Czechoslovakia"
map.world$region[map.world$region == "Russia"] = "Russia et al."
map.world$region[map.world$region == "Latvia"] = "Russia et al."
map.world$region[map.world$region == "Lithuania"] = "Russia et al."
map.world$region[map.world$region == "Estonia"] = "Russia et al."
map.world$region[map.world$region == "Belarus"] = "Russia et al."
map.world$region[map.world$region == "Ukraine"] = "Russia et al."
map.world$region[map.world$region == "Moldova"] = "Russia et al."
map.world$region[map.world$region == "Serbia"] = "Yugoslavia"
map.world$region[map.world$region == "Montenegro"] = "Yugoslavia"
map.world$region[map.world$region == "Croatia"] = "Yugoslavia"
map.world$region[map.world$region == "Bosnia and Herzegovina"] = "Yugoslavia"
map.world$region[map.world$region == "Kosovo"] = "Yugoslavia"
map.world$region[map.world$region == "Macedonia"] = "Yugoslavia"
map.world$region[map.world$region == "Slovenia"] = "Yugoslavia"

# Merge function here merges the region in map.world and sheep.df
# xlim and ylim are giving the latitude and longitude on world map
sheep.merge = merge(map.world, sheep.df, by = "region", all.x = TRUE, all.y = FALSE)
ggplot(sheep.merge, aes(fill = .resid)) + geom_map(map = map.world, aes(map_id = region)) + 
  coord_equal() + xlim(-10, 30) + ylim(35, 65)

# install.packages('rworldmap')
library(rworldmap)
sheep.join = joinCountryData2Map(sheep.df, joinCode = "NAME", nameJoinColumn = "region")

mapCountryData(sheep.join, nameColumnToPlot = ".resid", mapRegion = "europe", 
               mapTitle = "'80s sheep residual")