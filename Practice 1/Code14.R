load("lattice.RData")
library(ggplot2)
library(GGally)
library(tidyr)

#--------------------------------------
# HAMSTER DATA
#--------------------------------------
summary(hamster)

hamster.long = hamster %>% gather(organ, weight)
ggplot(hamster.long, aes(x = organ, y = weight)) + stat_boxplot()

# We are changing the scale to capture the variation
ggplot(hamster.long, aes(x = organ, y = log2(weight))) + stat_boxplot()

# We are not transforming in this case for easy interpretation
# Before correlation check for outliers
# Hamster testes show lower correltion with other variables
# Liver seems to be a key variable as it is more correlated with other variables
ggpairs(hamster)


liver.cat = cut_number(hamster$liver, n=3)

# Removing testes and adding categorical liver
# Coloring on the categorical liver
# Hamsters with small liver have smaller other organs too
ggpairs(data.frame(hamster[,1:5], liver.cat), aes(color=liver.cat))


#--------------------------------------
# IRIS FLOWER DATA
#--------------------------------------
ggpairs(iris, aes(color=Species))

# Analysing petals
# Overlapping between versicolor and Virginica
ggplot(iris, aes(y = Species, x = Petal.Length * Petal.Width, color = Species)) + 
  geom_point()

area = iris$Petal.Length * iris$Petal.Width
ratio = iris$Petal.Length / iris$Petal.Width
iris.size = data.frame(iris, area, ratio)
ggplot(iris.size, aes(x = area, y = ratio, color=Species)) + 
  geom_point()

ggplot(iris.size, aes(x = log2(area), y = log2(ratio), color=Species)) + 
  geom_point()


# Analysing petals
# There is high overlap between the different flower species
# It become very difficult to seperate on the basis of sepal area
ggplot(iris, aes(y = Species, x = Sepal.Length * Sepal.Width, color = Species)) + 
  geom_point()

# Setosa is easily distinguishable
# Versicolor and Virginica are intermingled
area2 = iris$Sepal.Length * iris$Sepal.Width
ratio2 = iris$Sepal.Length / iris$Sepal.Width
iris.size2 = data.frame(iris, area, ratio)
ggplot(iris.size2, aes(x = area2, y = ratio2, color=Species)) + 
  geom_point()