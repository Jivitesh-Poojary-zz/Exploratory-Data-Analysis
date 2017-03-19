library(dplyr)
library(lattice)
library(ggplot2)
library(GGally)

ggplot(singer, aes(x=height)) + stat_ecdf() 
ggplot(singer, aes(x=height)) + geom_histogram()
ggplot(singer, aes(x=height)) + geom_histogram(binwidth=1)
ggplot(singer, aes(x=height)) + geom_density()
ggplot(singer, aes(x=height)) + geom_density(adjust = 0.5) ##less smooth
ggplot(singer, aes(x="Height", y=height)) + geom_boxplot() # 'Height' is the dummy variable
ggplot(singer, aes(x=voice.part, y=height)) + geom_boxplot()


# Normal and uniform QQ plots
ggplot(singer, aes(sample=height)) + stat_qq()
ggplot(singer, aes(sample=height)) + stat_qq(distribution=qunif)

# Faceting
ggplot(singer, aes(sample=height)) + stat_qq(distribution=qunif) +
  facet_grid(~voice.part)

ggplot(singer, aes(sample=height)) + stat_qq(distribution=qunif) +
  facet_wrap(~voice.part, ncol=2)


# Constructing a uniform QQ plot manually
Tenor1 = sort(singer$height[singer$voice.part=="Tenor 1"])
nTenor1 = length(Tenor1)
f.value = (0.5:(nTenor1 - 0.5)) / nTenor1
Tenor1.df = data.frame(f.value, height=Tenor1)
ggplot(Tenor1.df, aes(x=f.value, y=height)) +
  geom_line() + geom_point()


# Two-sample QQ plots
Tenor1 = singer$height[singer$voice.part=="Tenor 1"]
Bass2 = singer$height[singer$voice.part=="Bass 2"]
qqplot(Tenor1, Bass2)
abline(0, 1)

# Tukey Mean-difference
# Humans can more accurately compare to a horizontal line than a diagonal one. 
Tenor1 = singer$height[singer$voice.part=="Tenor 1"]
Bass2 = singer$height[singer$voice.part=="Bass 2"]
qq.df = as.data.frame(qqplot(Tenor1, Bass2, plot.it=FALSE))
ggplot(qq.df, aes(x=x, y=y)) + geom_point() +
  geom_abline()

ggplot(qq.df, aes(x=(x+y)/2, y=y-x)) + geom_point() +
  geom_abline(slope=0)

# Pairwise QQ plots
singer.q.rows = aggregate(height ~ voice.part, quantile, probs=seq(.05, .95, .01), data=singer)
singer.q = t(singer.q.rows[-1])
names(singer.q) = singer.q.rows[,1]
singer.panel = function(x, y){
  lines(x, y, xlim=range(singer$height), ylim=range(singer$height))
  abline(0, 1)
}
ggpairs(singer.q, upper=c())

# External Code
require(datasets)
data("swiss")

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
g = ggpairs(swiss,columns = 1:4, lower = list(continuous = my_fn))


# Normality revisited
ggplot(singer[singer$voice.part=="Alto 1",], aes(sample=height)) + stat_qq(distribution=qunif)
ggplot(singer[singer$voice.part=="Alto 1",], aes(sample=height)) + stat_qq()


ggplot(singer, aes(sample = height)) +
  stat_qq() + facet_wrap(~voice.part, ncol=2) 