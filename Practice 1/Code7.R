load("lattice.RData")
library(ggplot2)
library(broom)


### ---------------------------------
### Fly eyes
### ---------------------------------

ggplot(fly, aes(x=temperature, y=facet)) + geom_point()

ggplot(fly, aes(x=temperature, y=facet)) + geom_point() + geom_jitter()

# This graph is messy
ggplot(fly, aes(x=temperature, y=facet)) + geom_point() + geom_jitter() + 
  geom_smooth(method = "lm")

# Decreasing trend
# Box 23 is lower than expected
# 
ggplot(fly, aes(x=factor(temperature), y=facet)) + geom_boxplot()

# QQ for 31 is showing departure form normality
# we can quantify by calculating the standard deviation
# We can assume homoscedasticity
ggplot(fly, aes(sample=facet)) + stat_qq() + facet_wrap(~temperature, ncol=3)

facet.means = aggregate(facet~temperature, mean, data=fly)
facet.lm = lm(facet~temperature, data=fly)

# There is non linearity between 23 and 25
# Linear form takes into consideration the errors in the y variable
# The variation in the data could be explained by varaiations in the x variable 
#   rather than non-linearity
# Real life modelling should also consider errors in x varaiables too
ggplot(facet.means, aes(x=temperature, y=facet)) + geom_point() + 
  geom_abline(intercept=facet.lm$coe[1], slope=facet.lm$coe[2])

facet.lm.df = augment(facet.lm)
facet.resid.means = aggregate(.resid~temperature, mean, data=facet.lm.df)

# Big residuals could be attributed to non constant temperatures
ggplot(facet.resid.means, aes(x=temperature, y=.resid)) + geom_point() + 
  geom_abline(slope=0)


### ---------------------------------
### Wind and temperature
### ---------------------------------

# On hotter days it is less windy while on colder days it is more windy
ggplot(environmental, aes(x=temperature, y=wind)) + geom_point() + geom_jitter()

ggplot(environmental, aes(sample=temperature)) + stat_qq()

ggplot(environmental, aes(sample=wind)) + stat_qq()

# TRULY BIVARIATE data - we should fit models for both the variables
# Its ok to have linear relationship in one form and non-linear in another
# Conditional expectation is not necessarily symmetric
ggplot(environmental, aes(x=temperature, y=wind)) + geom_point() + geom_jitter() + 
  geom_smooth(method.args=list(degree=1))

# The wind variable may require some transformation for obtaining a linear relationship
ggplot(environmental, aes(x=wind, y=temperature)) + geom_point() + geom_jitter() + 
  geom_smooth(method.args=list(degree=1))


## EXTRA
ggplot(environmental, aes(x=temperature, y=wind)) + geom_point() + geom_jitter() + 
  geom_smooth()

### ---------------------------------
### Ozone
### ---------------------------------


ggplot(ozone, aes(x=yonkers, y=stamford)) + 
  geom_point() + geom_abline() + coord_fixed()

ggplot(ozone, aes(x=(yonkers+stamford)/2, y=stamford-yonkers)) + 
  geom_point() + geom_abline(slope=0) + 
  geom_smooth(method.args=list(degree=1)) + coord_fixed()

# Take summary(log.ozone) to guess the base of the log
log.ozone = data.frame(log.yonkers = log2(ozone$yonkers), log.stamford = log2(ozone$stamford))

ggplot(log.ozone, aes(x=log.yonkers, y=log.stamford)) + 
  geom_point() + geom_abline() + coord_fixed()

# We can take it as a flat horizontal line and conclude of a mltiplicative shift
# Our conslusion will depend on the complexity of the solution we want
# However the curve is slightly increasing 
# If we want to do prediction we should assume horizontal random spread
ggplot(log.ozone, aes(x=(log.yonkers+log.stamford)/2, y=log.stamford-log.yonkers)) + 
  geom_point() + geom_abline(slope=0) + 
  geom_smooth(method.args=list(degree=1)) + coord_fixed()

# Get exact values by fitting a linear model

# Linear fit
ggplot(log.ozone, aes(x=(log.yonkers+log.stamford)/2, y=log.stamford-log.yonkers)) + 
  geom_point() + geom_abline(slope=0) + 
  geom_smooth(method = "lm") + coord_fixed()