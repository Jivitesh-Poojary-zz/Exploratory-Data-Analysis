library(dplyr)
library(lattice)
library(ggplot2)
library(tidyr)
library(broom)
library(MASS)

# Robust Linear Models - 
# When - Trend is a straight line but with outliers /assymmetry. Know there is a linear trend but cant use our usual methods
# What it does - upweights does close to trend, downweight far
# MASS library

#Loess:
#when non-linear trend
#What it does: fits a local polynomial at each x in R

ggplot(dating, aes(x=carbon, y=thorium-carbon)) + geom_point()

ggplot(dating, aes(x=carbon, y=thorium-carbon)) + geom_point() + geom_smooth(method="lm")

# For better judgment plot the residuals
# augment gives a nice data frame
# Outliers are present in the data which drag the regression line down
dating.lm = lm(thorium - carbon ~ carbon, data=dating)
dating.lm.df = augment(dating.lm)
ggplot(dating.lm.df, aes(x=carbon, y=.resid)) + geom_point() + geom_abline(slope=0)

# Loess does not go higher than the last point
# Outliers can be removed depending on a set rule not on personal judgment (subject)
ggplot(dating, aes(x=carbon, y=thorium-carbon)) + geom_point() + geom_smooth()

# Downweight a far away point from the trend
# Upweight a near point from the trend
# Here the standard error is not shaded for rlm
# Lines have different slope
# It comes closer to describing the bulk of the data
# It may or may not converge
ggplot(dating, aes(x=carbon, y=thorium-carbon)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + geom_smooth(method="rlm", se=FALSE, col="orange")

# Tukey's bisquare method can be helpful when there are convergence problems, 
# as it downweights extreme outliers to have zero weight.
# Psi function to give weights to points
# Huber Psi function is the default
# Bisquare Psi is most resistant to outliers
# method.arg is used as we are using the 'rlm' method
ggplot(dating, aes(x=carbon, y=thorium-carbon)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  geom_smooth(method="rlm", se=FALSE, col="orange", method.args=list(psi=psi.bisquare))

# Fitting in the rlm model
# Syntax is same as lm with addition of type of Psi function
# Again the default is Huber
age.diff = dating$thorium - dating$carbon
carbon = dating$carbon
dating.rlm = rlm(age.diff ~ carbon, psi = psi.bisquare)
tidy(dating.rlm)


dating.rlm.df = augment(dating.rlm)
ggplot(dating.rlm.df, aes(x=carbon, y=.resid)) + geom_point() + geom_abline(slope=0)

# We do not get a straight line again
# complicated error distribution
# As we are using RLM we are not making any assumptions of normality
# We can say something about the trend but not about the distribution
ggplot(dating.rlm.df, aes(sample=.resid)) + stat_qq()



### ---------------------------------
### Particulates and the Babinet point
### ---------------------------------

ggplot(polarization, aes(x=concentration, y=babinet)) + geom_point()
n = nrow(polarization)
power = rep(c(1,1/3,0,-1/3), each=n)
concentration = polarization$concentration
conc.trans = c(concentration, concentration^(1/3), log(concentration), concentration^(-1/3))

# Decreasing relationship
# There is rounding done here so many points have similar x or y values
# Jitter can be used
ggplot(polarization, aes(x=concentration^(1/3), y=babinet)) + geom_point()
ggplot(data.frame(power, conc.trans), aes(sample=conc.trans)) + stat_qq() + 
  facet_wrap(~power, scales="free")

ggplot(polarization, aes(x=(concentration+runif(n, -0.5, 0.5))^(1/3), y=babinet+runif(n, -0.05, 0.05))) + 
  geom_point()

# From the plot we can conclude that a straight line may not be the best solution here
ggplot(polarization, aes(x=concentration^(1/3), y=babinet)) + geom_point() + 
  geom_jitter()

# LOESS - we are doing a local regression
# Select a point and plot a linear or quadratic fucntion for its near points
# Quadratic is default in R, for large data quadratic is usually better
# Family - Gaussian or symmtric fit
#   Gaussian -> Least squares (Default option)
#   Symmetric -> Resistant to outliers
# Span: a number that tells you the proportion os data that is considered to be in the neighbourhood of x values
#   default is 0.75
ggplot(polarization, aes(x=concentration^(1/3), y=babinet)) + geom_point() + 
  geom_smooth(method.args=list(degree=1)) + 
  geom_smooth(method.args=list(degree=2), col="orange") +
  geom_jitter()

cuberootconc = (polarization$concentration)^(1/3)
babinet = polarization$babinet
polar.lo = loess(babinet ~ cuberootconc, degree=1)
polar.lo.df = augment(polar.lo)
ggplot(polar.lo.df, aes(x=cuberootconc, y=.resid)) + geom_point() + 
  geom_smooth(method.args=list(degree=1)) + geom_abline(slope=0) + geom_jitter(height=0)

ggplot(polar.lo.df, aes(sample=.resid)) + stat_qq()


polar.lo.df$.fitted = polar.lo.df$.fitted - mean(polar.lo.df$.fitted)
polar.lo.df.long = polar.lo.df %>% gather(type, value, c(.fitted, .resid))
ggplot(polar.lo.df.long, aes(sample=value)) + stat_qq(distribution=qunif) + facet_grid(~type)

var(polar.lo.df$.fitted)
var(polar.lo.df$.resid)

ggplot(polar.lo.df, aes(x=cuberootconc, y=.resid)) +
  geom_boxplot(aes(group = cut_number(cuberootconc, n=15)))

ggplot(polar.lo.df, aes(x=cuberootconc, y=.resid)) +
  geom_boxplot(aes(group = cut(cuberootconc, breaks=seq(2, 5, 0.25))))