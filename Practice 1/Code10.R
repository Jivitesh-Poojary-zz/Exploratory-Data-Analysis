load("lattice.RData")
library(ggplot2)
library(GGally)
ggpairs(ethanol)

ggplot(ethanol, aes(x=factor(C), y=NOx)) + geom_boxplot()

ggplot(ethanol, aes(x=factor(C), y=E)) + geom_boxplot()

# Curve explains most of the variation
# Thus we can fit a linear model without doing a transformation
ggplot(ethanol, aes(x=E, y=NOx)) + geom_point() + 
  geom_smooth() + 
  facet_wrap(~C, ncol=3) + 
  labs(title="Ethanol data split by compression")

# Differentiating the 5 curves
# If curves overlap than compression does not matter, [This is not the case]
# If curves shift up or down then interaction is not there
# Peaks are different for different values of C
# Higher compression results in higher NOx
# Left side of the graph shows that we may need an interaction as 
#     the curves cannot be transformed using a simple shift
# Probably underfitting as the span may be high
ggplot(ethanol, aes(x=E, y=NOx, group=C, color=C)) + 
  geom_point() + 
  geom_smooth(se=FALSE)

# The default color scale is not good as we can accurately differentiate the points
# Hence we override the default color scale
# There are some fixed palettes however we can make our custom palette
ggplot(ethanol, aes(x=C, y=NOx, group=E, color=E)) + 
  geom_point() + 
  scale_color_distiller(palette="RdYlBu")

# Adding Jitter
ggplot(ethanol, aes(x=C, y=NOx, group=E, color=E)) + 
  geom_point() + 
  geom_jitter() +
  scale_color_distiller(palette="RdYlBu")

# Loess Model
ggplot(ethanol, aes(x=C, y=NOx)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~cut_number(E, n=6), ncol=3) + 
  labs(title="Ethanol data split by equivalence ratio")

# We prefer this model as Loess as we may not include interaction and other non-linear
# This provided a reasonable result
# AS the slope changes with each graph which means there is some interaction happening
# Draw plot Loess in E, conditional in E and Linear in C along with a robust model, 
#     - Degree 2 in E, Linear in C, Span has to be tried 
ggplot(ethanol, aes(x=C, y=NOx)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  facet_wrap(~cut_number(E, n=6), ncol=3) + 
  labs(title="Ethanol data split by equivalence ratio")


# Loess model with interaction with E and C
# parametric - Linear model in C
# drop.square - not a quadratic ,fit as a linear
ethanol.lo = loess(NOx ~ C * E, data=ethanol, span=1/3, parametric="C", drop.square="C", family="symmetric")

# For visualizing the fit
ethanol.grid = expand.grid(C=c(7.5,9,12,15,18), E=seq(0.6, 1.2, 0.1))
ethanol.predict = predict(ethanol.lo, newdata=ethanol.grid)

# Slopes of the lines change as E increase
# The slope decrease from positive to negative with change in E
ggplot(data.frame(ethanol.grid, fit=as.vector(ethanol.predict)), aes(x=C, y=fit, group=E, color=E)) + 
  geom_line() + 
  scale_color_distiller(palette="RdYlBu")


ggplot(data.frame(ethanol.grid, fit=as.vector(ethanol.predict)), aes(x=C, y=fit, color=E)) + 
  geom_line() + 
  facet_grid(~E) + 
  scale_color_distiller(palette="RdYlBu") + 
  labs(title="NOx fit conditional on equivalence ratio")


ethanol.grid2 = expand.grid(C=c(7.5,9,12,15,18), E=seq(0.6, 1.2, 0.01))
ethanol.predict2 = predict(ethanol.lo, newdata=ethanol.grid2)

# Non monotonic relationhip + non - parametric
# Peaks are higher for larger values of C
ggplot(data.frame(ethanol.grid2, fit=as.vector(ethanol.predict2)), aes(x=E, y=fit, color=C)) + 
  geom_line() + 
  facet_grid(~C) + 
  labs(title="NOx fit conditional on compression ratio")


# For higher values of E the compression does not matter
# However for smaller values of E we have significantly different C values
ggplot(data.frame(ethanol.grid2, fit=as.vector(ethanol.predict2)), aes(x=E, y=fit, group=C, color=C)) + 
  geom_line()


library(broom)
ethanol.lo.df = augment(ethanol.lo)
ggplot(ethanol.lo.df, aes(x=E, y=.resid)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

# There are few outliers in the model
ggplot(ethanol.lo.df, aes(x=C, y=.resid)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

# First check if the plots are straight
# If not staright is there a pattern, we may have look for explanation for a pattern
# Do the lines have a similar slopes
# No consistant pattern - Interaction is doing a good job, The model is good
# Think twice before dropping an outlier 
ggplot(ethanol.lo.df, aes(x=E, y=.resid)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric")) + 
  facet_grid(~C)

# Test for homoscedaticity
# The line is resonably horizontal
# Curvature in this case could be random noice
ggplot(ethanol.lo.df, aes(x=.fitted, y=sqrt(abs(.resid)))) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

# Test for normality
# - Not normal
ggplot(ethanol.lo.df, aes(sample=.resid)) + stat_qq()

ethanol.fit = ethanol.lo.df$.fitted - mean(ethanol.lo.df$.fitted)
ethanol.resid = ethanol.lo.df$.resid
library(tidyr)
ethanol.lo.long = data.frame(ethanol.fit, ethanol.resid) %>% gather(component, NOx)
ggplot(ethanol.lo.long, aes(sample=NOx)) + 
  stat_qq(distribution="qunif") + 
  facet_grid(~component)
