load("lattice.RData")
library(ggplot2)
# install.packages("GGally")
library(GGally)
library(dplyr)
library(tidyverse)
library(MASS)

# This function is good to compare pairwise comparison of variables
# GGally library is used
# We are specifying columns as we not considering all the columns
# The diagonal gives density plots
# Below the diagonal we have a scatter plot
# Correlation is reasonable summary of the relationship
# Each graph however gives only a bivariate picture 
ggpairs(rubber, columns = c("hardness", "tensile.strength", "abrasion.loss"))

# The values used for splitting has to tried and tested iteratively
hard = rep(NA, nrow(rubber))
hard[rubber$hardness > 62] = "Hard"
hard[rubber$hardness <= 62] = "Soft"
rubber2 = data.frame(rubber, hard)

# This plot gives proof of a trivariate relationship
ggpairs(rubber2, columns = 1:3, aes(colour = hard))

#---------------------------------
# Conditional plotting / Co-plotting
# - draw scatter plots and facet it with the third variable
#---------------------------------

# We are considering the abrasion loss as our explainatory variable
# cut_number helps in making the numerical variable hardness into a categorical variable
# We have used degree 1 as there is less data
# Confirms that sodt rubber has more abrasion loss while hard rubber had less abrasion loss
# Hard rubber shows some different relationship
# One thing to be noted is that after tensile.str of 180 the curve flatens
ggplot(rubber, aes(x=tensile.strength, y=abrasion.loss)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1)) + 
  facet_grid(~cut_number(hardness, n=3)) + 
  labs(title="Rubber data split by hardness")

ggplot(rubber, aes(x=tensile.strength, y=abrasion.loss)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1)) + 
  facet_grid(~cut_number(hardness, n=4)) + 
  labs(title="Rubber data split by hardness")

# We are using span as 1 as less data requires more smoothing
# Linear model wont't work well in this case
# If we use span as default we get an error which disappears in this case
ggplot(rubber, aes(x=tensile.strength, y=abrasion.loss)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1)) + 
  facet_grid(~cut_number(hardness, n=4)) + 
  labs(title="Rubber data split by hardness")

# This graph gives some misleading result with left and center plot,the axis are different
ggplot(rubber, aes(x=hardness, y=abrasion.loss)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1)) + 
  facet_grid(~cut_number(tensile.strength, n=3)) +
  labs(title="Rubber data split by tensile strength")

# Confirms a generally decreasing relationsip
# Loess won't work well as we try splitting the data into larger cuts
ggplot(rubber, aes(x=hardness, y=abrasion.loss)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1)) + 
  facet_grid(~cut_number(tensile.strength, n=4)) +
  labs(title="Rubber data split by tensile strength")

# Linear model works well with large number of cuts
ggplot(rubber, aes(x=hardness, y=abrasion.loss)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  facet_grid(~cut_number(tensile.strength, n=5))

ggplot(rubber, aes(x=hardness, y=abrasion.loss)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  facet_grid(~cut_number(tensile.strength, n=6))

## Similar plots and slopes suggest some other
# - interaction can be tried we have less data
# - we can try using rlm

# - There's no obvious need for interaction terms. 
# - If the coplots had lines with very different slopes, we'd have to strongly consider interaction terms.

#----------------------
# PART TWO
#----------------------

ts.low = function(x){
  return((x - 180) * (x < 180))
}

rubber.rlm = rlm(abrasion.loss ~ hardness + ts.low(tensile.strength), data=rubber, psi=psi.bisquare)

rubber.grid = expand.grid(hardness=c(54,64,74,84), tensile.strength=c(144,162,180,198))
rubber.predict = predict(rubber.rlm, newdata=rubber.grid)

ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict)), 
       aes(x=tensile.strength, y=fit)) + 
  geom_line() + 
  facet_grid(~hardness) + 
  labs(title="Abrasion loss fit conditional on hardness")

ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict)), 
       aes(x=tensile.strength, y=fit, group=hardness, color=factor(hardness))) + 
  geom_line()

# The plots for tensile strength above 180 are same
ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict)), aes(x=hardness, y=fit)) + 
  geom_line() + 
  facet_grid(~tensile.strength) + 
  labs(title="Abrasion loss fit conditional on tensile strength")

# The slopes of the lines appear identical
# The last two lines overlap on each other as they are identical
ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict)), 
       aes(x=hardness, y=fit, group=tensile.strength, color=factor(tensile.strength))) + 
  geom_line()

summary(rubber.rlm)

#----------------------
# Exploring the residuals
#----------------------

rubber.rlm2 = rlm(abrasion.loss ~ hardness + ts.low, data=rubber, psi=psi.bisquare)

tensile.strength = rubber$tensile.strength
tensile.cat = cut_number(tensile.strength, n=3)
hardness = rubber$hardness
hard.cat = cut_number(hardness, n=3)
residual.loss = residuals(rubber.rlm2)
rubber.rlm2.df = data.frame(tensile.strength, tensile.cat, hardness, hard.cat, residual.loss)

# Plot is not exactly a straight line
# However it does not have any sharp edges
# We can attribute the curvature to an outlier in the lower left corner
ggplot(rubber.rlm2.df, aes(x=tensile.strength, y=residual.loss)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric")) + 
  geom_abline(slope=0)

# Plot is not exactly a straight line
# However it does not have any sharp edges
# We can attribute the curvature to an outlier in the lower right corner
ggplot(rubber.rlm2.df, aes(x=hardness, y=residual.loss)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric")) + 
  geom_abline(slope=0)

# There is something wierd in the third subplot
# To some extent we can attribute it to the outlier present the lower left corner
ggplot(rubber.rlm2.df, aes(x=tensile.strength, y=residual.loss)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric")) + 
  geom_abline(slope=0) + 
  facet_grid(~hard.cat) + 
  labs(title="Abrasion loss residuals split by hardness")

ggplot(rubber.rlm2.df, aes(x=hardness, y=residual.loss)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric")) + 
  geom_abline(slope=0) + 
  facet_grid(~tensile.cat) + 
  labs(title="Abrasion loss residuals split by tensile strength")


#----------------------
# Improving the fit
#----------------------

rubber.no.outliers = rubber[rubber$tensile.strength>130,]
rubber.rlm3 = rlm(abrasion.loss ~ hardness * ts.low, data = rubber.no.outliers, psi=psi.bisquare)
rubber.rlm3.df = data.frame(
  tensile.strength = rubber.no.outliers$tensile.strength,
  tensile.cat = cut_number(rubber.no.outliers$tensile.strength, n=3),
  hardness = rubber.no.outliers$hardness,
  hard.cat = cut_number(rubber.no.outliers$hardness, n=3),
  fitted.loss = fitted.values(rubber.rlm3),
  fit.demeaned = fitted.values(rubber.rlm3) - mean(fitted.values(rubber.rlm3)),
  residual.loss = residuals(rubber.rlm3))

ggplot(rubber.rlm3.df, aes(x=tensile.strength, y=residual.loss)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric")) + 
  geom_abline(slope=0) + 
  facet_grid(~hard.cat) + 
  labs(title="Abrasion loss residuals split by hardness")

ggplot(rubber.rlm3.df, aes(x=hardness, y=residual.loss)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric")) + 
  geom_abline(slope=0) + 
  facet_grid(~tensile.cat) + 
  labs(title="Abrasion loss residuals split by tensile strength")

summary(rubber.rlm3)

rubber.grid = data.frame(rubber.grid, ts.low = ts.low(rubber.grid$tensile.strength))
rubber.predict3 = predict(rubber.rlm3, newdata=rubber.grid)

ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict3)), aes(x=tensile.strength, y=fit)) + 
  geom_line() + 
  facet_grid(~hardness) + 
  labs(title="Abrasion loss model condtional on hardness")

ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict3)), 
       aes(x=tensile.strength, y=fit, group=hardness, color=factor(hardness))) + 
  geom_line()

ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict3)), aes(x=hardness, y=fit)) + 
  geom_line() + 
  facet_grid(~tensile.strength) + 
  labs(title="Abrasion loss model condtional on tensile strength")

ggplot(data.frame(rubber.grid, fit=as.vector(rubber.predict3)), 
       aes(x=hardness, y=fit, group=tensile.strength, color=factor(tensile.strength))) + 
  geom_line()


#----------------------
# Checking the residuals
#---------------------
ggplot(rubber.rlm3.df, aes(x=fitted.loss, y=sqrt(abs(residual.loss)))) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

#TEST
ggplot(rubber.rlm3.df, aes(x=fitted.loss, y=residual.loss)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

ggplot(rubber.rlm3.df, aes(sample=residual.loss)) + stat_qq()

rubber.rlm3.long = rubber.rlm3.df %>% gather(component, abrasion.loss, c(fit.demeaned, residual.loss))

ggplot(rubber.rlm3.long, aes(sample=abrasion.loss)) + 
  stat_qq(distribution="qunif") + 
  facet_grid(~component)