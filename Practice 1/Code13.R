#  Four  variables - (one response, three explanatory)
#-> Basically the sameas trivariate data, except you condition/ facet on two variables
#-> There are now multiple possible interactions, look carefully to see if they're justified

#  Two Plotting philosophy
#-> Start with something simple and add complexity if you need it
#-> Start with something complex and simplify if you can justify it. 

#  (neither is wrong but these approaches usually lead to different answers)
#-------------------------------------------------------------------------------- 

# Three way interactions are difficult to visualize
# Try searching for patterns by searching for patterns 
# Here we will start with complex approach as we can visualize most of the pattern in 4 variable

#--------------------------------------------------------------------------------


#GGpair
# descent correlation with ozone, other variables are not that correlated with each other
# Ozone is right skewed, we may want to normalize it


# two way Faceting
# - fitting and cutting with two variables

# Binning and converting into categorical variable
# High span loess
# Family symmetric for outliers
# In general we have positive slopes
# nothing looks that it requires a curve

# Differnces in facet plots
# - levels change, they increase
# - can't say much about interaction

# Facet with fit for wind
# - decreasing relationship but different slopes
# - W

# Facet with fit for templerature
# - increasing relationship but different slopes

# IMP - fit model with all the three variable



#------------------------------------------------

load("lattice.RData")
library(ggplot2)
library(GGally)
library(broom)
library(tidyr)

ggpairs(environmental)

ggplot(environmental, aes(x=radiation, y=ozone^(1/3))) + geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric"), se=FALSE) + 
  facet_wrap(~cut_number(wind, n=3) + cut_number(temperature, n=3))


CubeRootOzone = environmental$ozone^(1/3)
radiation = environmental$radiation
temperature = environmental$temperature
wind = environmental$wind
radiation.cat = rep(NA, nrow(environmental))
radiation.cat[radiation <= quantile(radiation, 1/3)] = "Low radiation"
radiation.cat[radiation > quantile(radiation, 1/3) & radiation <= quantile(radiation, 2/3)] = "Medium radiation"
radiation.cat[radiation > quantile(radiation, 2/3)] = "High radiation"
radiation.cat = factor(radiation.cat, levels=c("Low radiation", "Medium radiation", "High radiation"))
temperature.cat = rep(NA, nrow(environmental))
temperature.cat[temperature <= quantile(temperature, 1/3)] = "Low temperature"
temperature.cat[temperature > quantile(temperature, 1/3) & temperature <= quantile(temperature, 2/3)] = "Medium temperature"
temperature.cat[temperature > quantile(temperature, 2/3)] = "High temperature"
temperature.cat = factor(temperature.cat, levels=c("Low temperature", "Medium temperature", "High temperature"))
wind.cat = rep(NA, nrow(environmental))
wind.cat[wind <= quantile(wind, 1/3)] = "Low wind"
wind.cat[wind > quantile(wind, 1/3) & wind <= quantile(wind, 2/3)] = "Medium wind"
wind.cat[wind > quantile(wind, 2/3)] = "High wind"
wind.cat = factor(wind.cat, levels=c("Low wind", "Medium wind", "High wind"))
environmental.cat = data.frame(environmental, CubeRootOzone, radiation.cat, temperature.cat, wind.cat)

ggplot(environmental.cat, aes(x=radiation, y=CubeRootOzone)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric"), se=FALSE) + 
  facet_wrap(~wind.cat + temperature.cat)



ggplot(environmental.cat, aes(x=wind, y=CubeRootOzone)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric"), se=FALSE) + 
  facet_wrap(~temperature.cat + radiation.cat)


ggplot(environmental.cat, aes(x=temperature, y=CubeRootOzone)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric"), se=FALSE) + 
  facet_wrap(~wind.cat + radiation.cat)


ggplot(environmental, aes(x=wind, y=CubeRootOzone)) + 
  geom_point() + 
  geom_smooth(span=2/3, method.args=list(degree=1))

ggplot(environmental.cat, aes(x=temperature, y=radiation)) + 
  geom_point()

ggplot(environmental.cat, aes(x=wind, y=temperature)) + geom_point()

ggplot(environmental.cat, aes(x=wind, y=temperature)) + geom_point() + 
  geom_abline(intercept = 112, slope = -2, color = "blue") + 
  geom_abline(intercept = 87, slope = -2, color = "blue")


# Cropping out the diagonal corners
crop = (wind > 4) & (wind < 16) & (temperature < (-2 * wind + 112)) & (temperature > (-2 * wind + 87)) & (temperature > 60) & (temperature < 92)
environmental.crop = environmental.cat[crop,]


ggpairs(environmental.crop, columns = 2:4)

# NOTE - We can't remove the three way interaction using this model
environmental.lo = loess(CubeRootOzone ~ radiation + temperature + wind + radiation:temperature + radiation:wind + temperature:wind, span = 1)

environmental.lo2 = loess(CubeRootOzone ~ radiation*temperature*wind, span = 1)
summary(environmental.lo2)


loess.grid = expand.grid(radiation = min(radiation):max(radiation), temperature = c(60, 70, 80, 90), wind = c(4, 8, 12, 16))
environmental.predict = predict(environmental.lo, newdata=loess.grid)
environmental.df = data.frame(loess.grid, fit = as.vector(environmental.predict))
crop.grid = (loess.grid$temperature < (-2 * loess.grid$wind + 112)) & 
  (loess.grid$temperature > (-2 * loess.grid$wind + 87))
environmental.df = environmental.df[crop.grid,]

# Curve changes with higher values of wind
# We probably need the interaction with Radiation
# Two way facetting on wind and temperature
# Wind + temperature -> wind (top to bottom variable) , temperature (left to right variable)
ggplot(environmental.df, aes(x = radiation, y = fit)) + 
  geom_line() + 
  facet_wrap(~wind + temperature, drop = FALSE) + 
  labs(title = "Left to right: Increasing temperature; Top to bottom: Increasing wind")

# Grid is dense in radition and not so much on temperature
wind.grid = expand.grid(radiation = seq(50, 300, 50), 
                        temperature = c(60, 70, 80, 90), wind = seq(4, 16, 0.1))
environmental.predict = predict(environmental.lo, newdata=wind.grid)

#-------------------------------------------------------------
# Changing the main explanatory (in this case wind)
environmental.df = data.frame(wind.grid, fit = as.vector(environmental.predict))
wind.crop = (wind.grid$temperature < (-2 * wind.grid$wind + 112)) & 
  (wind.grid$temperature > (-2 * wind.grid$wind + 87))
environmental.df = environmental.df[wind.crop,]

# The across comparison in this case is much better than top to bottom
# Increase in wind increases the left end of the curve more than the right end
ggplot(environmental.df, aes(x = wind, y = fit)) + 
  geom_line() + 
  facet_wrap(~temperature + radiation, drop = FALSE, ncol=6) + 
  labs(title = "Left to right: Increasing radiation; Top to bottom: Increasing temperature")

#-------------------------------------------------------------

temperature.grid = expand.grid(radiation = seq(50, 300, 50), temperature = 61:92, wind = c(4, 8, 12, 16))
environmental.predict = predict(environmental.lo, newdata=temperature.grid)
environmental.df = data.frame(temperature.grid, fit = as.vector(environmental.predict))
temperature.crop = (temperature.grid$temperature < (-2 * temperature.grid$wind + 112)) & (temperature.grid$temperature > (-2 * temperature.grid$wind + 87))
environmental.df = environmental.df[temperature.crop,]

# The curves in the plots show that loess is a better fit than linear
# Shape remians the same across
# curves are shift upwards across
# No concrete proof of interaction
ggplot(environmental.df, aes(x = temperature, y = fit)) + 
  geom_line() + 
  facet_wrap(~wind + radiation, drop = FALSE, ncol=6) + 
  labs(title = "Left to right: Increasing radiation; Top to bottom: Increasing wind")

# Collapsing the column into a single column
# Group instead of faceting
# If there is interaction the curvature of the lines in each group should change
# Here interaction is present but its effect is small
# We can go back to contour plots instead of this approach
ggplot(environmental.df, aes(x = temperature, y = fit, group=radiation, color=radiation)) + 
  geom_line() + 
  facet_wrap(~wind, drop = FALSE, ncol=1) + 
  labs(title = "Top to bottom: Increasing wind")

# Check the fit with residual to check model
# The model is close to homoskedatic, primarily due to the cube root transformation
environmental.lo.broom = augment(environmental.lo)
ggplot(environmental.lo.broom, aes(x = .fitted, y = sqrt(abs(.resid)))) + 
  geom_point() + 
  geom_smooth(span = 1, method.args = list(degree = 1))

# Approximate normal residuals
# Least square was a correct choice
environmental.lo.broom = augment(environmental.lo)
ggplot(environmental.lo.broom, aes(sample = .resid)) + stat_qq()

# Fit captures moajority of the variation
# Residuals still has some variation
environmental.lo.broom$.fitted = environmental.lo.broom$.fitted - mean(environmental.lo.broom$.fitted)
environmental.lo.long = environmental.lo.broom %>% gather(component, value, c(.fitted, .resid))
ggplot(environmental.lo.long, aes(sample = value)) + 
  stat_qq(distribution = "qunif") + 
  facet_grid(~component)


#--------------------------------
# In conclusion the model is a decsent fit for the data