# Counter plots - Trivariate
# Two dimentional density plots

load("lattice.RData")
summary(galaxy)

library(ggplot2)
library(MASS)
library(tidyr)

# The directions are from a perspective of a person lying horizontally and facing the sky
ggplot(galaxy, aes(x=east.west, y=north.south)) + 
  geom_point() + 
  coord_fixed()

# Highest velocity at bottom right corner
# Lowest velocity at top left corner
ggplot(galaxy, aes(x=east.west, y=north.south)) + 
  geom_point() + 
  geom_jitter(width=0.5, height=0.5) + 
  facet_wrap(~cut_number(velocity, n=12), ncol=6) + 
  coord_fixed() + 
  labs(title="Galaxy locations split by velocity")

# Plots using polar co-ordinates instead of cartesian co-ordinates
# The edges of 102.5 are dense and hence we cannot say anything confidently
# To confirm the shape try fitting a Loess model
ggplot(galaxy, aes(x=radial.position, y=velocity)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~angle, ncol=4) + 
  labs(title="Galaxy position and velocity split by slit angle")


#------------------------------
# Modeling galaxy velocities
#------------------------------

# normalize here is FALSE, the scales are arbitary and thus we don't want to normalize
galaxy.lo = loess(velocity ~ east.west * north.south, data = galaxy, span = 0.25, family="symmetric", normalize=FALSE)

galaxy.lo.df = data.frame(galaxy,
                          .fitted = fitted.values(galaxy.lo),
                          .resid = residuals(galaxy.lo))

ggplot(galaxy.lo.df, aes(x=radial.position, y=velocity)) + 
  geom_point() + 
  geom_line(aes(x=radial.position, y=.fitted), color="blue") + 
  facet_wrap(~angle, ncol=4) + 
  labs(title="Galaxy position and velocity split by slit angle with fitted model")

# For most cases it appears that we have underfitted the data due to span= 1
# 
ggplot(galaxy.lo.df, aes(x=radial.position, y=.resid)) + 
  geom_point() + 
  geom_smooth(span=1) + 
  facet_wrap(~angle, ncol=4) + 
  labs(title="Galaxy position and residuals split by slit angle with fitted model")

# To test the homoscedaticity
# If homoscedatistic the line should be straight
# In this case there is minor heteroscedaticity
ggplot(galaxy.lo.df, aes(x=.fitted, y=sqrt(abs(.resid)))) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

# Not normal, we should not use usual methods
ggplot(galaxy.lo.df, aes(sample=.resid)) + stat_qq()

galaxy.fit = galaxy.lo.df$.fitted - mean(galaxy.lo.df$.fitted)
galaxy.resid = galaxy.lo.df$.resid

# Fitted values are more spreadout than residuals
# Model explains most of the variation
galaxy.lo.long = data.frame(galaxy.fit, galaxy.resid) %>% gather(component, velocity)
ggplot(galaxy.lo.long, aes(sample=velocity)) + 
  stat_qq(distribution="qunif") + 
  facet_grid(~component)

#------------------------------
# Visualizing the fit: geom_raster and geom_contour
#------------------------------

galaxy.grid = expand.grid(east.west=seq(-25,25,0.5), north.south=seq(-45,45,0.5))
galaxy.predict = predict(galaxy.lo, newdata=galaxy.grid)
galaxy.plot.df = data.frame(galaxy.grid, fit=as.vector(galaxy.predict))

# Raster graphics is used here
# Ratser fills in the missing pieces in the grid points
# Downsides - The intermediate color hues are not distinguishable
ggplot(galaxy.plot.df, aes(x=east.west, y=north.south, fill=fit)) + 
  geom_raster() + 
  coord_fixed() + 
  scale_fill_distiller(palette="RdYlBu")

# The maximum velocity is not most extreme region in red
ggplot(galaxy.plot.df, aes(x=east.west, y=north.south, fill=fit)) + 
  geom_raster() + 
  coord_fixed() + 
  scale_fill_distiller(palette="RdYlBu") + 
  facet_wrap(~cut_number(fit, n=16), ncol=4)

# Contour
# geom_contour uses the 'z' value
ggplot(galaxy.plot.df, aes(x=east.west, y=north.south, z=fit)) + 
  geom_raster(aes(fill = fit)) + 
  coord_fixed() + 
  scale_fill_distiller(palette="RdYlBu") + 
  geom_contour()

# The downside is that more countours have to be drawn to differential the regions
ggplot(data.frame(galaxy.grid, fit=as.vector(galaxy.predict)), aes(x=east.west, y=north.south, z=fit)) + 
  geom_contour(binwidth=10) + 
  coord_fixed()

# ..level.. -> value not in dataframe but has to be computed
# Color is picked as per the level of velocity
# Smaller values of binwidth makes the plot unreadable
# It again has to tried repeatedly for getting a good value
ggplot(data.frame(galaxy.grid, fit=as.vector(galaxy.predict)), aes(x=east.west, y=north.south, z=fit)) + 
  geom_contour(binwidth=10, aes(color=..level..)) + 
  coord_fixed()

#------------------------------
# Contour plots for bivariate densities
#------------------------------

# Plot gives three modes for a dataset
ggplot(geyser, aes(x=waiting, y=duration)) + 
  geom_point() + 
  stat_density_2d()

# The color gives us clue of the size of the regions
ggplot(geyser, aes(x=waiting, y=duration)) + 
  stat_density_2d(aes(fill=..level..), geom="polygon")

# Here we are switching the axis
# We are also mis aligning the columns by one record
# This is done as we can associate a waiting time for the next duartion
n = nrow(geyser)
next.geyser = data.frame(duration = geyser$duration[-n], waiting = geyser$waiting[-1])
ggplot(next.geyser, aes(x=duration, y=waiting)) + 
  stat_density_2d(aes(fill=..level..), geom="polygon")

# Creating a categorical variable 
# If there is tilt we should try duartion based regression model
duration.cat = rep(NA, nrow(next.geyser))
duration.cat[next.geyser$duration<=3] = "Short eruption"
duration.cat[next.geyser$duration>3] = "Long eruption"
duration.cat = factor(duration.cat, levels=c("Short eruption", "Long eruption"))

# We can see that short eruption was 
ggplot(data.frame(next.geyser, duration.cat), aes(x=waiting, fill=duration.cat, color=duration.cat)) + 
  stat_density() + 
  facet_wrap(~duration.cat, ncol=1)