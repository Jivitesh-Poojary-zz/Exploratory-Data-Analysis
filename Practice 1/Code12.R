# 3D points are better at getting a general shape
# We don't get more information by numbering the axes

# Contours are great for a dense grid of points
# Wireframes are goodfor sparse grid of points

load("lattice.RData")

# Library for 3D data
library(lattice)
# This is similar to a scatter plot
cloud(velocity ~ east.west * north.south, data=galaxy)

# Loess model with interaction
galaxy.lo = loess(velocity ~ east.west * north.south, data = galaxy, span = 0.25, family="symmetric", normalize=FALSE)

# We have kept enough distance between points
galaxy.wf.grid = expand.grid(east.west=seq(-25,25,2), north.south=seq(-45,45,2))
galaxy.wf.predict = predict(galaxy.lo, newdata=galaxy.wf.grid)

# Vector was used to fit it in the dataframe
galaxy.wf.df = data.frame(galaxy.wf.grid, fit=as.vector(galaxy.wf.predict))

wireframe(fit ~ east.west * north.south, data=galaxy.wf.df)

# install.packages("TeachingDemos")
# Z slider is most important
library(TeachingDemos)
rotate.wireframe(fit ~ east.west * north.south, data=galaxy.wf.df)

# screen argument is used for rotation, it is a list
# Rotating the frame and viewing it from different angles gives more information on the curve
wireframe(fit ~ east.west * north.south, data=galaxy.wf.df, screen = list(z=30, x=-60, y=0))

wireframe(fit ~ east.west * north.south, data=galaxy.wf.df, screen = list(z=120, x=-60, y=0))

wireframe(fit ~ east.west * north.south, data=galaxy.wf.df, screen = list(z=210, x=-60, y=0))

wireframe(fit ~ east.west * north.south, data=galaxy.wf.df, screen = list(z=300, x=-60, y=0))

# For adding color we use drape
wireframe(fit ~ east.west * north.south, data=galaxy.wf.df, screen = list(z=120, x=-60, y=0), drape=TRUE)



#--------------
# Ethanol data
#--------------

# This view appear like a scatter plot
cloud(NOx ~ C * E, data=ethanol, screen = list(z=90, x=-90, y=0))

cloud(NOx ~ C * E, data=ethanol, screen = list(z=90, x=-90, y=0), groups=C)

ethanol.lo = loess(NOx ~ C * E, data=ethanol, span=1/3, parametric="C", drop.square="C", family="symmetric")
ethanol.grid = expand.grid(C=c(7.5,9,12,15,18), E=seq(0.6, 1.2, 0.1))
ethanol.predict = predict(ethanol.lo, newdata=ethanol.grid)
ethanol.df = data.frame(ethanol.grid, fit=as.vector(ethanol.predict))
wireframe(fit ~ C * E, data=ethanol.df, screen=list(z=90, x=-90, y=0))

# Viewing from a frontline angle tells that C may not be contributing enough
# This is very subjective as we may have to check different angles
# We should keep one of these tunnel plot as 
wireframe(fit ~ C * E, data=ethanol.df, screen=list(z=92, x=-97, y=0))

# Viewing from a side angle tells that C is contributing and  the curve is increasing
wireframe(fit ~ C * E, data=ethanol.df, screen=list(z=30, x=-60, y=0))


#--------------
# Soil
#--------------
library(ggplot2)

# Response variable - Resistivity of the soil (saltness)
# Explanatory variables - Locations (northing, easting), is.ns (is it a north south track)
ggplot(soil, aes(x=easting, y=northing)) + 
  geom_point() + 
  coord_fixed()

# Plot for north south track
# The pattern is hard to work out due to the zig zag nature
ggplot(subset(soil, is.ns == TRUE), aes(x=northing, y=resistivity)) + 
  geom_point() + 
  facet_wrap(~track, ncol=4)

# Plot for east west track
# ncol 4 is a bad choice as the plot is very compressed
# Here we cannot say anything definetly, thus a complex modelmay be required here
ggplot(subset(soil, is.ns == FALSE), aes(x=easting, y=resistivity)) + 
  geom_point(size=0.5) + 
  facet_wrap(~track, ncol=8)

# Loess model with interaction
soil.lo = loess(resistivity ~ easting * northing, span = 0.25, data=soil)

soil.grid = expand.grid(easting = seq(0, 1.5, 0.01), northing = seq(0, 3.5, 0.01))
soil.predict = predict(soil.lo, newdata=soil.grid)
soil.df = data.frame(soil.grid, fit=as.vector(soil.predict))

# there is a complex additive fit here or other non-parametric model
ggplot(soil.df, aes(x=easting, y=northing, z=fit, fill=fit)) + 
  geom_raster() + 
  geom_contour(binwidth=10, color="black") + 
  scale_fill_distiller(palette="RdBu") + 
  coord_fixed()

# install.packages("plot3D")
library(plot3D)
east.grid = seq(0, 1.5, 0.01)
north.grid = seq(0, 3.5, 0.01)
mesh.grid = mesh(east.grid, north.grid)
fit.grid = matrix(soil.predict, nrow=length(east.grid))
surf3D(mesh.grid$x, mesh.grid$y, fit.grid, theta=0, col = ramp.col(col = c("blue", "red"), n = 10))
