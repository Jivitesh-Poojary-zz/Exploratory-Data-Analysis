# NOTES
# 1. Seasonal components vary over a fixed time period, while oscillatory components 
#   vary over a varible time period. "Cyclic" is a vague term that could mean anything 
#   that goes up and down, whether seasonal or oscillatory.
# 2. We use the convention of fitting one loess per component (besides residuals.) So if 
#   we wish to fit seasonal, trend, and oscillatory components (in that order), we use 
#   loess three times.
# 3. The oscillatory component can be thought of as the remaining cyclic pattern after 
#   seasonal components have been removed.


load("lattice.RData")
library(ggplot2)
library(broom)
library(tidyr)

# ts object is a vector
fake.ts = ts(data = 1:7, start=2010)
fake.ts

# Conversion into a dataframe matrix
fake.df = data.frame(x=as.matrix(fake.ts), year=time(fake.ts))
fake.df

#-----------------------------------------
# Melanoma: Decomposing time series
#-----------------------------------------

ggplot(melanoma, aes(x=year, y=incidence)) + geom_point() + geom_line()

# This plot gives a trend
ggplot(melanoma, aes(x=year, y=incidence)) + geom_point() + geom_line() + geom_smooth(method.args=list(degree=1))

melanoma.lo = loess(incidence~year, degree=1, data=melanoma)
melanoma.lo.df = augment(melanoma.lo)
ggplot(melanoma.lo.df, aes(x=year, y=.resid)) + geom_point() + geom_line()

# The span values should be small
# This curves gives use an idea of the oscillatory component
# Sometimes there is an explanation of the oscillatory component sometimes it is just noice
ggplot(melanoma.lo.df, aes(x=year, y=.resid)) + geom_point() + geom_smooth(span=0.25)

# Normal
# As we see some regular pattern we should try to fit a Loess curve
melanoma.lo2 = loess(.resid~year, span=0.25, data=melanoma.lo.df)
melanoma.lo2.df = augment(melanoma.lo2)
names(melanoma.lo2.df)

# Finding the residuals
names(melanoma.lo2.df) = c(".resid","year",".fitted", ".se.fit",".resid2")
ggplot(melanoma.lo2.df, aes(x=year, y=.resid2)) + geom_point() + geom_line()

ggplot(melanoma.lo2.df, aes(sample=.resid2)) + stat_qq()

# Biquare
melanoma.lo2bi = loess(.resid~year, span=0.25, family="gaussian",data=melanoma.lo.df)
melanoma.lo2bi.df = augment(melanoma.lo2bi)
names(melanoma.lo2bi.df)

names(melanoma.lo2bi.df) = c(".resid","year",".fitted", ".se.fit",".resid2")
ggplot(melanoma.lo2bi.df, aes(x=year, y=.resid2)) + geom_point() + geom_line()

ggplot(melanoma.lo2bi.df, aes(sample=.resid2)) + stat_qq()


# Facet of the Trend, Oscillatory and residual components
Year = melanoma$year
Trend = melanoma.lo.df$.fitted - mean(melanoma.lo.df$.fitted)
Oscillatory = melanoma.lo2.df$.fitted
Residuals = melanoma.lo2.df$.resid
melanoma.ts = data.frame(Year, Trend, Oscillatory, Residuals)
melanoma.ts.long = melanoma.ts %>% gather(type, Incidence, Trend:Residuals)
melanoma.ts.long$type = factor(melanoma.ts.long$type, levels=c("Trend", "Oscillatory", "Residuals"))
ggplot(melanoma.ts.long, aes(x=Year, y=Incidence)) + geom_point() + geom_line() + facet_grid(~type)

var(Trend)
var(Oscillatory)
var(Residuals)

# Check for factors that can cause Melanoma
sunspot.ts = data.frame(Year, Oscillatory, sunspot)
sunspot.ts.long = sunspot.ts %>% gather(type, number, Oscillatory:sunspot)

ggplot(sunspot.ts.long, aes(x=Year, y=number)) + geom_line() + facet_wrap(~type, ncol=1, scales="free_y")



#-----------------------------------------
# Carbon dioxide: Seasonal time series
#-----------------------------------------

co2.df = data.frame(year=time(co2), co2)
ggplot(co2.df, aes(x=year, y=co2)) + geom_line()

# Stl breaks down the data into trend, oscillation and residual
# s.window controls seasonal smoothing
# s.window has to be an odd number and ideally should not be a small value
co2.stl = stl(co2, s.window=25, s.degree=1)
head(co2.stl$time.series)

# Selection of s.window values
plot(stl(co2, s.window=1, s.degree=1))
plot(stl(co2, s.window=3, s.degree=1))
#The sesonal pattern is still irregular hence we choose 25 above
plot(stl(co2, s.window=7, s.degree=1))
plot(stl(co2, s.window=100, s.degree=1))

# Periodic cycle in seasonal component
# We should try using to model the seasonal component without the periodic function
# However there are some usecases for the periodic function
plot(stl(co2, s.window="periodic", s.degree=1))


# Slicing the period 
year.cut = cut_number(time(co2), n=5)
co2.df2 = data.frame(year=time(co2), co2.stl$time.series, year.cut)
ggplot(co2.df2, aes(x=year, y=seasonal)) + geom_line() + facet_wrap(~year.cut, ncol=1, scales="free_x")
ggplot(co2.df2, aes(x=year, y=seasonal)) + geom_line() + facet_wrap(~year.cut, ncol=1, scales="fixed")

# Monthplot 
# Horizonatal line gives average value from year to year
# The curve gives us information regarding the variation of the co2 level year to year
monthplot(co2.stl, choice="seasonal")
monthplot(co2.stl, choice="trend")
monthplot(co2.stl, choice="remainder")

# Again gives us a variation of the seasonal component as it varies from month to month along with year to year
# Positive values become more positive , same is the case with the negative values
# Sesonal component increases in magnitude with evey year
co2.month = data.frame(co2.df2, month=factor(1:12))
ggplot(co2.month, aes(x=year, y=seasonal)) + geom_line() + facet_wrap(~month)

# This doesnot give more information
ggplot(co2.month, aes(x=year, y=trend)) + geom_line() + facet_wrap(~month)

# This plot gives us an idea abouth the trend
# Oscillation is removed in this case
ggplot(co2.month, aes(x=year, y=trend+remainder)) + geom_line()

# Reasonalable fit to the data
ggplot(co2.month, aes(x=year, y=trend+remainder)) + geom_line() + geom_smooth()

# First Loess fit to get the oscillatory component
co2.lo = loess(trend+remainder~year, data=co2.month)
co2.lo.df = augment(co2.lo)
# There is some trend that is still present
ggplot(co2.lo.df, aes(x=year, y=.resid)) + geom_line()

# Local Loess with only 10% of the data
ggplot(co2.lo.df, aes(x=year, y=.resid)) + geom_line() + geom_smooth(span=0.1)
ggplot(co2.lo.df, aes(x=year, y=.resid)) + geom_line() + geom_smooth(span=0.2)

# Check normality of residuals of First Loess fit
ggplot(co2.lo.df, aes(sample=.resid)) + stat_qq()

Year = co2.month$year

# Second Loess fit to remove the remaining oscillatory components
co2.lo2 = loess(residuals(co2.lo)~Year, span=0.1)
co2.lo2.df = augment(co2.lo2)

# There could be little autocorrelation
# However it looks more like random noise
ggplot(co2.lo2.df, aes(x=Year, y=.resid)) + geom_line() + geom_abline(slope=0)
ggplot(co2.lo2.df, aes(x=Year, y=.fitted)) + geom_line() + geom_abline(slope=0)

# Check normality of residuals of Second Loess fit
ggplot(co2.lo2.df, aes(sample=.resid)) + stat_qq()

# Here we have not considered the trend here
co2.ts = data.frame(Year, Seasonal=co2.month$seasonal, Oscillatory=co2.lo2.df$.fitted, Residuals=co2.lo2.df$.resid)
co2.ts.long = co2.ts %>% gather(type, co2.ppm, Seasonal:Residuals)

# Seasonal component has a greater effect once we take out the trend
ggplot(co2.ts.long, aes(x=Year, y=co2.ppm)) + geom_line() + facet_wrap(~type, ncol=1)
