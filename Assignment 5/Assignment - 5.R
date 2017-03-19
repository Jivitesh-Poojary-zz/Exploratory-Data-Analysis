library(ggplot2)
library(MASS)
library(tidyr)
library(stringr)
library(TeachingDemos)
library(lattice)

mb <- read.csv('movie_budgets.txt', header=TRUE)

movie_budgets <- data.frame(Title = character(), Year= numeric(),Length = numeric(),Budget = numeric(),LogBudget = numeric())

for(i in 1:nrow(mb)) {
  print(tail(strsplit(toString(mb[i,1]),split=" ")[[1]],3))
  len <- length(strsplit(toString(mb[i,1]),split=" ")[[1]])
  movie_budgets <- rbind(movie_budgets, data.frame(
    Title = str_c(head(strsplit(toString(mb[i,1]),split=" ")[[1]],len-3), collapse = " "),
    Year = strtoi(tail(strsplit(toString(mb[i,1]),split=" ")[[1]],3)[1]),
    Length = strtoi(tail(strsplit(toString(mb[i,1]),split=" ")[[1]],3)[2]),
    Budget = tail(strsplit(toString(mb[i,1]),split=" ")[[1]],3)[3],
    LogBudget = log10(strtoi(tail(strsplit(toString(mb[i,1]),split=" ")[[1]],3)[3]))
  ))
}

write.csv(movie_budgets, file = "MyData.csv")

#Test
mb2 <- read.csv('MyData.csv', header=TRUE)

# Simple plot
ggplot(movie_budgets, aes(x=LogBudget, y=Year)) + geom_point() + 
  geom_smooth() 

ggplot(movie_budgets, aes(x=LogBudget, y=Length)) + geom_point() + 
  geom_smooth() 

# Span  variation
ggplot(movie_budgets, aes(x=LogBudget, y=Year)) + geom_point() + 
  geom_smooth(span=1) 

ggplot(movie_budgets, aes(x=LogBudget, y=Length)) + geom_point() + 
  geom_smooth(span=1) 

# Robust fits
ggplot(movie_budgets, aes(x=LogBudget, y=Year)) + geom_point() + 
  geom_smooth(method="rlm") 

ggplot(movie_budgets, aes(x=LogBudget, y=Length)) + geom_point() + 
  geom_smooth(method="rlm") 

# Colored plots
ggplot(movie_budgets, aes(x=LogBudget, y=Year, group=Length, color=Length)) + geom_point() + 
  scale_color_distiller(palette="RdYlBu")

ggplot(movie_budgets, aes(x=LogBudget, y=Length, group=Year, color=Year)) + geom_point() + 
  scale_color_distiller(palette="RdYlBu") 

# Direct facet
ggplot(movie_budgets, aes(x=LogBudget, y=Year)) + geom_point() + 
  geom_smooth(method="lm") + 
  facet_wrap(~cut_number(Length, n=6), ncol=3) 

ggplot(movie_budgets, aes(x=LogBudget, y=Length)) + geom_point() + 
  geom_smooth(method="lm") + 
  facet_wrap(~cut_number(Year, n=6), ncol=3)

# Dropping columns
#drops <- c("Title","Budget")
#movie_budgets2 <- movie_budgets[ , !(names(movie_budgets) %in% drops)]

#-------------------------------------------      
# ANSWER - 1
#------------------------------------------- 
# Loess Fit with interaction
movie_budgets.lo = loess(LogBudget ~ Length + Year + Length * Year, data=movie_budgets, span=1/3, 
                         parametric="Length", drop.square="Length", family="symmetric")

summary(movie_budgets.lo)

library(broom)
movie_budgets.lo.df = augment(movie_budgets.lo)


#------------------------------------------- 
# ANSWER - 2
#------------------------------------------- 
# Plots of fit
ggplot(movie_budgets.lo.df, aes(x=LogBudget, y=.fitted)) + geom_point() + 
  geom_smooth(method="lm") + 
  facet_wrap(~cut_number(Length, n=6), ncol=3) 

ggplot(movie_budgets.lo.df, aes(x=LogBudget, y=.fitted)) + geom_point() + 
  geom_smooth(method="lm") + 
  facet_wrap(~cut_number(Year, n=6), ncol=3)


# Plots of residuals
ggplot(movie_budgets.lo.df, aes(x=Length, y=.resid)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

ggplot(movie_budgets.lo.df, aes(x=Year, y=.resid)) + 
  geom_point() + 
  geom_smooth(method.args=list(degree=1))

ggplot(movie_budgets.lo.df, aes(x=Length, y=.resid)) + 
  geom_point() + 
  geom_smooth(span=1, method.args=list(degree=1, family="symmetric"))

#------------------------------------------- 
# ANSWER - 3
#------------------------------------------- 
# Contour density
ggplot(movie_budgets.lo.df, aes(x=LogBudget, y=.fitted)) + 
  geom_point() + 
  stat_density_2d()

ggplot(movie_budgets.lo.df, aes(x=LogBudget, y=.fitted)) + 
  stat_density_2d(aes(fill=..level..), geom="polygon")

# Wire frame plots
range(movie_budgets$Year)
range(movie_budgets$Length)

movie_budgets.wf.grid = expand.grid(Length=seq(1,400,5), Year=seq(1905,2005,5))
movie_budgets.wf.predict = predict(movie_budgets.lo, newdata=movie_budgets.wf.grid)
movie_budgets.wf.df = data.frame(movie_budgets.wf.grid, fit=as.vector(movie_budgets.wf.predict))

wireframe(fit ~ Length * Year, data=movie_budgets.wf.df, screen = list(z=30, x=-60, y=0))

wireframe(fit ~ Length * Year, data=movie_budgets.wf.df, screen = list(z=120, x=-60, y=0))

wireframe(fit ~ Length * Year, data=movie_budgets.wf.df, screen = list(z=210, x=-60, y=0))

wireframe(fit ~ Length * Year, data=movie_budgets.wf.df, screen = list(z=300, x=-60, y=0))


