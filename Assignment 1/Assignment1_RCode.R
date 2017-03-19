library(dplyr)
library(lattice)
library(ggplot2)

# Answer 1
tipData <- read.table("C:/Users/jpoojary/Documents/tips.txt", header=TRUE)
tipData$tip_Perc <- (tipData$tip * 100)/tipData$total_bill

ggplot(tipData, aes(sample=tip_Perc)) + stat_qq()
ggplot(tipData, aes(sample=tip_Perc)) + stat_qq(distribution=qunif)

# Answer 2
tipData.means = aggregate(tip_Perc~size, FUN=mean, data=tipData)

ggplot(tipData, aes(sample=tip_Perc)) + stat_qq(distribution=qunif) + 
  facet_wrap(~size, ncol=2)

# Answer 3
lmfit = lm(formula = tipData$tip_Perc ~ tipData$size)

f.value = tipData$tip_Perc - (18.4375 - 0.9173*tipData$size)
tipResidual.df = data.frame(f.value, tipData$tip_Perc)
ggplot(tipResidual.df, aes(x=f.value, y=tipData$tip_Perc)) +
  geom_line() + geom_point()
###########################################################

tipData.lm = lm(formula = tipData$tip_Perc ~ tipData$size)
tipData.res = data.frame(size=tipData$size, residual=residuals(tipData.lm))

ggplot(tipData.res, aes(x=size, y=residual)) + geom_boxplot()

ggplot(tipData.res, aes(sample = residual)) +
  stat_qq() + facet_wrap(~size, ncol=2) 

ggplot(tipData.res, aes(x = residual)) +
  stat_ecdf()

ggplot(tipData.res, aes(sample = residual)) +
  stat_qq()

ggplot(tipData.res, aes(sample = residual)) +
  stat_qq() + geom_abline(intercept=0, slope=summary(tipData.lm)$sigma)
