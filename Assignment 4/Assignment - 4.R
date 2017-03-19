library(ggplot2)
library(broom)
library(tidyr)
library(GGally)

prcSurvey <- read.csv('Pew Research - Final2.csv', header=TRUE)

#----------------------
# Answer 1
#----------------------
prcSurvey <- read.csv('Pew Research - Final2.csv', header=TRUE)
prcSurvey$SumOfScore <- prcSurvey$Birthplace + prcSurvey$Language	+ 
  prcSurvey$Religion + prcSurvey$Customs
prcSurvey

n = length(prcSurvey)
f.value = (0.5:(n - 0.5)) / n
prcSurvey.long = prcSurvey %>% gather(type, value,Birthplace:SumOfScore)
ggplot(prcSurvey.long, aes(x=f.value, y=value)) +
  geom_point() + facet_wrap(~type)

#----------------------
# Answer 2
#----------------------
ggpairs(prcSurvey, columns = c("Birthplace","Language","Religion","Customs"))

#----------------------
# Answer 3
#----------------------
# Strong Correlated variables - Survey.Country, Dominant.Denomination, Nationality
# Weak Correlated variables - National.Language
#----------------------

meanWeak <- mean(prcSurvey$Language)

weakVar = rep(NA, nrow(prcSurvey))
weakVar[prcSurvey$Language > meanWeak] = "Above Average"
weakVar[prcSurvey$Language <= meanWeak] = "Below Average"
prcSurvey2 = data.frame(prcSurvey, weakVar)

ggplot(prcSurvey2, aes(x=Birthplace, y=Religion, color=weakVar)) +
  geom_point()
