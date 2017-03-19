# Load data
nationalID = read.csv("national-identity.csv")
IDnumerator = 4*nationalID[,3] + 3*nationalID[,4] + 2*nationalID[,5] + nationalID[,6]
IDdenominator = nationalID[,3] + nationalID[,4] + nationalID[,5] + nationalID[,6]
IDscore = IDnumerator / IDdenominator
nationalID2 = data.frame(Country = nationalID$Country, Question = nationalID$Question, IDscore)

# Change from long form to wide form
library(tidyr)
IDwide = spread(nationalID2, Question, IDscore)
# Standardize
IDscaled = data.frame(Country = IDwide$Country, scale(IDwide[,-1]))
# Fix Japan
IDscaled[is.na(IDscaled)] = 0

# Find country totals
IDtotal = apply(IDscaled[,-1], 1, sum)
library(ggplot2)
ggplot(data.frame(Country=IDscaled$Country, IDtotal),
       aes(x = IDtotal, y = reorder(Country, IDtotal))) +
       geom_point() +
       xlab("Total of standardized scores on national ID questions") +
       ylab("Country")

# Draw paired plots
library(GGally)
ggpairs(IDscaled[,-1])

# Conditional analysis
LanguageFactor = factor(sign(IDscaled$Language))
ggplot(data.frame(Birth = IDscaled$Birth, Religion = IDscaled$Religion, Language=LanguageFactor),
       aes(x = Birth, y = Religion, color=Language)) +
       geom_point() + scale_color_discrete(labels=c("Below average","Above average"))


