library(ggplot2)
library(broom)

ps3data = read.table("s670-ps3-data.txt", header = TRUE)

ggplot(ps3data, aes(x=x, y=1/y1)) + geom_point() + geom_smooth(method.args=list(degree=1))
ggplot(ps3data, aes(x=x, y=log10(y2))) + geom_point() + geom_smooth(method.args=list(degree=1, family="symmetric"))
ggplot(ps3data, aes(x=x, y=y3)) + geom_point() + geom_smooth(span=0.1)