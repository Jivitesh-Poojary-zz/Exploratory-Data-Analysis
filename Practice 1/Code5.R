load("lattice.RData")
library(ggplot2)

ganglion.gg = ggplot(ganglion, aes(x=area, y=cp.ratio)) + geom_point()

ganglion.gg + geom_smooth()

ganglion.gg + geom_smooth(method="lm")

ganglion.gg + geom_smooth(method="lm", formula = y ~ x + I(x^2))

ganglion.lm = lm(cp.ratio ~ area, data=ganglion)
# install.packages(broom)
library(broom)
gang.lm.df = augment(ganglion.lm)
summary(gang.lm.df)

ggplot(gang.lm.df, aes(x=area, y=.resid)) + geom_point() + geom_smooth() +
  geom_abline(slope=0, intercept=0)

ganglion.lm2 = lm(cp.ratio ~ area + I(area^2), data=ganglion)
gang.lm2.df = augment(ganglion.lm2)
summary(gang.lm2.df)

ggplot(gang.lm2.df, aes(x=area, y=.resid)) + geom_point() + geom_smooth() +
  geom_abline(slope=0, intercept=0)

ggplot(gang.lm2.df, aes(x=.fitted, y=sqrt(abs(.resid)))) + geom_point() + geom_smooth()

ggplot(ganglion, aes(x=area, y=log2(cp.ratio))) + geom_point() + geom_smooth()

ggplot(ganglion, aes(x=area, y=log2(cp.ratio))) + geom_point() + geom_smooth(method = "lm")

ganglion.log.lm = lm(log2(cp.ratio) ~ area, data=ganglion)
gang.log.lm.df = augment(ganglion.log.lm)
ggplot(gang.log.lm.df, aes(x=area, y=.resid)) + geom_point() + geom_smooth() +
  geom_abline(slope=0, intercept=0)

ggplot(gang.log.lm.df, aes(x=.fitted, y=sqrt(abs(.resid)))) + geom_point() + geom_smooth()

n = nrow(gang.log.lm.df)
f.value = (0.5:(n - 0.5)) / n
gang.log.fit = data.frame(f.value, Fitted = sort(gang.log.lm.df$.fitted) - mean(gang.log.lm.df$.fitted), Residuals = sort(gang.log.lm.df$.resid))
library(tidyr)
gang.log.fit.long = gang.log.fit %>% gather(type, value, Fitted:Residuals)
ggplot(gang.log.fit.long, aes(x=f.value, y=value)) +
  geom_point() + facet_wrap(~type)

ggplot(gang.log.lm.df, aes(sample=.resid)) + stat_qq()

