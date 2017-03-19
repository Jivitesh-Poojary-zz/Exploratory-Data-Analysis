library(dplyr)
library(ggplot2)

load("lattice.RData")

time = fusion.time$time
nv.vv = fusion.time$nv.vv
NV.times = sort(time[nv.vv=="NV"])
VV.times = sort(time[nv.vv=="VV"])
NV.VV.qq = as.data.frame(qqplot(NV.times, VV.times, plot.it=FALSE))
ggplot(NV.VV.qq, aes(x, y)) + geom_point() +
  geom_abline()

# Log transformation
NV.times.log = sort(log2(time[nv.vv=="NV"]))
VV.times.log = sort(log2(time[nv.vv=="VV"]))
NV.VV.qq.log = as.data.frame(qqplot(NV.times.log, VV.times.log, plot.it=FALSE))
ggplot(NV.VV.qq.log, aes(x, y)) + geom_point() +
  geom_abline(intercept=-0.75)

# Tukey mean-difference plot
ggplot(NV.VV.qq.log, aes((x+y)/2, y-x)) + geom_point()


# Power transformation
n.VV = length(VV.times)
power = rep(seq(-1,1,0.25), each=n.VV)
VV.time = c(VV.times^-1, VV.times^-.75, VV.times^-.5, VV.times^-.25, log(VV.times), VV.times^.25, VV.times^.5, VV.times^.75, VV.times)
ggplot(data.frame(power, VV.time), aes(sample=VV.time)) + stat_qq() + facet_wrap(~power, scales="free")

# Food webs
# ----------------------------------
# Uniform Distribution
ggplot(food.web, aes(sample=mean.length)) +
  stat_qq(distribution = qunif) +
  facet_grid(~dimension)

# spread-location plot
web.length = food.web$mean.length
dimension = food.web$dimension
n = nrow(food.web)
median.3 = median(web.length[dimension=="Three"])
median.2 = median(web.length[dimension=="Two"])
median.mixed = median(web.length[dimension=="Mixed"])
group.median = rep(NA, n)
group.median[dimension == "Three"] = median.3
group.median[dimension == "Two"] = median.2
group.median[dimension == "Mixed"] = median.mixed
jittered.medians = group.median + runif(n, -0.1, 0.1)
root.abs.res = sqrt(abs(web.length - group.median))
food.web.sl = data.frame(jittered.medians, root.abs.res, dimension)
ggplot(food.web.sl, aes(jittered.medians, root.abs.res, col=dimension)) +
  geom_point()

# Normal Distribution
ggplot(food.web, aes(sample = mean.length)) +
  stat_qq() + facet_wrap(~dimension)

# spread-location plot
inv.web.length = 1/food.web$mean.length
median.3.inv = median(inv.web.length[dimension=="Three"])
median.2.inv = median(inv.web.length[dimension=="Two"])
median.mixed.inv = median(inv.web.length[dimension=="Mixed"])
group.median.inv = rep(NA, n)
group.median.inv[dimension == "Three"] = median.3.inv
group.median.inv[dimension == "Two"] = median.2.inv
group.median.inv[dimension == "Mixed"] = median.mixed.inv
jittered.medians.inv = group.median.inv + runif(n, -0.01, 0.01)
root.abs.res.inv = sqrt(abs(inv.web.length - group.median.inv))
food.web.inv.sl = data.frame(jittered.medians.inv, root.abs.res.inv, dimension)
ggplot(food.web.inv.sl, aes(jittered.medians.inv, root.abs.res.inv, col=dimension)) +
  geom_point()

aggregate(root.abs.res.inv ~ dimension, FUN=mean)

# spread-location plot
ggplot(food.web, aes(sample = 1/mean.length)) +
  stat_qq() + facet_wrap(~dimension)

# two-sample QQ
# 
food.web.lm = lm(inv.web.length ~ dimension)
food.web.res = residuals(food.web.lm)
res.qq.3 = qqplot(food.web.res, food.web.res[dimension == "Three"], plot.it=FALSE)
res.qq.2 = qqplot(food.web.res, food.web.res[dimension == "Two"], plot.it=FALSE)
res.qq.mixed = qqplot(food.web.res, food.web.res[dimension == "Mixed"], plot.it=FALSE)
food.web.res.qq = data.frame(pooled = c(res.qq.3$x, res.qq.2$x, res.qq.mixed$x),
                             residual = c(res.qq.3$y, res.qq.2$y, res.qq.mixed$y),
                             dimension=c(rep("Three",length(res.qq.3$x)),
                                         rep("Two",length(res.qq.2$x)),
                                         rep("Mixed",length(res.qq.mixed$x))))
ggplot(food.web.res.qq, aes(pooled, residual)) + geom_point() +
  geom_abline() + facet_wrap(~dimension)

# Residual-fit plot 
food.web.fitted = sort(fitted.values(food.web.lm)) - mean(fitted.values(food.web.lm))
n = length(inv.web.length)
f.value = (0.5:(n - 0.5)) / n
food.web.fit = data.frame(f.value, Fitted=food.web.fitted, Residuals=sort(food.web.res))
food.web.fit.long = food.web.fit %>% gather(type, value, Fitted:Residuals)
ggplot(food.web.fit.long, aes(x=f.value, y=value)) +
  geom_point() + facet_wrap(~type)