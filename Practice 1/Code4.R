library(ggplot2)
load("F:/Data/lattice.RData")

ggplot(bin.packing, aes(factor(number.runs), log2(empty.space))) + geom_boxplot()

ggplot(bin.packing, aes(sample=log2(empty.space))) +
  stat_qq() + facet_wrap(~number.runs)

# Spread-standardized residuals
number.runs = bin.packing$number.runs
log2.space = log2(bin.packing$empty.space)
log2.packing = data.frame(log2.space, number.runs)
log2.space.medians = aggregate(log2.space~number.runs, median, data=log2.packing)
log2.space.mad = aggregate(log2.space~number.runs, mad, data=log2.packing)
n = nrow(log2.packing)
log2.space.fitted = rep(NA, n)
log2.space.madlist = rep(NA, n)
for(J in 1:n){
  which.runs = which(log2.space.medians$number.runs == number.runs[J])
  log2.space.fitted[J] = log2.space.medians$log2.space[which.runs]
  log2.space.madlist[J] = log2.space.mad$log2.space[which.runs]
}
log2.space.residuals = log2.space - log2.space.fitted
log2.space.standardized = log2.space.residuals / log2.space.madlist
log2.model = data.frame(number.runs, 
                        residuals=log2.space.residuals,
                        std.residuals=log2.space.standardized)
ggplot(log2.model, aes(sample=std.residuals)) +
  stat_qq() + facet_wrap(~number.runs, ncol=3)

# Pool the residuals for those cases and draw a normal QQ plot.
log2.model.big.n = log2.model[number.runs > 1000,]
ggplot(log2.model.big.n, aes(sample=std.residuals)) +
  stat_qq() + geom_abline()


k = nrow(log2.space.medians)
plot248 = ggplot(log2.space.medians,
                 aes(x=log2(number.runs), y=log2.space)) +
  geom_point()


plot248 + geom_abline(slope=1/3,
                      intercept=log2.space.medians[k,2]-1/3*log2(log2.space.medians[k,1]))
plot248


median.log2.space = log2.space.medians$log2.space
log2.relative.spread = log2(log2.space.mad$log2.space / min(log2.space.mad$log2.space))
log2.sl = data.frame(median.log2.space, log2.relative.spread)
ggplot(log2.sl, aes(x=median.log2.space, y=log2.relative.spread)) +
  geom_point()


empty.space.medians = aggregate(empty.space~number.runs, median, data=bin.packing)
empty.space.mad = aggregate(empty.space~number.runs, mad, data=bin.packing)
empty.space.sl = data.frame(median.empty.space=empty.space.medians$empty.space,
                            log2.relative.spread=log2(empty.space.mad$empty.space/min(empty.space.mad$empty.space)))
ggplot(empty.space.sl, aes(x=median.empty.space, y=log2.relative.spread)) +
  geom_point()