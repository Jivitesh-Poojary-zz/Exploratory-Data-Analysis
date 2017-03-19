
# Color based facet
# MIPS machine faster than VAX - right shifted points

# Finding the improvements over the absolute values

# Spread is used for simplifying the math

# If interaction was present in the algo model improvment plot the there would be change in the dist betweeen
#   red and green points
# The pointer data shows that there could be interaction in the model


load("lattice.RData")
library(ggplot2)
library(tidyr)
library(broom)
library(MASS)

ggplot(run.time, aes(x = log2(time), y = algorithm)) + 
  geom_point() + 
  facet_wrap(~machine + input, ncol = 6)

ggplot(run.time, aes(x = log2(time), y = algorithm, color = machine)) + 
  geom_point() + 
  facet_wrap(~input, ncol = 3)

ggplot(run.time, aes(x = log2(time), y = input, color = machine)) + 
  geom_point() + 
  facet_wrap(~algorithm, ncol = 1)

run.wide = run.time %>% spread(algorithm, time)

run.wide

run.wide$imp.ucb = log2(run.wide$ucb) - log2(run.wide$new)
run.wide$imp.7th = log2(run.wide$"7th") - log2(run.wide$new)

run.long = run.wide %>% gather(algorithm, improvement, imp.ucb:imp.7th)

improvement.means = aggregate(improvement ~ input, mean, data = run.long)
improvement.means

input.order = improvement.means$input[order(improvement.means$improvement)]
run.long$input = factor(run.long$input, levels = input.order)

ggplot(run.long, aes(x = improvement, y = input, color = algorithm)) + 
  geom_point() + 
  facet_grid(~machine)

#------------------------------------------------
# Multiway linear model
runtime.lm = lm(improvement ~ input + algorithm + machine + input:machine + 
                  algorithm:machine, data = run.long)

runtime.lm = lm(improvement ~ (input + algorithm) * machine, data = run.long)

runtime.lm.df = augment(runtime.lm)

ggplot(runtime.lm.df, aes(x = .resid, y = input)) + 
  geom_point() + 
  facet_wrap(~algorithm + machine)

runtime.lm.df$.fit.demeaned = runtime.lm.df$.fitted - mean(runtime.lm.df$.fitted)
runtime.lm.long = runtime.lm.df %>% gather(component, value, c(.fit.demeaned, .resid))
ggplot(runtime.lm.long, aes(sample = value)) + 
  stat_qq(distribution = "qunif") + 
  facet_grid(~component)

ggplot(subset(runtime.lm.df, algorithm == "imp.7th"), aes(x = .fitted, y = input)) + 
  geom_point() + 
  facet_wrap(~machine, ncol = 1)

ggplot(subset(runtime.lm.df, algorithm == "imp.7th"), aes(x = 2^(.fitted), y = input)) + 
  geom_point() + 
  facet_wrap(~machine, ncol = 1)


#------------------------------------------------------------
# Barley
ggplot(barley, aes(x = yield, y = variety)) + 
  geom_point() + 
  facet_wrap(~year + site, ncol = 6)

ggplot(barley, aes(x = yield, y = variety, color = year)) + 
  geom_point() + 
  facet_wrap(~site)

morris1932fixed = barley$yield[barley$site == "Morris" & barley$year == 1931]
morris1931fixed = barley$yield[barley$site == "Morris" & barley$year == 1932]
barley.fixed = barley
barley.fixed$yield[barley$site == "Morris" & barley$year == 1932] = morris1932fixed
barley.fixed$yield[barley$site == "Morris" & barley$year == 1931] = morris1931fixed
ggplot(barley.fixed, aes(x = yield, y = variety, color = year)) + 
  geom_point() + 
  facet_wrap(~site)

barley.wide = barley %>% spread(year, yield)
barley.wide$difference = barley.wide$"1931" - barley.wide$"1932"

ggplot(barley.wide, aes(x = difference, y = variety)) + 
  geom_point() + 
  facet_wrap(~site)

barley.rlm = rlm(yield ~ variety + year * site, psi = psi.bisquare, data = barley)

barley.rlm.df = augment(barley.rlm)
barley.rlm.df$.fitted = barley.rlm.df$.fitted - mean(barley.rlm.df$.fitted)
barley.rlm.long = barley.rlm.df %>% gather(component, value, c(.fitted, .resid))
ggplot(barley.rlm.long, aes(sample = value)) + stat_qq(distribution = "qunif") + 
  facet_grid(~component)


barley.effects = dummy.coef(barley.rlm)
year.site.main = outer(barley.effects$year, barley.effects$site, "+")
year.site.inter = barley.effects$"year:site"
year.site.effect = year.site.inter + as.vector(year.site.main)
years = rep(row.names(year.site.main), 6)
sites = rep(colnames(year.site.main), each = 2)
sites = factor(sites, levels = names(barley.effects$site))
year.site.df = data.frame(year = years, site = sites, effect = year.site.effect)
ggplot(year.site.df, aes(x = effect, y = site, col = year)) + 
  geom_point()

variety.effects = sort(barley.effects$variety)
varieties = factor(names(barley.effects$variety), levels = names(barley.effects$variety))
variety.df = data.frame(effect = variety.effects, variety = varieties)
ggplot(variety.df, aes(x = effect, y = variety)) + 
  geom_point()

ggplot(barley.rlm.df, aes(x = .resid, y = site)) + 
  geom_point() + 
  facet_wrap(~variety + year, ncol = 4) + 
  theme(axis.text.y = element_text(size = 5))

ggplot(barley.rlm.df, aes(x = .resid, y = variety)) + 
  geom_point() + 
  facet_wrap(~site + year) + 
  theme(axis.text.y = element_text(size = 7))



