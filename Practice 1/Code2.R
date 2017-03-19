library(lattice)
library(ggplot2)

singer.means = aggregate(height~voice.part, FUN=mean, data=singer)
ggplot(singer.means, aes(x=voice.part, y=height)) +
  geom_point() 
ggplot(singer.means, aes(x=height, y=voice.part)) +
  geom_point() 

# Models and residuals
singer.lm = lm(height ~ voice.part, data=singer)
singer.res = data.frame(voice.part=singer$voice.part, residual=residuals(singer.lm))

ggplot(singer.res, aes(x=voice.part, y=residual)) + geom_boxplot()

ggplot(singer.res, aes(sample = residual)) +
  stat_qq() + facet_wrap(~voice.part, ncol=2) 

ggplot(singer.res, aes(x = residual)) +
  stat_ecdf()

ggplot(singer.res, aes(sample = residual)) +
  stat_qq()

ggplot(singer.res, aes(sample = residual)) +
  stat_qq(distribution = qunif)

round(mean(singer.res$residual), 3)

round(sd(singer.res$residual), 3)

round(summary(singer.lm)$sigma, 3)

ggplot(singer.res, aes(sample = residual)) +
  stat_qq() + geom_abline(intercept=0, slope=summary(singer.lm)$sigma)


# Residual-fit spread
singer.fitted = sort(fitted.values(singer.lm)) - mean(fitted.values(singer.lm))
singer.residuals = sort(residuals(singer.lm))

n = length(singer.residuals)
f.value = (0.5:(n - 0.5)) / n
singer.fit = data.frame(f.value, Fitted=singer.fitted, Residuals=singer.residuals)


# gather(): A lifesaver
library(tidyr)
singer.fit.long = singer.fit %>% gather(type, value, Fitted:Residuals)

Counties = c("Hamilton","Marion","Monroe","Tippecanoe","Vermillion")
Clinton = c(2819, 212676, 34183, 27207, 2081)
Trump = c(8530, 130228, 20527, 30711, 4511)
Indiana = data.frame(Counties, Clinton, Trump)

Indiana.long = Indiana %>% gather(Candidate, Votes, Clinton:Trump)

ggplot(singer.fit.long, aes(x=f.value, y=value)) +
  geom_point() + facet_wrap(~type)


# Interlude: Tibbles
billboard.raw = read.csv("https://github.com/hadley/tidy-data/raw/master/data/billboard.csv", stringsAsFactors = FALSE)
billboard = tbl_df(billboard.raw)

billboard.time = strsplit(billboard$time, ":")

billboard.time = matrix(unlist(billboard.time),
                        byrow=T, ncol=2)
billboard.mins = as.numeric(billboard.time[,1])
billboard.secs = as.numeric(billboard.time[,2])
billboard.time = billboard.mins * 60 + billboard.secs
billboard$time = billboard.time / 60

billboard.long <- billboard %>% gather(week, rank, x1st.week:x76th.week, na.rm = TRUE)
billboard.long

ggplot(billboard, aes(x=time)) + geom_histogram(breaks=seq(2.5, 8, 0.25))
ggplot(billboard.long, aes(x=time)) + geom_histogram(breaks=seq(2.5, 8, 0.25))
ggplot(subset(billboard.long, rank <= 10), aes(x=time)) + 
  geom_density() + facet_wrap(~rank, ncol=2)

time.means = aggregate(time~rank, FUN=mean, data=billboard.long)
ggplot(time.means, aes(x=rank, y=time)) +
  geom_line()
