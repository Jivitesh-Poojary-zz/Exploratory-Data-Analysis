library(dplyr)
library(lattice)
library(ggplot2)
library(tidyr)

# Answer 1
pennVote <- read.table("F:/Data/pennsylvania.txt", header=TRUE)
pennVote.long = pennVote %>% gather(Candidate, Votes, Obama:Clinton)

# Answer 2
pennVote.long.log = log10(pennVote.long$Votes)
ggplot(pennVote.long, aes(sample=pennVote.long.log)) +
  stat_qq() +
  facet_grid(~Candidate)

ggplot(pennVote.long, aes(x=pennVote.long.log)) +
  geom_density() +
  facet_grid(~Candidate)

# Answer 3
vote = pennVote.long$Votes
obama.clinton = pennVote.long$Candidate

Obama.times = sort(vote[obama.clinton=="Obama"])
Clinton.times = sort(vote[obama.clinton=="Clinton"])
Obama.Clinton.qq = as.data.frame(qqplot(Obama.times, Clinton.times, plot.it=FALSE))
ggplot(Obama.Clinton.qq, aes(x, y)) + geom_point() +
  geom_abline()

Obama.times.log = sort(log10(vote[obama.clinton=="Obama"]))
Clinton.times.log = sort(log10(vote[obama.clinton=="Clinton"]))
Obama.Clinton.qq.log = as.data.frame(qqplot(Obama.times.log, Clinton.times.log, plot.it=FALSE))
ggplot(Obama.Clinton.qq.log, aes(x, y)) + geom_point() +
  geom_abline()