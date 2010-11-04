# plots - «Short one line description»
# plots

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Thu 04 Nov 2010 09:27:12 AM CET IM:

fraser <- read.table(file='../exercises/model_fitting/fraser.dat', sep='\t', header=T)
attach(fraser)

png('fraser.png')

foo <- function(bhat)
{ recest <- bhat * spawn
  sum((rec - recest)^2)
}

# optimization function in R, returns minimum (best value of bhat) and
# objective (SS at that value)
res <- optimize(foo, c(1, 3))

bhat <- res$minimum
ss <- res$objective
ssreg <- ss
sstot <- sum((rec - mean(rec)) ^2)
R2 <- 1-(ssreg/sstot)

# NOTE: Shouldn't this really be sserr?
# sstot = sum((y - mean(y)) ^ 2)
# ssreg = sum((yhat - mean(yhat)) ^ 2)
# sserr = sum((y - yhat) ^ 2)

plot(spawn, rec, xlab='spawners (thousands)', ylab='recruits (thousands)', pch=19,
  ylim=c(0, 2000))
abline(0, bhat, col='red', lwd=2)


dev.off()

# B&H
# R = a*S / 1 + b*S

spawn <- seq(0:50)

afoo <- function(a, b)
{
  plot(spawn, (a*spawn)/(1+b[1]*spawn), type='l', lwd=2, ylab='Recruits', xlab='Spawners')
  text(5, 26, labels=paste('a =', a))
  for(i in 2:length(b))
  {
    lines(spawn, (a*spawn)/(1 + b[i]*spawn), lwd=2, ylab='Recruits', xlab='Spawners')
  }
  text(45, (a*45)/(1 + b*45)-((a*45)/(1 + b*45)/20), labels=paste('b =', b))
}

afoo(6, seq(0.2, 0.5, by=0.1))
afoo(0.05, seq(0.07, 0.17, length=3))





