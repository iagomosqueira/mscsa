# SC_simulation_M - «Short one line description»
# SC_simulation_M

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Tue 12 Jan 2010 09:51:24 AM CET IM:

source("vpa.R")

# Mortalities
M <- 0.2
F <- 0.3

# Selectivity
Pa <- c(0.001, 0.25, rep(1, 13))

iter <- 200
iyears <- 25
pyears <- 25
f <- rep(F, iter)

# Empty NAA array
naa <- array(NA, dim=c(iyears + pyears,15, iter),
    dimnames=list(year=1:(iyears + pyears), age=1:15, iter=1:iter))

# Initial population size at age=1
naa[1,1,] <- 1000

# Simulation for all ages
for(a in 2:15)
  naa[1,a,] <- naa[1,a-1,] * exp(- (F*Pa[a-1] + M))

# Von Bertalanffy growth
winf <- 10
k <- 0.5
t0 <- -0.1
b <- 3

# Calculate weight at age
waa <- winf * (1 - exp(-k * ((1:15)-t0))) ^ b

# Recruitment
b1 <- 1000
b2 <- 1000

ssb <- sum(naa[1,3:15,] * waa[3:15])

# reference points
baa <- apply(naa, c(1,3), function(x) x*waa)
meanb <- mean(apply(apply(baa, c(2,3), sum), 1, mean), na.rm=TRUE)

# OM
for (y in 2:iyears)
{
  # recruitment
  naa[y,1,] <- (b1 * ssb) / (b2 + ssb) + (runif(iter) - 0.5) * 1000
    
  # abundances
  for(a in 2:15)
    naa[y,a,] <- naa[y-1,a-1,] * exp(- (F*Pa[a-1] + M))
  
  # SSB
  ssb <- sum(waa[3:15] * naa[y, 3:15,])
}

# catch series
caa <- naa
for (i in 1:50)
    caa[i,,] <- naa[i,,] * (1 - exp(-Pa %*% t(f) -M)) * (Pa %*% t(f)) / (Pa %*% t(f) + M)


# Simulation
for (i in (iyears+1):(iyears+pyears))
{
  # observation

  # run assessment
  res <- vpa(caa[1:(i-1),,], Pa, f, M, agesF=3:14)
  
  # HCR
  res$biom <- apply(apply(res$naa[i-1,,], c(2), function(x) x * waa), 2, sum, na.rm=TRUE)
  # If biomass < meanb, cut F by 10%
  f[res$biom < meanb] <- f[res$biom < meanb] * 0.90
  f[res$biom > meanb] <- f[res$biom > meanb] * 1.10
  print(sum(res$biom < meanb))

  # project population with new Fs
  ssb <- apply(waa[3:15] * naa[i-1, 3:15,], 2, sum)
  naa[i,1,] <- (b1 * ssb) / (b2 + ssb) + (runif(iter) - 0.5) * 1000
  caa[i,1,] <- naa[i,1,] * f * Pa[1]
  for(a in 2:15)
  {
    naa[i,a,] <- naa[i,a-1,] * exp(- (f*Pa[a-1] + M))
    # catch by age
    caa[i,a,] <- naa[i,a,] * f * Pa[a]
  }
}

# recruitment
plot(apply(naa[,1,], 1, median), type='b', pch=19, ylim=c(0, max(naa)), xlab="",
    ylab="rec")
  lines(1:50, apply(naa[,1,], 1, quantile, 0.025), lty=2)
  lines(1:50, apply(naa[,1,], 1, quantile, 0.975), lty=2)
abline(v=25, lty=2, col='red')

# SSB
ssb <- apply(apply(naa[,,], c(1,3), function(x) x * waa), c(2,3), sum)
plot(apply(ssb, 1, median), type='b', ylim=c(0, max(ssb)), pch=19, xlab="", ylab="SSB")
  lines(1:50, apply(ssb, 1, quantile, 0.025), lty=2)
  lines(1:50, apply(ssb, 1, quantile, 0.975), lty=2)
abline(v=25, lty=2, col='red')

# SSB
catch <- apply(apply(caa[,,], c(1,3), function(x) x * waa), c(2,3), sum)
plot(apply(catch, 1, median), type='b', ylim=c(0, max(ssb)), pch=19, xlab="", ylab="Catch")
  lines(1:50, apply(catch, 1, quantile, 0.025), lty=2)
  lines(1:50, apply(catch, 1, quantile, 0.975), lty=2)
abline(v=25, lty=2, col='red')
