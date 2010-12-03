# mse_simple - «Short one line description»
# mse_simple

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Thu 02 Dec 2010 10:56:12 AM CET IM:

library(FLXSA)
library(FLBRP)

iter <- 10
ystart <- 1957

# CONDITIONING
# In this example we assume the result of the SA gives us a sufficient OM

# load data: FLStock and FLIndices
data(ple4)
data(ple4.indices)

# run XSA
ple4xsa <- FLXSA(ple4, ple4.indices, FLXSA.control())

# update FLStock with XSA results: stock.n & harvest
ple4 <- ple4xsa + ple4

# creating and fitting an stock-recruitment object
ple4SR <- as.FLSR(ple4, model='bevholt')
ple4SR <- fmle(ple4SR)

# creating an FLBiol, the 'true' population
ple4B <- as(ple4, 'FLBiol')

# creating a fleet to fish on it
ple4F <- as(ple4, 'FLFleet')

# set capacity to 0.75
capacity(ple4F) <- 0.75

# and catchability to vary
catch.q(ple4F,1,1) <- FLQuant(1*exp(rnorm(length(c(catch.q(ple4F,1)[[1]])), 0, 0.2)),
  dimnames=dimnames(catch.q(ple4F, 1)[[1]]))


# OEM
# Random noise corresponding to measurement error
deviates <- exp(rnorm(iter, FLQuant(0, dimnames=dimnames(n(ple4B))), sd=0.30))
 
# Index of abundance from survey
ple4I <- FLIndex(index=deviates * n(ple4B), name='survey')
range(ple4I, c("startf","endf")) <- c(0,0.1)
 
# stock as generated for assessment from fleet catch of this spp
ple4S <- as(catches(ple4F)[[1]], 'FLStock')
ple4S <- transform(ple4S, stock.wt=wt(ple4B),
    catch.wt=wt(ple4B),
    m=m(ple4B),
    mat=fec(ple4B),
    harvest.spwn=spwn(ple4B),
    m.spwn=spwn(ple4B))
ple4S <- propagate(ple4S, iter)

# SIMULATION

# extend objects for simulation
# ple4S <- stf(ple4S, nyears=11)
ple4B <- stf(ple4B, nyears=11)
ple4I <- window(ple4I, end=2018)

for (y in 2009:2018)
{
  # Observation

  # Assessment
  xsa <- FLXSA(window(ple4S, end=y-1), FLIndices(one=window(ple4I, end=y-1)),
    FLXSA.control(), diag.flag=FALSE)
  # update ple4S
  ple4S <- ple4S[,ac(seq(ystart, y-1))] + xsa

  # HCR & management
  ple4S <- stf(ple4S, nyears=3)
  ctrl <- fwdControl(data.frame(year=c(y, y+1, y+2), val=25000, quantity="catch"))
  ple4S <- fwd(window(ple4S, end=2011), ctrl=ctrl, sr=list(model="mean",
    params=FLPar(500000)))

  # Go fish!
  metier(ple4F, 1) <- FLMetier(window(as(ple4S, 'FLCatch'), end=2009))
  ple4B <- fwd(window(ple4B, end=y+2), window(as(ple4S, 'FLCatch'), end=y+2),
    ctrl=ctrl, sr=ple4SR)

}
