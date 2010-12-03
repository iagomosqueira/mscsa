# hcr - «Short one line description»
# hcr

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 01 Dec 2010 08:45:19 AM CET IM:

library(FLash)
library(FLBRP)
library(FLAssess)

# Let's look at the basis for developing HCRs in FLR: fwd()

# A data.frame is constructed with columns:
# - year: year for projection
# - quantity: target type for which projection is being made. These can be “ssb”, “biomass”, “catch”, “landings”, “discards”, “f”, “z”, “f.landings”, “f.discards”, “effort”, “costs”, “revenue”, “profit”. “mnsz”
# - min: lower bound for target
# - val: target for quantity
# - max: upper bound for target
# - spp: stock for which target applies
# - fleet: fleet for which target applies
# - metier: metier for which target applies
# - rel: if NA then quantity is absolute, otherwise it specifies the year which target is relative to

# The minimum information we need to provide is year, val and quantity
ctrl<-fwdControl(data.frame(year=2000, val=25000, quantity="catch"))
ctrl<-fwdControl(data.frame(year=2000, val=0.9, quantity="catch", rel=1999))

ctrl

# A first EXAMPLE
data(ple4)
 
# stf sets up the FLStock object ready for a short term forecast by extending
# it along the years dimension.
ple4P <- stf(ple4, nyears=3)

summary(ple4P)
catch(ple4P)
 
## default future F is mean of last 3 years
mean(fbar(ple4P)[,ac(2006:2008)])
 
# Take a look a the catch and fbar before the projection is run
computeCatch(ple4P)[,ac(2006:2008)]
fbar(ple4P)[,ac(2006:2011)]
 
## Catch Projection
ctrl <- fwdControl(data.frame(year=2009:2011, val=25000, quantity="catch"))
# This sets a target catch value of 25000 t in years 2009-2011
ctrl
 

ple4P <- fwd(ple4P, ctrl=ctrl, sr=list(model="mean",params=FLPar(500000)))

# Check recruitment was set as desired
rec(ple4P)

# and catch was achieved
catch(ple4P)

# Inspect results
plot(ple4P)
 
 
## F projection
ctrl <- fwdControl(data.frame(year=2008:2011, val=0.35, quantity="f"))
ple4P <- fwd(ple4P, ctrl=ctrl, sr=list(model="mean", params=FLPar(500000)))


# Stock-Recruitment
ple4SR <- as.FLSR(ple4, model='bevholt')
ple4SR <- fmle(ple4SR)

plot(ple4SR)
summary(ple4SR)

# catch projection with SR
ele4P <- fwd(ple4P, ctrl=ctrl, sr=ple4SR)

rec(ple4P)
plot(ple4P)

# Can you create a range of F & catch scenarios to test?

# (1) Catches increasing year by year by 15%
ctrl <- fwdControl(data.frame(year=2009:2011, val=1.15, quantity="catch", rel.year=2008:2010))
ple4P <- fwd(ple4P, ctrl=ctrl, sr=ple4SR)

plot(ple4P)

# (2) Decrease in F from last year to F_MSY = 0.134 in 3 years
ple4P <- stf(ple4, nyears=7)
ctrl <- fwdControl(data.frame(year=2009:2015,
  val=seq(c(fbar(ple4)[,'2008']), 0.134, length=8)[-1], quantity="f"))

ple4P <- fwd(ple4P, ctrl=ctrl, sr=ple4SR)

plot(ple4P)

# Test with another SR model
ple4SR2 <- as.FLSR(ple4, model='ricker')
ple4SR2 <- fmle(ple4SR2)

# Getting BRPs
ple4BRP <- brp(FLBRP(ple4, sr=ple4SR))
refpts(ple4BRP)

# Hake-like recovery plan
ctrl <- fwdControl(data.frame(year=rep(2009:2015, each=2), val=rep(c(0.90, NA), 7), quantity=rep(c("f", "catch"), 7), rel.year=rep(2008:2014, each=2), min=rep(c(NA, 0.85), 7), max=rep(c(NA, 1.15), 7)))

