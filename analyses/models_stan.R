## Looking at each major phase individually running stan
## Look at inter vs intra specific variation, is there local adaptation??
# 12 Feb 2021 by Cat

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
options(mc.cores = parallel::detectCores())
graphics.off()

# Load libraries
library(gridExtra)
library(ggplot2)
library(viridis)
library(rstan)

# Set Working Directory
setwd("~/Documents/git/wildhellgarden/analyses")

cg <- read.csv("output/clean_obsdata_phases.csv")

dvr.stan <- subset(cg, select=c("spp", "year", "site", "ind", "plot", "dvr"))
dvr.stan <- dvr.stan[complete.cases(dvr.stan),]


getpop <- paste(dvr.stan$spp, dvr.stan$site)
dvr.stan$pophere <- as.numeric(as.factor(getpop))
dvr.stan$spp <- as.numeric(as.factor(dvr.stan$spp))

datalist.dvr.pop <- with(dvr.stan, 
                         list(y = dvr,  
                              sp = spp,
                              pop = pophere,
                              N = nrow(dvr.stan),
                              n_sp = length(unique(dvr.stan$spp)),
                              n_pop = length(unique(dvr.stan$pophere))
                         )
)


m3l.ni.dvr = stan('stan/nointer_3levelwpop_ncp.stan', data = datalist.dvr.pop,
              iter = 2000, warmup=1500, chains=2)





