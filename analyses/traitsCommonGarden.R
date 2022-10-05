# started October 4, 2022

# aim of this code is to test whether there are differences in traits across the populations

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
options(mc.cores = parallel::detectCores())
#graphics.off()

if(length(grep("deirdreloughnan", getwd())>0)) {
  setwd("~/Documents/github/wildhellgarden")
} else if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/others/deirdre/synchrony")
} else{
  setwd("/home/deirdre/wildhellgarden") # for midge
}

# Load libraries
library(gridExtra)
library(ggplot2)
library(viridis)
library(rstan)
library(dplyr)

# read in traits:
ht <- read.csv("analyses/input/height_data.csv")

temp <- str_split_fixed(ht$ID, "_", 4); head(temp)
ht$spp<- temp[,1]
ht$pop <- temp[,2]
ht$indiv <- temp[,3]
ht$dup <- temp[,4]

datalist.bb.pop <- with(ht, 
                        list(y = Height,  
                             sp = spp,
                             pop = pophere,
                             N = nrow(bb.stan2018),
                             n_sp = length(unique(bb.stan2018$spp)),
                             n_pop = length(unique(bb.stan2018$pophere))
                        )
)


m3l.ni.bb = stan('stan/nointer_3levelwpop_pheno_ncp.stan', data = datalist.bb.pop,
                 iter = 5000, warmup=4000, chains=4, control=list(adapt_delta=0.999, max_treedepth = 15))
