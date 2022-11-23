# started October 4, 2022

# aim of this code is to test whether there are differences in traits across the populations

rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
options(mc.cores = parallel::detectCores())
#graphics.off()

if(length(grep("deirdreloughnan", getwd())>0)) {
  setwd("~/Documents/github/wildhellgarden/analyses")
} else if(length(grep("Lizzie", getwd())>0)) {
  setwd("~/Documents/git/projects/others/deirdre/synchrony")
} else{
  setwd("/home/deirdre/WildHellCommonGarden") # for midge
}

# Load libraries
library(gridExtra)
library(ggplot2)
library(viridis)
library(rstan)
library(dplyr)
require(stringr)

# read in height data
ht <- read.csv("input/height_data.csv")

temp <- str_split_fixed(ht$ID, "_", 3); head(temp)
ht$spp<- temp[,1]
ht$pop <- temp[,2]
ht$indiv <- temp[,3]

ht$spp[ht$spp == "*SPITOM"] <- "SPITOM"
ht$spp[ht$spp == "BETPOPX"] <- "BETPOP"


unique(ht$Year) # 2019, 2021, 2022
htData <- ht[complete.cases(ht$Height),]

# 2019
htData2019 <- subset(htData, Year == "2019")
htData2019$sppFact <- as.numeric(as.factor(htData2019$spp))
htData2019$popFact <- as.numeric(as.factor(htData2019$pop))
datalistHt <- with(htData2019, 
                        list(y = Height,  
                             sp = sppFact,
                             pop = popFact,
                             N = nrow(htData2019),
                             n_sp = length(unique(htData2019$spp)),
                             n_pop = length(unique(htData2019$pop))
                        )
)
# 
# mdlHt19 = stan('stan/nointer_3levelwpop_pheno_ncp.stan',
#                  data = datalistHt, iter = 5000, warmup= 4000,
#                  chains=4,
#                  control=list(adapt_delta=0.999,max_treedepth = 15)
#                    #adapt_delta=0.999,
# 
#                  )
# 
# save(mdlHt19, file="output/mdlHt2019.Rda")
# 
# #load("analyses/output/mdlHt2019.Rda")
# sumHt19 <- summary(mdlHt19)$summary
# # 
# # # pdf("3monthCov.pdf", width = 10, height = 10)
# # # pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]"))
# # # dev.off()
# # ssm <-  as.shinystan(mdlHt)
# # launch_shinystan(ssm)
# 
# range(sumHt19[, "n_eff"])
# range(sumHt19[, "Rhat"])
# 952.346 7879.273 n_eff
# 0.999-1.001
# runs with no issues
# But sigma_a_sp  & sigma_a_pop look weird, the a_sppop also look weird

# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outHt <- data.frame(sumHt[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])

# 2021

htData2021 <- subset(htData, Year == "2021")
htData2021$sppFact <- as.numeric(as.factor(htData2021$spp))
htData2021$popFact <- as.numeric(as.factor(htData2021$pop))
datalistHt <- with(htData2021, 
                   list(y = Height,  
                        sp = sppFact,
                        pop = popFact,
                        N = nrow(htData2021),
                        n_sp = length(unique(htData2021$spp)),
                        n_pop = length(unique(htData2021$pop))
                   )
)


mdlHt21 = stan('stan/nointer_3levelwpop_pheno_ncp.stan',
             data = datalistHt, iter = 5000, warmup= 4000,
             chains=4,
             control=list(adapt_delta=0.99,max_treedepth = 15)
)

save(mdlHt21, file="output/mdlHt2021.Rda")

#load("analyses/output/mdlHt2021.Rda")
sumHt21 <- summary(mdlHt21)$summary
# 
# # pdf("3monthCov.pdf", width = 10, height = 10)
# # pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]"))
# # dev.off()
# ssm <-  as.shinystan(mdlHt)
# launch_shinystan(ssm)

range(sumHt21[, "n_eff"])
range(sumHt21[, "Rhat"])
# 647.3473 8596.0526
#0.9992854 1.0029670

# No issues
# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outHt <- data.frame(sumHt[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])

# 2022
htData2022 <- subset(htData, Year == "2022")
htData2022$sppFact <- as.numeric(as.factor(htData2022$spp))
htData2022$popFact <- as.numeric(as.factor(htData2022$pop))
datalistHt <- with(htData2022, 
                   list(y = Height,  
                        sp = sppFact,
                        pop = popFact,
                        N = nrow(htData2022),
                        n_sp = length(unique(htData2022$spp)),
                        n_pop = length(unique(htData2022$pop))
                   )
)

datalistHt$sp
mdlHt22 = stan('stan/nointer_3levelwpop_pheno_ncp.stan',
             data = datalistHt, iter = 5000, warmup= 4000,
             chains=4,
             control=list(adapt_delta=0.97, max_treedepth = 15)
)

save(mdlHt22, file="output/mdlHt2022.Rda")
# runs with no issues

#load("analyses/output/mdlHt2022.Rda")
sumHt22 <- summary(mdlHt22)$summary
# 
# # pdf("3monthCov.pdf", width = 10, height = 10)
# # pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]"))
# # dev.off()
# ssm <-  as.shinystan(mdlHt)
# launch_shinystan(ssm)

range(sumHt22[, "n_eff"])
range(sumHt22[, "Rhat"])
#688.4607 5623.9720
# 0.9993108 1.0130419
# No div transitions or warnings

# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outHt <- data.frame(sumHt[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])
#########################################################################
# read in leaf trait data:
leaf <- read.csv("input/SLA_LDMC_trait.csv")

colnames(leaf)[colnames(leaf) == "species"] <- "spp"
colnames(leaf)[colnames(leaf) == "site"] <- "pop"
colnames(leaf)[colnames(leaf) == "ind"] <- "indiv"

######### SLA #################################
slaData <- leaf[complete.cases(leaf$SLA),]
unique(slaData$year)

# 2019
slaData2019 <- subset(slaData, year == "2019")
slaData2019$sppFact <- as.numeric(as.factor(slaData2019$spp))
slaData2019$popFact <- as.numeric(as.factor(slaData2019$pop))
datalistSla <- with(slaData2019, 
                   list(y = SLA,  
                        sp = sppFact,
                        pop = popFact,
                        N = nrow(slaData2019),
                        n_sp = length(unique(slaData2019$spp)),
                        n_pop = length(unique(slaData2019$pop))
                   )
)


mdlSLA19 = stan('stan/nointer_3levelwpop_pheno_ncp.stan', 
                 data = datalistSla, iter = 5000, warmup= 4000, 
                 chains=4, 
                 control=list(adapt_delta=0.999, max_treedepth = 15)
)
save(mdlSLA19, file="output/mdlSLA2019.Rda")
# runs with no issues

#load("analyses/output/mdlSLA2019.Rda")
sumSLA19 <- summary(mdlSLA19)$summary
# 
# # pdf("3monthCov.pdf", width = 10, height = 10)
# # pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]"))
# # dev.off()
# ssm <-  as.shinystan(mdlSLA)
# launch_shinystan(ssm)

range(sumSLA19[, "n_eff"])
range(sumSLA19[, "Rhat"])
# 1383.840 7387.617
# 0.9991008 1.0017110

# no div transitions or warnings

# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outSLA <- data.frame(sumSLA[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])

# 2022
slaData2022 <- subset(slaData, year == "2022")
slaData2022$sppFact <- as.numeric(as.factor(slaData2022$spp))
slaData2022$popFact <- as.numeric(as.factor(slaData2022$pop))
datalistSla <- with(slaData2022, 
                    list(y = SLA,  
                         sp = sppFact,
                         pop = popFact,
                         N = nrow(slaData2022),
                         n_sp = length(unique(slaData2022$spp)),
                         n_pop = length(unique(slaData2022$pop))
                    )
)

# h1 <- hist(slaData2019$SLA, xlim = c(0, 400), ylim = c(0, 450))
# h2 <- hist(slaData2022$SLA)
# plot(h2, col=rgb(0,0,1,1/4), xlim = c(0, 200), ylim = c(0, 450))
# plot(h1, col=rgb(1,0,1,1/4), add = T)

mdlSLA22 = stan('stan/nointer_3levelwpop_pheno_ncp.stan', 
              data = datalistSla, iter = 5000, warmup= 4000, 
              chains=4,
              control=list(adapt_delta=0.999, max_treedepth = 15)
)

save(mdlSLA22, file="output/mdlSLA2022.Rda")
# Oct 21 2022: 2000+ div trans and Rhat > 2

#load("analyses/output/mdlSLA2022.Rda")
sumSLA22 <- summary(mdlSLA22)$summary
# 
# # pdf("3monthCov.pdf", width = 10, height = 10)
# # pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]"))
# # dev.off()
# ssm <-  as.shinystan(mdlSLA)
# launch_shinystan(ssm)

range(sumSLA22[, "n_eff"])
range(sumSLA22[, "Rhat"])

# Horrific!! 3967 div transitions
# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outSLA <- data.frame(sumSLA[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])
############### LDMC ##############################
ldmcData <- leaf[complete.cases(leaf$LDMC),]
unique(ldmcData$year)

# 2019
ldmcData2019 <- subset(ldmcData, year == "2019")
ldmcData2019$sppFact <- as.numeric(as.factor(ldmcData2019$spp))
ldmcData2019$popFact <- as.numeric(as.factor(ldmcData2019$pop))
datalistLdmc <- with(ldmcData2019, 
                    list(y = LDMC,  
                         sp = sppFact,
                         pop = popFact,
                         N = nrow(ldmcData2019),
                         n_sp = length(unique(ldmcData2019$spp)),
                         n_pop = length(unique(ldmcData2019$pop))
                    )
)

# h1 <- hist(ldmcData2019$LDMC, xlim = c(0, 700), ylim = c(0, 500))
# h2 <- hist(ldmcData2022$LDMC)
# plot(h2, col=rgb(0,0,1,1/4), xlim = c(0, 200), ylim = c(0, 450))
# plot(h1, col=rgb(1,0,1,1/4), add = T)


mdlLDMC19 = stan('stan/nointer_3levelwpop_pheno_ncp.stan', 
              data = datalistLdmc, iter = 6000, warmup= 4000, 
              chains=4, 
              control=list(adapt_delta=0.999, max_treedepth = 15)
)
save(mdlLDMC19, file="output/mdlLDMC2019.Rda")
#205 dT, no other issues

#load("analyses/output/mdlLDMC2019.Rda")
sumLDMC19 <- summary(mdlLDMC19)$summary
# 
# # pdf("3monthCov.pdf", width = 10, height = 10)
# # pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]"))
# # dev.off()
# ssm <-  as.shinystan(mdlLDMC)
# launch_shinystan(ssm)

range(sumLDMC19[, "n_eff"])
range(sumLDMC19[, "Rhat"])

# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outLDMC <- data.frame(sumLDMC[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])

# 2022
ldmcData2022 <- subset(ldmcData, year == "2022")
ldmcData2022$sppFact <- as.numeric(as.factor(ldmcData2022$spp))
ldmcData2022$popFact <- as.numeric(as.factor(ldmcData2022$pop))
datalistLdmc <- with(ldmcData2022, 
                     list(y = LDMC,  
                          sp = sppFact,
                          pop = popFact,
                          N = nrow(ldmcData2022),
                          n_sp = length(unique(ldmcData2022$spp)),
                          n_pop = length(unique(ldmcData2022$pop))
                     )
)


mdlLDMC22 = stan('stan/nointer_3levelwpop_pheno_ncp.stan', 
               data = datalistLdmc, iter = 6000, warmup= 4000, 
               chains=4, control=list(adapt_delta=0.99, max_treedepth = 15)
)
save(mdlLDMC22, file="output/mdlLDMC2022.Rda")
#8000 div trans rhat > 2

# load("analyses/output/mdlLDMC2022.Rda")
sumLDMC22 <- summary(mdlLDMC22)$summary
# 
# # pdf("3monthCov.pdf", width = 10, height = 10)
# # pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]"))
# # dev.off()
# ssm <-  as.shinystan(mdlLDMC)
# launch_shinystan(ssm)

range(sumLDMC22[, "n_eff"])
range(sumLDMC22[, "Rhat"])

#7899 div transitions

# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outLDMC <- data.frame(sumLDMC[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])
################# SSD ############################################
# read in stem trait data: only one year of data
stem <- read.csv("input/SSD_trait.csv")

ssdData <- stem[complete.cases(stem$ssd),]
ssdData$sppFact <- as.numeric(as.factor(ssdData$species))
ssdData$popFact <- as.numeric(as.factor(ssdData$site))
datalistSSD <- with(ssdData, 
                    list(y = ssd,  
                         sp = sppFact,
                         pop = popFact,
                         N = nrow(ssdData),
                         n_sp = length(unique(ssdData$species)),
                         n_pop = length(unique(ssdData$site))
                    )
)


mdlSSD = stan('stan/nointer_3levelwpop_pheno_ncp.stan', 
              data = datalistSSD, iter = 4000, warmup= 3000, 
              chains=4, 
              control=list(adapt_delta=0.999,
              max_treedepth = 18)
)
save(mdlSSD, file="output/mdlSSD2022.Rda")
# 3 div trans
sumSSD <- summary(mdlSSD)$summary

# pdf("3monthCov.pdf", width = 10, height = 10)
# pairs(mdlCovOld, pars = c("cont_beta[1]","cont_alpha[1]")) 
# dev.off()

range(sumSSD[, "n_eff"])
range(sumSSD[, "Rhat"])

# param <- c("mu_a_sp","alpha",
#            "a_sp[1]","a_sp[2]","a_sp[3]","a_sp[4]","a_sp[5]","a_sp[6]","a_sp[7]","a_sp[8]","a_sp[9]","a_sp[10]","a_sp[11]","a_sp[12]","a_sp[13]","a_sp[14]","a_sp[15]","a_sp[16]","a_sp[17]","a_sp[18]","a_sp[19]", "a_sppop[1]", "a_sppop[2]","a_sppop[3]","a_sppop[4]","a_sppop[5]","sigma_a_sp","sigma_a_pop","sigma_y")
# 
# outSSD <- data.frame(sumSSD[param, c("mean","2.5%", "97.5%", "n_eff", "Rhat")])