## Looking at each major phase individually running stan
## Look at inter vs intra specific variation, is there local adaptation??
### Dan August 2022

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
library(dplyr)
library(brms)
# Set Working Directory
setwd("~/Documents/git/wildhellgarden/analyses")

cg <- read.csv("output/clean_obsdata_phases.csv")

checkers<-cg %>% group_by(spp,site) %>% count()

checkers<-checkers %>% group_by(spp,site) %>% count()

##SPECIES AT 4 SITES
All4<-c("VIBCAS","ALNINC","SPIALB","SPITOM","DIELON")

##SPECIES AT 3 SITES
GR.SH.WM<-c("ACESPI", "AMECAN","BETALL","MYRGAL")
GR.HF.SH<-c("BETPAP")
GR.HF.WM<-c("BETPOP")

#SPECIES AT 2 sites
HF.WM<-c("ACEPEN","AROMEL","QUERUB")
GR.HF<-c("SAMRAC")
GR.SH<-c("SORAME")
GR.WM<-c("VACMYR")

Big5<-filter(cg,spp %in% All4)

table(is.na(Big5$budburst)) #342 obs
table(is.na(Big5$leafout)) # 450
table(is.na(Big5$flowers)) #196
table(is.na(Big5$budset)) # 288
table(is.na(Big5$fruit)) # 228

table(is.na(Big5$flowering)) #87
table(is.na(Big5$fruiting)) #88
table(is.na(Big5$allflofruit)) #66

table(is.na(Big5$dvr)) # 337
table(is.na(Big5$fls)) # 191
table(is.na(Big5$gs)) #283

###try a crossed model first
mod.gs<-brm(gs~as.factor(year)+(1|spp)+(1|site),data=Big5,
        warmup=3000,iter=4000, control=list(adapt_delta=.99))

mod.gs %>%
  spread_draws(b_Intercept, r_site[site,]) %>%
  mutate(site_mean = b_Intercept + r_site) %>%
  ggplot(aes(y = site, x = site_mean)) +
  stat_halfeye()

Big5$year.cent<-Big5$year-mean(Big5$year)
mod.gs.lat<-brm(gs~year.cent+provenance.lat+(1|spp),data=Big5,
            warmup=3000,iter=4000, control=list(adapt_delta=.99))

mod.gs.lat<-brm(gs~year.cent+provenance.lat+(provenance.lat|spp),data=Big5,
                warmup=3000,iter=4000, control=list(adapt_delta=.99))

mean(Big5$year.cent)
newdata<-data.frame(year.cent=rep(c(2.921302e-14),each=4),provenance.lat=rep(unique(Big5$provenance.lat),2))
ploty<-cbind(newdata,fitted(mod.gs.lat,probs=c(.25,.75),newdata = newdata,re_formula = NA))
ggplot(ploty,aes(x=Estimate,y=provenance.lat))+geom_point()+geom_errorbarh(aes(xmin=`Q25`,xmax=`Q75`),height=0)


cg$year.cent<-cg$year-mean(cg$year)
mod.leafout.lat.all<-brm(leafout~year.cent+provenance.lat+(provenance.lat|spp),data=cg,
                warmup=3000,iter=4000, control=list(adapt_delta=.99))
fixef(mod.gs.lat.all)

