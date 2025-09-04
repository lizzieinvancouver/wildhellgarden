## Loking at each major phase individually running stan
## Look at inter vs intra specific variation, is there local adaptation??
### Dan May 2022

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
options(mc.cores = parallel::detectCores())
graphics.off()

if(length(grep("deirdreloughnan", getwd())>0)) { 
  setwd("~/Documents/github/wildhellgarden/analyses")
} else if(length(grep("lizzie", getwd())>0)) {setwd("~/Documents/git/projects/") 
} else setwd("~/Documents/git/wildhellgarden/analyses")

# Load libraries

#library(rstan)
library(dplyr)
library(brms)
library(tidybayes)
library(tidyr)

 #source("source/combineWeather.R")
 #source("source/conceptualFigure.R")
#load("cgseasonmods.Rda")


#couM<-cg1 %>% filter(year==2020) %>% group_by(spp) %>%count()


goober <- read.csv("output/gddData.csv")
cg <- read.csv("output/clean_obs_allyrs.csv")### this is the updated data with leaf color

###calculate primary growth season and full growing season in days
cg$pgs<-cg$budset-cg$leafout
cg$fgs<-cg$leafcolor-cg$leafout
 
#### remove NA's 
cg1<-filter(cg,!is.na(pgs)) #This is datasheet for "primary growing season (leafout to budset)

cg2<-filter(cg,!is.na(fgs)) #This is datasheet for "full growing season (leafout to leaf color)

###round any fraction doys to closest full doy 
cg1$leafout<-round(cg1$leafout)
cg2$leafout<-round(cg2$leafout) ### some were X.5 which messes up everything up

ref<-data.frame(year=cg1$year,start=cg1$leafout,end=cg1$budset)## a list of to Sos and EoS for each individual in each year
myvec<-c()

##this calculates the thermal growing season of primary growth for each individual
for(j in 1:length(cg1$pgs)){
temp<-filter(goober,year==cg1$year[j])
tempS<-filter(temp,doy==cg1$leafout[j])
tempE<-filter(temp,doy==cg1$budset[j])

GDD <- tempE$GDD_10-tempS$GDD_10
 #print(GDD)
      myvec<-c(myvec,GDD)
}

cg1$pgsGDD<-myvec ### add this to main dataframe

cg1$indy<-paste(cg1$spp,cg1$site,cg1$ind,cg1$plot)### give and unique identifier

### now to the same thing for the full growing season
ref2<-data.frame(year=cg2$year,start=cg2$leafout,end=cg2$leafcolor)
myvec2<-c()

for(j in 1:length(cg2$fgs)){
  temp<-filter(goober,year==cg2$year[j])
  tempS<-filter(temp,doy==cg2$leafout[j])
  tempE<-filter(temp,doy==cg2$leafcolor[j])
  GDD<-tempE$GDD_10-tempS$GDD_10
 # print(GDD)
  myvec2<-c(myvec2,GDD)
}

cg2$fgsGDD<-myvec2

####center variables
cg1$pgsGDD_cent<-cg1$pgsGDD-mean(cg1$pgsGDD)
cg1$leafout_cent<-cg1$leafout-mean(cg1$leafout)

cg_wgdd <- merge(cg2, cg1, by = c("spp", "year", "site", "ind", "plot"), all = TRUE)

#ggplot(cg_wgdd,aes(leafcolor,budset))+geom_point()+geom_abline()

####par II models
###model is growing season duration ~ dat of leafout centered with partial pooling on species  for slope and intercept

pgsGDD.mod<-brm(pgsGDD~leafout_cent+(leafout_cent|spp),data=cg1,control = list(adapt_delta=.95),warmup=3000,iter=4000)
pgs.mod<-brm(pgs~leafout_cent+(leafout_cent|spp),data=cg1,control = list(adapt_delta=.95),warmup=3000,iter=4000)





###do the same models for full growing season
cg2$fgsGDD_cent<-cg2$fgsGDD-mean(cg2$fgsGDD)
cg2$leafout_cent<-cg2$leafout-mean(cg2$leafout)

fgsGDD.mod<-brm(fgsGDD~leafout_cent+(leafout_cent|spp),data=cg2,control = list(adapt_delta=.95),warmup=3000,iter=4000)
fgs.mod<-brm(fgs~leafout_cent+(leafout_cent|spp),data=cg2,control = list(adapt_delta=.95),warmup=3000,iter=4000)

###make posterior predictions

new.data<-data.frame(leafout_cent=rep(c(-20,0,20),each=18),spp=rep(unique(cg1$spp),3)) ## generate a new data frame


FGDD<-epred_draws(fgsGDD.mod,newdata = new.data,ndraws = 1000)### prediction thermal model for each species
Fp<-epred_draws(fgs.mod,newdata = new.data,ndraws = 1000)### prediction for calaendar each species
FGDD2<-epred_draws(fgsGDD.mod,newdata = new.data,ndraws = 1000,re_formula = NA)### prediction thermal model for each species
F2<-epred_draws(fgs.mod,newdata = new.data,ndraws = 1000,re_formula = NA)### prediction for calaendar each species

FGDD$grouper<-paste(FGDD$spp,FGDD$.draw) ## dummy variable to group each posterior draw for plotting
Fp$grouper<-paste(Fp$spp,Fp$.draw)


###same thing for primary growing season
PGDD<-epred_draws(pgsGDD.mod,newdata = new.data,ndraws = 1000)
Pp<-epred_draws(pgs.mod,newdata = new.data,ndraws = 1000)
PGDD2<-epred_draws(pgsGDD.mod,newdata = new.data,ndraws = 1000,re_formula = NA)
PGDD$grouper<-paste(PGDD$spp,PGDD$.draw)
Pp$grouper<-paste(Pp$spp,Pp$.draw)

P2<-epred_draws(pgs.mod,newdata = new.data,ndraws = 1000,re_formula = NA)

order<-c("SAMRAC","VIBCAS","SPIALB","AMECAN","DIELON","AROMEL","SPITOM","BETPOP","MYRGAL","BETPAP","BETALL","ALNINC","SORAME")

Pp2<-filter(Pp,spp %in% order)
Fp2<-filter(Fp,spp %in% order)


Pp2$species<-NA
Pp2$species<-ifelse(Pp2$spp=="SAMRAC","S. racemosa",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="VIBCAS","V. cassinoides",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="SPIALB","S. alba",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="AMECAN","A. canadensis",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="DIELON","D. lonicera",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="AROMEL","A. melanocarpa",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="SPITOM","S. tomentosa",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="BETPOP","B. populifolia",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="MYRGAL","M. gale",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="BETPAP","B. papyrifera",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="BETALL","B. alleghaniensis",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="ALNINC","A. incana",Pp2$species)
Pp2$species<-ifelse(Pp2$spp=="SORAME","S. americana",Pp2$species)

Fp2$species<-NA
Fp2$species<-ifelse(Fp2$spp=="SAMRAC","S. racemosa",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="VIBCAS","V. cassinoides",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="SPIALB","S. alba",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="AMECAN","A. canadensis",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="DIELON","D. lonicera",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="AROMEL","A. melanocarpa",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="SPITOM","S. tomentosa",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="BETPOP","B. populifolia",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="MYRGAL","M. gale",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="BETPAP","B. papyrifera",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="BETALL","B. alleghaniensis",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="ALNINC","A. incana",Fp2$species)
Fp2$species<-ifelse(Fp2$spp=="SORAME","S. americana",Fp2$species)


aa<-ggplot(PGDD2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=.draw))+geom_smooth(method="lm")+ylab("Thermal growing season \n(primary)")+
  ggthemes::theme_few()

PGGD2<-filter(PGDD,spp %in% order)
PGGD2$species<-NA
PGGD2$species<-ifelse(PGGD2$spp=="SAMRAC","S. racemosa",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="VIBCAS","V. cassinoides",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="SPIALB","S. alba",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="AMECAN","A. canadensis",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="DIELON","D. lonicera",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="AROMEL","A. melanocarpa",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="SPITOM","S. tomentosa",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="BETPOP","B. populifolia",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="MYRGAL","M. gale",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="BETPAP","B. papyrifera",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="BETALL","B. alleghaniensis",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="ALNINC","A. incana",PGGD2$species)
PGGD2$species<-ifelse(PGGD2$spp=="SORAME","S. americana",PGGD2$species)


FGGD2<-filter(FGDD,spp %in% order)
FGGD2$species<-NA
FGGD2$species<-ifelse(FGGD2$spp=="SAMRAC","S. racemosa",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="VIBCAS","V. cassinoides",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="SPIALB","S. alba",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="AMECAN","A. canadensis",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="DIELON","D. lonicera",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="AROMEL","A. melanocarpa",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="SPITOM","S. tomentosa",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="BETPOP","B. populifolia",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="MYRGAL","M. gale",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="BETPAP","B. papyrifera",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="BETALL","B. alleghaniensis",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="ALNINC","A. incana",FGGD2$species)
FGGD2$species<-ifelse(FGGD2$spp=="SORAME","S. americana",FGGD2$species)


order<-c("S. racemosa","V. cassinoides","S. alba","A. canadensis","D. lonicera","A. melanocarpa","S. tomentosa","B. populifolia","M. gale","B. papyrifera","B. alleghaniensis","A. incana","S. americana")

cc<-ggplot(PGGD2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("")+xlab("leafout anomaly \n(days)")+
  ggthemes::theme_few(base_size = 11)+facet_wrap(~factor(species, levels=order)) + theme(strip.text = element_text(face = "italic"))

dd<-ggplot(Pp2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("")+xlab("")+
  ggthemes::theme_few(base_size = 11)+facet_wrap(~factor(species, levels=order)) + theme(strip.text = element_text(face = "italic"))



ggpubr::ggarrange(cc,dd,ncol=1)
ggpubr::ggarrange(c,d,ncol=1)





aa<-ggplot()+geom_point(data=cg1,aes(x=leafout_cent,pgs),size=0.1)+
geom_line(data=P2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=P2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Primary calander growing season")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("")


bb<-ggplot()+geom_point(data=cg1,aes(x=leafout_cent,pgsGDD),size=0.1)+
  geom_line(data=PGDD2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=PGDD2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Primary thermal growing season")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("SoS anomaly \n(days)")


## This is figure 3
jpeg("figures/primarygrowingseason_modplots.jpeg", height=8,width=9,unit='in',res=200)
ggpubr::ggarrange(aa,dd,bb,cc,labels = c("a)","b)","c","d)"))
dev.off()

####This is figure Suppliment)

a<-ggplot()+geom_point(data=cg2,aes(x=leafout_cent,fgs),size=0.1)+
  geom_line(data=F2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=F2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Full calander growing season")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("")

b<-ggplot()+geom_point(data=cg2,aes(x=leafout_cent,fgsGDD),size=0.1)+
  geom_line(data=FGDD2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=FGDD2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Full thermal growing season")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("SoS anomaly \n(days)")

c<-ggplot(FGGD2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("")+xlab("leafout anomaly \n(days)")+
  ggthemes::theme_few(base_size = 11)+facet_wrap(~factor(species, levels=order)) + theme(strip.text = element_text(face = "italic"))

d<-ggplot(Fp2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("")+xlab("")+
  ggthemes::theme_few(base_size = 11)+facet_wrap(~factor(species, levels=order)) + theme(strip.text = element_text(face = "italic"))


jpeg("figures/fullgrowingseason_modplots.jpeg", height=8,width=9,unit='in',res=200)
ggpubr::ggarrange(a,d,b,c,labels = c("a)","b)","c","d)"))
dev.off()




ee<-ggplot(cg,aes(budset,leafout))+geom_jitter(width=3,height=3,size=.5)+geom_smooth(method='lm')+ggthemes::theme_few()+ylab("SoS")+xlab("EoS \n(primary growth)")


ff<-ggplot(cg,aes(leafcolor,leafout))+geom_jitter(width=3,height=3,size=.5)+geom_smooth(method='lm')+ggthemes::theme_few()+ylab("SoS")+xlab("EoS \n(secondary growth)")
cor.test(cg$leafout,cg$leafcolor)


jpeg("figures/SoSEoS.jpeg", height=8,width=7,unit='in',res=200)
ggpubr::ggarrange(ee,ff,ncol=1)
dev.off()

### next variance partitiional models
####
use.data<-dplyr::filter(cg1,!is.na(pgs))
cor.test(use.data$leafout,use.data$budset)
round(cor.test(use.data$leafout,use.data$budset)[4]$estimate,2)
round(cor.test(use.data$leafout,use.data$budset)[9]$conf.int[1],2)
round(cor.test(use.data$leafout,use.data$budset)[9]$conf.int[2],2)

use.data2<-dplyr::filter(cg2,!is.na(fgs))


###note you can ignore treedepth warnings as it affects model run efficiency not the posterior estimates
#### leafout varience partitioning
mod.lo<-brm(leafout~(1|spp)+(1|site)+(1|year),data=use.data,
               warmup=4000,iter=5000, control=list(adapt_delta=.99))

use.data$interval_leafout<-ifelse(use.data$year==2020,use.data$leafout-7,use.data$leafout-3.5)

mod.lo.censor<-brm(bf(interval_leafout|cens('interval',leafout)~(1|spp)+(1|site)+(1|year)),data=use.data,
            warmup=4000,iter=5000, control=list(adapt_delta=.99))

summary(mod.lo)[17]$random$spp[3]
summary(mod.lo)[17]$random$year[1]

gumbo<-summary(mod.lo)

summary(mod.lo)
summary(mod.lo.censor)
### budset variance partitioning
mod.bs<-brm(budset~(1|spp)+(1|site)+(1|year),data=use.data,
            warmup=4000,iter=5000, control=list(adapt_delta=.99)) 

use.data$interval_budeset<-ifelse(use.data$year==2020,use.data$budset-7,use.data$budset-3)

prior_summary(mod.bs.cens)

mod.bs.cens<-brm(bf(interval_budeset2|cens('interval',budset2)~(1|spp)+(1|site)+(1|year)),data=use.data,
            warmup=4000,iter=5000, control=list(adapt_delta=.99),chains=1)

use.data$interval_budeset2<-use.data$interval_budeset-100
use.data$budset2<-use.data$budset-100

summary(mod.bs)[17]$random$spp

summary(mod.lo)[17]$random$year[1]


#primary growing season variance partitioning
mod.pgs<-brm(pgs~(1|spp)+(1|site)+(1|year),data=use.data,
               warmup=4000,iter=5000, control=list(adapt_delta=.995))

mod.pgsGDD<-brm(pgsGDD~(1|spp)+(1|site)+(1|year),data=use.data,
             warmup=4000,iter=5000, control=list(adapt_delta=.995))


spsum<-use.data %>% group_by(spp) %>% summarise(mean_lo=mean(leafout),mean_bs=mean(budset)) %>% arrange(mean_lo)

fmod.lo<-brm(leafout~(1|spp)+(1|site)+(1|year),data=use.data2,
            warmup=4000,iter=5000, control=list(adapt_delta=.995))

fmod.lc<-brm(leafcolor~(1|spp)+(1|site)+(1|year),data=use.data2,
            warmup=4000,iter=5000, control=list(adapt_delta=.99)) 

ranef(fmod.lc)
ranef(mod.bs)

mod.fgs<-brm(fgs~(1|spp)+(1|site)+(1|year),data=use.data2,
             warmup=4000,iter=5000, control=list(adapt_delta=.995))

mod.fgsGDD<-brm(fgsGDD~(1|spp)+(1|site)+(1|year),data=use.data2,
             warmup=4000,iter=5000, control=list(adapt_delta=.995))

##make leafout plots
bb.sppout<-mod.lo %>% spread_draws(r_spp[spp,Intercept])
bb.sppout$species<-NA
bb.sppout$species<-ifelse(bb.sppout$spp=="SAMRAC","S. racemosa",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="VIBCAS","V. cassinoides",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="SPIALB","S. alba",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="AMECAN","A. canadensis",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="DIELON","D. lonicera",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="AROMEL","A. melanocarpa",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="SPITOM","S. tomentosa",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="BETPOP","B. populifolia",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="MYRGAL","M. gale",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="BETPAP","B. papyrifera",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="BETALL","B. alleghaniensis",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="ALNINC","A. incana",bb.sppout$species)
bb.sppout$species<-ifelse(bb.sppout$spp=="SORAME","S. americana",bb.sppout$species)
bb.sppout<-filter(bb.sppout, species %in% order)

bs.sppout<-mod.bs %>% spread_draws(r_spp[spp,Intercept])
bs.sppout$species<-NA
bs.sppout$species<-ifelse(bs.sppout$spp=="SAMRAC","S. racemosa",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="VIBCAS","V. cassinoides",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="SPIALB","S. alba",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="AMECAN","A. canadensis",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="DIELON","D. lonicera",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="AROMEL","A. melanocarpa",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="SPITOM","S. tomentosa",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="BETPOP","B. populifolia",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="MYRGAL","M. gale",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="BETPAP","B. papyrifera",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="BETALL","B. alleghaniensis",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="ALNINC","A. incana",bs.sppout$species)
bs.sppout$species<-ifelse(bs.sppout$spp=="SORAME","S. americana",bs.sppout$species)
bs.sppout<-filter(bs.sppout, species %in% order)

pgs.sppout<-mod.pgs %>% spread_draws(r_spp[spp,Intercept])
pgs.sppout$species<-NA
pgs.sppout$species<-ifelse(pgs.sppout$spp=="SAMRAC","S. racemosa",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="VIBCAS","V. cassinoides",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="SPIALB","S. alba",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="AMECAN","A. canadensis",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="DIELON","D. lonicera",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="AROMEL","A. melanocarpa",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="SPITOM","S. tomentosa",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="BETPOP","B. populifolia",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="MYRGAL","M. gale",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="BETPAP","B. papyrifera",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="BETALL","B. alleghaniensis",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="ALNINC","A. incana",pgs.sppout$species)
pgs.sppout$species<-ifelse(pgs.sppout$spp=="SORAME","S. americana",pgs.sppout$species)
pgs.sppout<-filter(pgs.sppout, species %in% order)

GDDpgs.sppout<-mod.pgsGDD %>% spread_draws(r_spp[spp,Intercept])
GDDpgs.sppout$species<-NA
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="SAMRAC","S. racemosa",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="VIBCAS","V. cassinoides",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="SPIALB","S. alba",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="AMECAN","A. canadensis",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="DIELON","D. lonicera",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="AROMEL","A. melanocarpa",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="SPITOM","S. tomentosa",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="BETPOP","B. populifolia",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="MYRGAL","M. gale",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="BETPAP","B. papyrifera",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="BETALL","B. alleghaniensis",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="ALNINC","A. incana",GDDpgs.sppout$species)
GDDpgs.sppout$species<-ifelse(GDDpgs.sppout$spp=="SORAME","S. americana",GDDpgs.sppout$species)
GDDpgs.sppout<-filter(GDDpgs.sppout, species %in% order)


####now full growing season

fbb.sppout<-fmod.lo %>% spread_draws(r_spp[spp,Intercept])
fbb.sppout$species<-NA
fbb.sppout$species<-ifelse(fbb.sppout$spp=="SAMRAC","S. racemosa",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="VIBCAS","V. cassinoides",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="SPIALB","S. alba",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="AMECAN","A. canadensis",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="DIELON","D. lonicera",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="AROMEL","A. melanocarpa",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="SPITOM","S. tomentosa",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="BETPOP","B. populifolia",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="MYRGAL","M. gale",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="BETPAP","B. papyrifera",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="BETALL","B. alleghaniensis",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="ALNINC","A. incana",fbb.sppout$species)
fbb.sppout$species<-ifelse(fbb.sppout$spp=="SORAME","S. americana",fbb.sppout$species)
fbb.sppout<-filter(fbb.sppout, species %in% order)


lc.sppout<-fmod.lc %>% spread_draws(r_spp[spp,Intercept])
lc.sppout$species<-NA
lc.sppout$species<-ifelse(lc.sppout$spp=="SAMRAC","S. racemosa",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="VIBCAS","V. cassinoides",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="SPIALB","S. alba",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="AMECAN","A. canadensis",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="DIELON","D. lonicera",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="AROMEL","A. melanocarpa",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="SPITOM","S. tomentosa",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="BETPOP","B. populifolia",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="MYRGAL","M. gale",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="BETPAP","B. papyrifera",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="BETALL","B. alleghaniensis",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="ALNINC","A. incana",lc.sppout$species)
lc.sppout$species<-ifelse(lc.sppout$spp=="SORAME","S. americana",lc.sppout$species)
lc.sppout<-filter(lc.sppout, species %in% order)


fgs.sppout<-mod.fgs %>% spread_draws(r_spp[spp,Intercept])
fgs.sppout$species<-NA
fgs.sppout$species<-ifelse(fgs.sppout$spp=="SAMRAC","S. racemosa",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="VIBCAS","V. cassinoides",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="SPIALB","S. alba",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="AMECAN","A. canadensis",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="DIELON","D. lonicera",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="AROMEL","A. melanocarpa",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="SPITOM","S. tomentosa",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="BETPOP","B. populifolia",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="MYRGAL","M. gale",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="BETPAP","B. papyrifera",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="BETALL","B. alleghaniensis",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="ALNINC","A. incana",fgs.sppout$species)
fgs.sppout$species<-ifelse(fgs.sppout$spp=="SORAME","S. americana",fgs.sppout$species)
fgs.sppout<-filter(fgs.sppout, species %in% order)


GDDfgs.sppout<-mod.fgsGDD %>% spread_draws(r_spp[spp,Intercept])
GDDfgs.sppout$species<-NA
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="SAMRAC","S. racemosa",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="VIBCAS","V. cassinoides",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="SPIALB","S. alba",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="AMECAN","A. canadensis",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="DIELON","D. lonicera",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="AROMEL","A. melanocarpa",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="SPITOM","S. tomentosa",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="BETPOP","B. populifolia",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="MYRGAL","M. gale",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="BETPAP","B. papyrifera",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="BETALL","B. alleghaniensis",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="ALNINC","A. incana",GDDfgs.sppout$species)
GDDfgs.sppout$species<-ifelse(GDDfgs.sppout$spp=="SORAME","S. americana",GDDfgs.sppout$species)
GDDfgs.sppout<-filter(GDDfgs.sppout, species %in% order)



loplot<-ggplot(bb.sppout,aes(r_spp,reorder(species,r_spp)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+xlim(-15,15)+
  xlab("leafout")+
  ylab("")+ggthemes::theme_few()+theme(axis.text.y = element_text(face = "italic"))

floplot<-ggplot(fbb.sppout,aes(r_spp,species))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+xlim(-15,15)+
  xlab("leafout")+
  ylab("")+ggthemes::theme_few()+theme(axis.text.y = element_text(face = "italic"))+
  scale_y_discrete(name="",limits=order)


bb.sppout$growing_season<-"primary"
fbb.sppout$growing_season<-"full"

start.sppout<-rbind(bb.sppout,fbb.sppout)

splot<-ggplot(start.sppout,aes(r_spp,species))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlim(-20,15)+xlab("start phase")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+
  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)



bsplot<-ggplot(bs.sppout,aes(r_spp,species))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlim(-30,20)+xlab("budset")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())

lcplot<-ggplot(lc.sppout,aes(r_spp,species))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlim(-30,20)+xlab("leaf color")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())

bs.sppout$growing_season<-"primary"
lc.sppout$growing_season<-"full"

end.sppout<-rbind(bs.sppout,lc.sppout)

eplot<-ggplot(end.sppout,aes(r_spp,species))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlim(-30,20)+xlab("end phase")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())+
  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)

pgs.sppout$growing_season<-"primary"
fgs.sppout$growing_season<-"full"

gs.sppout<-rbind(pgs.sppout,fgs.sppout)
gplot<-ggplot(gs.sppout,aes(r_spp,species))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlim(-30,30)+xlab("calendar growing season")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())+
  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)
                                                                     
GDDpgs.sppout$growing_season<-"primary"
GDDfgs.sppout$growing_season<-"full"
GDDgs.sppout<-rbind(GDDpgs.sppout,GDDfgs.sppout)

ggplot<-ggplot(GDDgs.sppout,aes(r_spp,species))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlim(-200,200)+xlab("thermal growing season")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())+
  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)

speciesVar<-ggpubr::ggarrange(splot,eplot,gplot,ggplot,nrow=1,common.legend = TRUE,widths = c(3,2,2,2))



plotdur<-ggplot(pgs.sppout,aes(r_spp,species))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("primary calendar growing season")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())
plotdurGDD<-ggplot(GDDpgs.sppout,aes(r_spp,species))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("primary thermal growing season")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())


plotdurF<-ggplot(fgs.sppout,aes(r_spp,species))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("full calendar growing season")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())

plotdurGDDF<-ggplot(GDDfgs.sppout,aes(r_spp,species))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("full thermal growing season")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)+theme(axis.text.y = element_blank(),
                                                                     axis.ticks.y = element_blank())


spcis<-ggpubr::ggarrange(loplot,bsplot,plotdur,plotdurGDD,ncol=4,widths = c(3,2,2,2))
spcisF<-ggpubr::ggarrange(floplot,lcplot,plotdurF,plotdurGDDF,ncol=4,widths = c(3,2,2,2))


###populate effects
frostfree<-c("Harvard Forest, MA, USA","Saint Hippolyte, QC, CA","White Mountains, NH, USA","Second College Grant, NH, USA")

lo.siteout<-mod.lo %>% spread_draws(r_site[site,Intercept])
lo.siteout$population<-NA
lo.siteout$population<-ifelse(lo.siteout$site=="WM","White Mountains, NH, USA",lo.siteout$population)
lo.siteout$population<-ifelse(lo.siteout$site=="HF","Harvard Forest, MA, USA",lo.siteout$population)
lo.siteout$population<-ifelse(lo.siteout$site=="GR","Second College Grant, NH, USA",lo.siteout$population)
lo.siteout$population<-ifelse(lo.siteout$site=="SH","Saint Hippolyte, QC, CA",lo.siteout$population)


flo.siteout<-fmod.lo %>% spread_draws(r_site[site,Intercept])
flo.siteout$population<-NA
flo.siteout$population<-ifelse(flo.siteout$site=="WM","White Mountains, NH, USA",flo.siteout$population)
flo.siteout$population<-ifelse(flo.siteout$site=="HF","Harvard Forest, MA, USA",flo.siteout$population)
flo.siteout$population<-ifelse(flo.siteout$site=="GR","Second College Grant, NH, USA",flo.siteout$population)
flo.siteout$population<-ifelse(flo.siteout$site=="SH","Saint Hippolyte, QC, CA",flo.siteout$population)


bs.siteout<-mod.bs %>% spread_draws(r_site[site,Intercept])

bs.siteout$population<-NA
bs.siteout$population<-ifelse(bs.siteout$site=="WM","White Mountains, NH, USA",bs.siteout$population)
bs.siteout$population<-ifelse(bs.siteout$site=="HF","Harvard Forest, MA, USA",bs.siteout$population)
bs.siteout$population<-ifelse(bs.siteout$site=="GR","Second College Grant, NH, USA",bs.siteout$population)
bs.siteout$population<-ifelse(bs.siteout$site=="SH","Saint Hippolyte, QC, CA",bs.siteout$population)

lc.siteout<-fmod.lc %>% spread_draws(r_site[site,Intercept])

lc.siteout$population<-NA
lc.siteout$population<-ifelse(lc.siteout$site=="WM","White Mountains, NH, USA",lc.siteout$population)
lc.siteout$population<-ifelse(lc.siteout$site=="HF","Harvard Forest, MA, USA",lc.siteout$population)
lc.siteout$population<-ifelse(lc.siteout$site=="GR","Second College Grant, NH, USA",lc.siteout$population)
lc.siteout$population<-ifelse(lc.siteout$site=="SH","Saint Hippolyte, QC, CA",lc.siteout$population)


pgs.siteout<-mod.pgs %>% spread_draws(r_site[site,Intercept])
pgs.siteout$population<-NA
pgs.siteout$population<-ifelse(pgs.siteout$site=="WM","White Mountains, NH, USA",pgs.siteout$population)
pgs.siteout$population<-ifelse(pgs.siteout$site=="HF","Harvard Forest, MA, USA",pgs.siteout$population)
pgs.siteout$population<-ifelse(pgs.siteout$site=="GR","Second College Grant, NH, USA",pgs.siteout$population)
pgs.siteout$population<-ifelse(pgs.siteout$site=="SH","Saint Hippolyte, QC, CA",pgs.siteout$population)


fgs.siteout<-mod.fgs %>% spread_draws(r_site[site,Intercept])
fgs.siteout$population<-NA
fgs.siteout$population<-ifelse(fgs.siteout$site=="WM","White Mountains, NH, USA",fgs.siteout$population)
fgs.siteout$population<-ifelse(fgs.siteout$site=="HF","Harvard Forest, MA, USA",fgs.siteout$population)
fgs.siteout$population<-ifelse(fgs.siteout$site=="GR","Second College Grant, NH, USA",fgs.siteout$population)
fgs.siteout$population<-ifelse(fgs.siteout$site=="SH","Saint Hippolyte, QC, CA",fgs.siteout$population)



GDDpgs.siteout<-mod.pgsGDD %>% spread_draws(r_site[site,Intercept])
GDDpgs.siteout$population<-NA
GDDpgs.siteout$population<-ifelse(GDDpgs.siteout$site=="WM","White Mountains, NH, USA",GDDpgs.siteout$population)
GDDpgs.siteout$population<-ifelse(GDDpgs.siteout$site=="HF","Harvard Forest, MA, USA",GDDpgs.siteout$population)
GDDpgs.siteout$population<-ifelse(GDDpgs.siteout$site=="GR","Second College Grant, NH, USA",GDDpgs.siteout$population)
GDDpgs.siteout$population<-ifelse(GDDpgs.siteout$site=="SH","Saint Hippolyte, QC, CA",GDDpgs.siteout$population)

GDDfgs.siteout<-mod.fgsGDD %>% spread_draws(r_site[site,Intercept])
GDDfgs.siteout$population<-NA
GDDfgs.siteout$population<-ifelse(GDDfgs.siteout$site=="WM","White Mountains, NH, USA",GDDfgs.siteout$population)
GDDfgs.siteout$population<-ifelse(GDDfgs.siteout$site=="HF","Harvard Forest, MA, USA",GDDfgs.siteout$population)
GDDfgs.siteout$population<-ifelse(GDDfgs.siteout$site=="GR","Second College Grant, NH, USA",GDDfgs.siteout$population)
GDDfgs.siteout$population<-ifelse(GDDfgs.siteout$site=="SH","Saint Hippolyte, QC, CA",GDDfgs.siteout$population)


lo.siteout$growing_season<-"primary"
flo.siteout$growing_season<-"full"
start.siteout<-rbind(lo.siteout,flo.siteout)

bs.siteout$growing_season<-"primary"
lc.siteout$growing_season<-"full"
end.siteout<-rbind(bs.siteout,lc.siteout)

pgs.siteout$growing_season<-"primary"
fgs.siteout$growing_season<-"full"
gs.siteout<-rbind(pgs.siteout,fgs.siteout)

GDDpgs.siteout$growing_season<-"primary"
GDDfgs.siteout$growing_season<-"full"
GDDgs.siteout<-rbind(GDDpgs.siteout,GDDfgs.siteout)




ssite<-ggplot(start.siteout,aes(r_site,population))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("start phase")+
  ylab("")+coord_cartesian(xlim = c(-2,2))+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(legend.position = "none")


esite<-ggplot(end.siteout,aes(r_site,population))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("end phase")+
  ylab("")+coord_cartesian(xlim = c(-10,10))+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())+scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(legend.position = "none")


gsite<-ggplot(gs.siteout,aes(r_site,population))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("calendar growing season")+
  ylab("")+coord_cartesian(xlim = c(-10,10))+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())+
  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(legend.position = "none")

ggsite<-ggplot(GDDgs.siteout,aes(r_site,population))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("thermal growing season")+
  ylab("")+ coord_cartesian(xlim = c(-100,100))+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())+
  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(legend.position = "none")

sitesVar<-ggpubr::ggarrange(ssite,esite,gsite,ggsite,nrow=1,widths = c(4,2,2,2))


losite<-ggplot(lo.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leafout")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))

flosite<-ggplot(flo.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leafout")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))


bssite<-ggplot(bs.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("budset")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())

lcsite<-ggplot(lc.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leaf color")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())


pgssite<-ggplot(pgs.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("primary calendar growing season")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())


fgssite<-ggplot(fgs.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("full calendar growing season")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())


GDDpgssite<-ggplot(GDDpgs.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("primary thermal growing season")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())

GDDfgssite<-ggplot(GDDfgs.siteout,aes(r_site,population))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("full thermal growing season")+
  ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+theme(axis.text.y = element_blank(),
                                                                              axis.ticks.y = element_blank())

sties<-ggpubr::ggarrange(losite,bssite,pgssite,GDDpgssite,ncol=4,widths = c(4,2,2,2))
sties2<-ggpubr::ggarrange(flosite,lcsite,fgssite,GDDfgssite,ncol=4,widths = c(4,2,2,2))

lo.yearout<-mod.lo %>% spread_draws(r_year[year,Intercept])
bs.yearout<-mod.bs %>% spread_draws(r_year[year,Intercept])
pgs.yearout<-mod.pgs %>% spread_draws(r_year[year,Intercept])
GDDpgs.yearout<-mod.pgsGDD %>% spread_draws(r_year[year,Intercept])

flo.yearout<-fmod.lo %>% spread_draws(r_year[year,Intercept])
lc.yearout<-fmod.lc %>% spread_draws(r_year[year,Intercept])
fgs.yearout<-mod.fgs %>% spread_draws(r_year[year,Intercept])
GDDfgs.yearout<-mod.fgsGDD %>% spread_draws(r_year[year,Intercept])


yorder<-c("2018","2019","2020")


lo.yearout$growing_season<-"primary"
flo.yearout$growing_season<-"full"
syearout<-rbind(lo.yearout,flo.yearout)

syear<-ggplot(syearout,aes(r_year,as.factor(year)))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("start phase")+
  ylab("")+xlim(-30,30)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(legend.position = "none")

bs.yearout$growing_season<-"primary"
lc.yearout$growing_season<-"full"
eyearout<-rbind(bs.yearout,lc.yearout)

eyear<-ggplot(eyearout,aes(r_year,as.factor(year)))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("end phase")+
  ylab("")+xlim(-70,60)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(axis.text.y = element_blank(),
                                                                          axis.ticks.y = element_blank())+theme(legend.position = "none")


pgs.yearout$growing_season<-"primary"
fgs.yearout$growing_season<-"full"
gyearout<-rbind(pgs.yearout,fgs.yearout)

gyear<-ggplot(gyearout,aes(r_year,as.factor(year)))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("calendar growing season")+
  ylab("")+xlim(-40,40)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(axis.text.y = element_blank(),
                                                                          axis.ticks.y = element_blank())+theme(legend.position = "none")


GDDpgs.yearout$growing_season<-"primary"
GDDfgs.yearout$growing_season<-"full"
ggyearout<-rbind(GDDpgs.yearout,GDDfgs.yearout)


ggyear<-ggplot(ggyearout,aes(r_year,as.factor(year)))+
  stat_halfeye(aes(fill=growing_season,color=growing_season),.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("thermal growing season")+
  ylab("")+xlim(-700,600)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+  scale_fill_viridis_d(name=" growing season",begin = .4,end=.8)+
  scale_color_viridis_d(name=" growing season",begin = .45,end=.85)+theme(axis.text.y = element_blank(),
                                                                          axis.ticks.y = element_blank())+theme(legend.position = "none")

yearVar<-ggpubr::ggarrange(syear,eyear,gyear,ggyear,nrow=1,widths = c(2.5,2,2,2))

jpeg("figures/bothseasons_var_parts.jpeg", height=8,width=11,unit='in',res=200)
ggpubr::ggarrange(speciesVar,sitesVar,yearVar,common.legend = TRUE,ncol=1,heights=c(2,1,1))
dev.off()

loyear<-ggplot(lo.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leafout")+
  ylab("")+xlim(-40,40)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))


floyear<-ggplot(flo.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leafout")+
  ylab("")+xlim(-40,40)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))

bsyear<-ggplot(bs.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("budset")+
  ylab("")+xlim(-40,40)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+theme(axis.text.y = element_blank(),
                                                                           axis.ticks.y = element_blank())

lcyear<-ggplot(lc.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leaf color")+
  ylab("")+xlim(-70,60)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+theme(axis.text.y = element_blank(),
                                                                           axis.ticks.y = element_blank())

pgsyear<-ggplot(pgs.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("primary calendar growing season")+
  ylab("")+xlim(-20,20)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+theme(axis.text.y = element_blank(),
                                                                           axis.ticks.y = element_blank())

fgsyear<-ggplot(fgs.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("full calendar growing season")+
  ylab("")+xlim(-40,40)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+theme(axis.text.y = element_blank(),
                                                                           axis.ticks.y = element_blank())
                                                                           
pgsyearGDD<-ggplot(GDDpgs.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("primary thermal growing season")+
  ylab("")+xlim(-400,400)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+theme(axis.text.y = element_blank(),
                                                                           axis.ticks.y = element_blank())

fgsyearGDD<-ggplot(GDDfgs.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5,.9),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("thermal growing season")+
  ylab("")+xlim(-600,600)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+theme(axis.text.y = element_blank(),
                                                                           axis.ticks.y = element_blank())



yrs<-ggpubr::ggarrange(loyear,bsyear,pgsyear,pgsyearGDD,ncol=4)

yrs2<-ggpubr::ggarrange(floyear,lcyear,fgsyear,fgsyearGDD,ncol=4)
## current figure 2
jpeg("figures/var_parts.jpeg", height=8,width=7,unit='in',res=200)
ggpubr::ggarrange(spcis,sties,yrs,ncol=1,labels=c("a)","b)","c)"),heights=c(2,1.2,1))
dev.off()

jpeg("figures/full_var_parts.jpeg", height=8,width=7,unit='in',res=200)
ggpubr::ggarrange(spcisF,sties2,yrs2,ncol=1,labels=c("a)","b)","c)"),heights=c(2,1.2,1))
dev.off()

save.image("cgseasonmods.Rda")
new.daterz<-data.frame(spp=rep(unique(bb.sppout$spp),each=4),site=rep(unique(lo.siteout$site),13))

lopred<- mod.lo %>% 
  epred_draws(newdata =new.daterz,ndraws = 1000,re_formula = ~(1|spp)+(1|site))

new.daterz2<-data.frame(year=rep(unique(lo.yearout$year),each=4),site=rep(unique(lo.siteout$site),3))

lopred<- mod.lo %>% 
  epred_draws(newdata =new.daterz,ndraws = 1000,re_formula = ~(1|spp)+(1|site))

lopred2<- mod.lo %>% 
  epred_draws(newdata=new.daterz2,ndraws = 1000,re_formula = ~(1|year)+(1|site))


bspred<- mod.bs %>% 
  epred_draws(newdata =new.daterz,ndraws = 1000,re_formula = ~(1|spp)+(1|site))

bspred2<- mod.bs %>% 
  epred_draws(newdata =new.daterz2,ndraws = 1000,re_formula = ~(1|year)+(1|site))

library(tidybayes)
pd<-position_dodge(.5)

raw1<-ggplot()+stat_summary(data=use.data,aes(x=spp,y=leafout,color=site),position=pd)+
geom_point(data=use.data,aes(x=spp,y=leafout,color=site),position=pd,size=.1)+coord_cartesian(ylim=c(110,150))+
  xlab("")+ggthemes::theme_few()+scale_colour_viridis_d()

raw2<-ggplot()+stat_summary(data=use.data,aes(x=spp,y=budset,color=site),position=pd)+
  geom_point(data=use.data,aes(x=spp,y=budset,color=site),position=pd,size=.1)+coord_cartesian(ylim=c(230,290))+
  xlab("")+ggthemes::theme_few()+scale_colour_viridis_d()

jpeg("figures/rawpopulations.jpeg",width = 12,height=6,unit='in',res=200)
ggpubr::ggarrange(raw1,raw2,ncol=1,common.legend=TRUE)
dev.off()
tag<-paste(use.data$spp,use.data$site)
tag<-as.data.frame(tag)
tagy<-distinct(tag)

lopred$tag<-paste(lopred$spp,lopred$site)
lopred<-filter(lopred,tag %in% c(tagy$tag))

#lopred2$tag<-paste(lopred2$spp,lopred2$site)
#lopred2<-filter(lopred2,tag %in% c(tagy$tag))


bspred$tag<-paste(bspred$spp,bspred$site)
bspred<-filter(bspred,tag %in% c(tagy$tag))

#bspred2$tag<-paste(bspred2$spp,bspred2$site)
#bspred2<-filter(bspred2,tag %in% c(tagy$tag))

pop1<-ggplot()+
  stat_pointinterval(data=lopred,aes(x=spp,y=.epred,color=site),.width = c(.5,.9),position=pd)+coord_cartesian(ylim=c(110,150))+ylab("leafout")+
  xlab("")+ggthemes::theme_few()+scale_colour_viridis_d()

pop1a<-ggplot()+
  stat_pointinterval(data=lopred2,aes(x=0,y=.epred,shape=site,color=as.factor(year)),.width = c(.5,.9),position=pd)+coord_cartesian(ylim=c(110,150))+ylab("leafout")+
  xlab("")+ggthemes::theme_few()+scale_colour_viridis_d()+scale_x_discrete()


pop2<-ggplot()+
  stat_pointinterval(data=bspred,aes(x=spp,y=.epred,color=site),.width = c(.5,.9),position=pd)+
  ylab("budset")+coord_cartesian(ylim=c(230,290))+
  xlab("")+ggthemes::theme_few()+scale_colour_viridis_d()

po21a<-ggplot()+
  stat_pointinterval(data=bspred2,aes(x=0,y=.epred,shape=site,color=as.factor(year)),.width = c(.5,.9),position=pd)+coord_cartesian(ylim=c(230,290))+ylab("buset")+
  xlab("")+ggthemes::theme_few()+scale_colour_viridis_d()+scale_x_discrete()

jpeg("figures/fittedpopulations.jpeg",width = 12,height=6,unit='in',res=200)
ggpubr::ggarrange(pop1,pop2,ncol=1,common.legend=TRUE)
dev.off()

jpeg("figures/fittedpopulations_x_year.jpeg",width = 6,height=6,unit='in',res=200)
ggpubr::ggarrange(pop1a,po21a,ncol=2,common.legend=TRUE)
dev.off()

 ggplot(lopred,aes(spp,.epred))
ggplot(bspred,aes(spp,.epred))+stat_pointinterval(aes(color=site),.width = c(.5),position=pd)

jpeg("figures/SPxPOP.jpeg")
ggplot(lopred,aes(site,.epred))+stat_pointinterval(.width = c(.5,.9))+facet_wrap(~spp,scales="free")
dev.off()
ggplot(bspred,aes(site,.epred))+stat_pointinterval(.width = c(.5,.9))+facet_wrap(~spp,scales="free")

### quesiton, does indiviudal variation cahnge things?
use.data$indy<-paste(use.data$spp,use.data$site,use.data$ind,use.data$plot)### give and unique identifier

mod.bs.2<-brm(budset~(1|indy)+(1|spp)+(1|site)+(1|year),data=use.data,
            warmup=4000,iter=5000, control=list(adapt_delta=.99)) 

mod.lo.2<-brm(leafout~(1|indy)+(1|spp)+(1|site)+(1|year),data=use.data,
              warmup=4000,iter=5000, control=list(adapt_delta=.99)) 
summary(mod.lo.2)
summary(mod.bs.2)

#some plots to try and understand how much buset varies across years for individuals. it seems like alot
bp<-filter(use.data,spp=="BETPOP")
ggplot(bp,aes(budset,indy,shape=as.factor(year)))+geom_point()

aln<-filter(use.data,spp=="ALNINC")
ggplot(aln,aes(budset,indy,shape=as.factor(year)))+geom_point()

pap<-filter(use.data,spp=="BETPAP")
ggplot(pap,aes(budset,indy,shape=as.factor(year)))+geom_point()

all<-filter(use.data,spp=="BETALL")
ggplot(all,aes(budset,indy,shape=as.factor(year)))+geom_point()

ron<-filter(use.data,spp=="AROMEL")
ggplot(ron,aes(budset,indy,shape=as.factor(year)))+geom_point()

sam<-filter(use.data,spp=="SAMRAC")
ggplot(sam,aes(budset,indy,shape=as.factor(year)))+geom_point()
