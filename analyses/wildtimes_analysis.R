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

# source("source/combineWeather.R")
# source("source/conceptualFigure.R")

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

## check basic marginal effects
conditional_effects(pgsGDD.mod)
conditional_effects(pgs.mod)

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

a<-ggplot(FGDD2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=.draw))+geom_smooth(method="lm")+ylab("Thermal growing season \n(full)")+
  ggthemes::theme_few()

c<-ggplot(FGDD,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("Thermal growing season \n(full)")+
  ggthemes::theme_few()+facet_wrap(~spp,ncol=9)

d<-ggplot(Fp,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("Calander growing season \n(full)")+
  ggthemes::theme_few()+facet_wrap(~spp,ncol=9)

ggpubr::ggarrange(d,c,ncol=1)

b<-ggplot(F2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=.draw))+geom_smooth(method="lm")+ylab("Calander growing season \n(full)")+
  ggthemes::theme_few()

ggpubr::ggarrange(b,a)

###same thing for primary growing season
PGDD<-epred_draws(pgsGDD.mod,newdata = new.data,ndraws = 1000)
Pp<-epred_draws(pgs.mod,newdata = new.data,ndraws = 1000)
PGDD2<-epred_draws(pgsGDD.mod,newdata = new.data,ndraws = 1000,re_formula = NA)
PGDD$grouper<-paste(PGDD$spp,PGDD$.draw)
Pp$grouper<-paste(Pp$spp,Pp$.draw)

P2<-epred_draws(pgs.mod,newdata = new.data,ndraws = 1000,re_formula = NA)

order<-c("SAMRAC","VIBCAS","SPIALB","AMECAN","DIELON","AROMEL","SPITOM","BETPOP","MYRGAL","BETPAP","BETALL","ALNINC","SORAME")

Pp2<-filter(Pp,spp %in% order)


aa<-ggplot(PGDD2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=.draw))+geom_smooth(method="lm")+ylab("Thermal growing season \n(primary)")+
  ggthemes::theme_few()

PGGD2<-filter(PGDD,spp %in% order)

cc<-ggplot(PGGD2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("Thermal growing season \n(primary)")+
  ggthemes::theme_few()+facet_wrap(~factor(spp, levels=order))

dd<-ggplot(Pp2,aes(leafout_cent,.epred))+geom_line(size=0.01,aes(group=grouper))+geom_smooth(method="lm")+
  ylab("Calander growing season \n(primary)")+
  ggthemes::theme_few()+facet_wrap(~factor(spp, levels=order))

ggpubr::ggarrange(cc,dd,ncol=1)

a<-ggplot()+geom_point(data=cg2,aes(x=leafout_cent,fgs),size=0.1)+
  geom_line(data=F2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=F2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Calander growing season \n(secondary growth)")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("SoS \n(deviation from mean)")

b<-ggplot()+geom_point(data=cg2,aes(x=leafout_cent,fgsGDD),size=0.1)+
  geom_line(data=FGDD2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=FGDD2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Thermal growing season \n(secondary growth)")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("SoS \n(deviation from mean)")

aa<-ggplot()+geom_point(data=cg1,aes(x=leafout_cent,pgs),size=0.1)+
geom_line(data=P2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=P2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Calander growing season \n(primary growth)")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("SoS \n(deviation from mean)")


bb<-ggplot()+geom_point(data=cg1,aes(x=leafout_cent,pgsGDD),size=0.1)+
  geom_line(data=PGDD2,size=0.01,aes(group=.draw,x=leafout_cent,y=.epred))+
  geom_smooth(data=PGDD2,aes(x=leafout_cent,y=.epred),method="lm")+ylab("Thermal growing season \n(primary growth)")+
  ggthemes::theme_few()+coord_cartesian(xlim=c(-20,20))+xlab("SoS \n(deviation from mean)")

## This is figure 3
jpeg("figures/primarygrowingseason_modplots.jpeg", height=8,width=7,unit='in',res=200)
ggpubr::ggarrange(aa,dd,bb,cc,labels = c("a)","b)","c","d)"))
dev.off()

ee<-ggplot(cg,aes(budset,leafout))+geom_jitter(width=3,height=3,size=.5)+geom_smooth(method='lm')+ggthemes::theme_few()+ylab("SoS")+xlab("EoS \n(primary growth)")


ff<-ggplot(cg,aes(leafcolor,leafout))+geom_jitter(width=3,height=3,size=.5)+geom_smooth(method='lm')+ggthemes::theme_few()+ylab("SoS")+xlab("EoS \n(secondary growth)")
cor.test(cg$leafout,cg$leafcolor)


jpeg("figures/SoSEoS.jpeg", height=8,width=7,unit='in',res=200)
ggpubr::ggarrange(ee,ff,ncol=1)
dev.off()

### next variance partitiional models

use.data<-dplyr::filter(cg,!is.na(pgs))
cor.test(use.data$leafout,use.data$budset)

###note you can ignore treedepth warnings as it affects model run efficiency not the posterior estimates
#### leafout varience partitioning
mod.lo<-brm(leafout~(1|spp)+(1|site)+(1|year),data=use.data,
               warmup=4000,iter=5000, control=list(adapt_delta=.99))
summary(mod.lo)

### budset variance partitioning
mod.bs<-brm(budset~(1|spp)+(1|site)+(1|year),data=use.data,
            warmup=4000,iter=5000, control=list(adapt_delta=.99)) 

#primary growing season variance partitioning
mod.pgs<-brm(pgs~(1|spp)+(1|site)+(1|year),data=use.data,
               warmup=4000,iter=5000, control=list(adapt_delta=.995))


##make leafout plots
bb.sppout<-mod.lo %>% spread_draws(r_spp[spp,Intercept])
bb.sppout<-filter(bb.sppout, spp %in% order)

bs.sppout<-mod.bs %>% spread_draws(r_spp[spp,Intercept])
bs.sppout<-filter(bs.sppout, spp %in% order)

pgs.sppout<-mod.pgs %>% spread_draws(r_spp[spp,Intercept])
pgs.sppout<-filter(pgs.sppout, spp %in% order)

loplot<-ggplot(bb.sppout,aes(r_spp,reorder(spp,r_spp)))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+xlim(-15,15)+
  xlab("leafout")+
  ylab("")+
  ggthemes::theme_few()

bsplot<-ggplot(bs.sppout,aes(r_spp,spp))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlim(-30,20)+xlab("budset")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)

plotdur<-ggplot(pgs.sppout,aes(r_spp,spp))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("growing season")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=order)

spcis<-ggpubr::ggarrange(loplot,bsplot,plotdur,ncol=3)

###populate effects
bb.sppout<-mod.lo %>% spread_draws(r_spp[spp,Intercept])

bb.sppout<-filter(bb.sppout, spp %in% order)

lo.siteout<-mod.lo %>% spread_draws(r_site[site,Intercept])
bs.siteout<-mod.bs %>% spread_draws(r_site[site,Intercept])
pgs.siteout<-mod.pgs %>% spread_draws(r_site[site,Intercept])

losite<-ggplot(lo.siteout,aes(r_site,site))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leafout")+
  ylab("")+
  ggthemes::theme_few()

bssite<-ggplot(bs.siteout,aes(r_site,site))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("budset")+
  ylab("")+
  ggthemes::theme_few()

pgssite<-ggplot(pgs.siteout,aes(r_site,site))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("growing season")+
  ylab("")+
  ggthemes::theme_few()

sties<-ggpubr::ggarrange(losite,bssite,pgssite,ncol=3)

lo.yearout<-mod.lo %>% spread_draws(r_year[year,Intercept])
bs.yearout<-mod.bs %>% spread_draws(r_year[year,Intercept])
pgs.yearout<-mod.pgs %>% spread_draws(r_year[year,Intercept])

loyear<-ggplot(lo.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("leafout")+
  ylab("")+
  ggthemes::theme_few()

bsyear<-ggplot(bs.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("budset")+
  ylab("")+
  ggthemes::theme_few()

pgsyear<-ggplot(pgs.yearout,aes(r_year,as.factor(year)))+
  stat_halfeye(.width = c(.5),alpha=0.6)+geom_vline(xintercept=0)+
  xlab("growing season")+
  ylab("")+
  ggthemes::theme_few()

yrs<-ggpubr::ggarrange(loyear,bsyear,pgsyear,ncol=3)

## current figure 2
jpeg("figures/var_parts.jpeg", height=8,width=7,unit='in',res=200)
ggpubr::ggarrange(spcis,sties,yrs,ncol=1,labels=c("a)","b)","c)"),heights=c(1.6,1.2,1))
dev.off()

save.image("cgseasonmods.Rda")

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
