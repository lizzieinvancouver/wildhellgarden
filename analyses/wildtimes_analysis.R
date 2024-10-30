## Loking at each major phase individually running stan
## Look at inter vs intra specific variation, is there local adaptation??
### Dan AMay 2022

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
library(tidybayes)

library(tidyr)
library(lubridate)
library("pollen")
# Set Working Directory

#load("cgseasonmods.Rda")

setwd("~/Documents/git/wildhellgarden/analyses")
cg <- read.csv("output/clean_obs_allyrs.csv")### this is the updated data with leaf color

###calculate primary growth season and full growing seasion in days
cg$pgs<-cg$budset-cg$leafout
cg$fgs<-cg$leafcolor-cg$leafout

###calculate gdds
weather<-read.csv("input/weldhill.csv") #from https://labs.arboretum.harvard.edu/weather/

weather<-filter(weather,grepl("2018|2019|2020",weather$Eastern.daylight.time)) ## pull the years we need


##get max and min T
goo<-weather %>% separate(Eastern.daylight.time, into = c('date', 'time'), sep = -8, convert = TRUE) ### make date and time seperate cols

goo<-goo%>% group_by(date) %>% summarise(maxTf=max(Temp..F ),minTf=min(Temp..F )) ###make daily mins and maxT

###whoops, The weld hill temperature data ends, use the closest weather station
more20<-read.csv("input/Daily Data for Boston (Weld Hill), MA - [id=ma_weld nwon, lat=42.2953, lon=-71.1337].csv")

###get ready to mush them together
goo2<-goo # make new copy df
goo2$date<-as.Date(goo2$date,"%m/%d/%Y") # make dates dates again
more20$date<-as.Date(more20$date,"%m/%d/%Y")

goo2$doy<-yday(goo2$date) ##convert to doy
more20$doy<-yday(more20$date) ##convert to doy
###put it in order
goo2<-goo2[order(goo2$date), ]
more20<-more20[order(more20$date), ]
colnames(more20)
more20<-dplyr::select(more20,date,Max.Air.Temp...F.,Min.Air.Temp...F.,doy)
colnames(goo2)
colnames(more20)<-colnames(goo2)
goo2<-rbind(goo2,more20)

goo2$maxTc<-(goo2$maxTf-32)*(5/9) ##convert to  C
goo2$minTc<-(goo2$minTf-32)*(5/9)

#new column for year
goo2$year<-substr(goo2$date, 1, 4)
###give each year a seperate data frame
goo18<-filter(goo2,year=="2018")
goo19<-filter(goo2,year=="2019")
goo20<-filter(goo2,year=="2020")



###Calculate gdd
goo18$GDD_10<- pollen::gdd(tmax = goo18$maxTc,tmin = goo18$minTc,tbase = 10,type = "B")
goo19$GDD_10<- pollen::gdd(tmax = goo19$maxTc,tmin = goo19$minTc,tbase = 10,type = "B")
goo20$GDD_10<- pollen::gdd(tmax = goo20$maxTc,tmin = goo20$minTc,tbase = 10,type = "B")
goober<-rbind(goo18,goo19,goo20)


#### below makes the concept figure 
if(FALSE){
delta<-diff(goo19$GDD_10)
delta<-c(0,delta)
 goo19$ggdperday<-delta

 early<-filter(goo19,doy %in% c(115:240))
 early$GDD_10<-early$GDD_10-early$GDD_10[1]
240-115
  142-115 #27 day diff
 253-240 #13
 27-13
 # day diff
 late<-filter(goo19,doy %in% c(142:253))
 late$GDD_10<-late$GDD_10-late$GDD_10[1]
 
all<-filter(goo19,doy %in% c(115:253))

early2<-filter(goo19,doy %in% c(127:240))
early2$GDD_10<-early2$GDD_10-early2$GDD_10[1]

late2<-filter(goo19,doy %in% c(147:245))
late2$GDD_10<-late2$GDD_10-late2$GDD_10[1]


##start a month differnce
##end a week difference
p1<-ggplot(data=all,aes(x=doy,y=ggdperday))+
  geom_rect(xmin=142,xmax=253, ymin=-2, ymax=0, alpha=0.02,fill="green3")+
geom_rect(xmin=115,xmax=240, ymin=-3, ymax=-1, alpha=0.02,fill="darkgreen")+
  geom_rect(xmin=147,xmax=245, ymin=-6, ymax=-4, alpha=0.02,fill="skyblue1")+
  geom_rect(xmin=127,xmax=240, ymin=-7, ymax=-5, alpha=0.02,fill="skyblue3")+
  
  geom_line()+ggthemes::theme_few()+ylab("GDD's per day")+ylim(-8,20)+xlab("day of season")
  

p2<-ggplot()+geom_line(data=early,aes(x=doy,y=GDD_10),color="darkgreen",size=2)+
geom_line(data=late,aes(x=doy,y=GDD_10),color="green3",size=2)+
  ggthemes::theme_few()+ylab("Cumulative GDD's")+xlab("day of season")
p3<-ggplot()+geom_line(data=early2,aes(x=doy,y=GDD_10),color="skyblue1",size=2)+
  geom_line(data=late2,aes(x=doy,y=GDD_10),color="skyblue3",size=2)+
  ggthemes::theme_few()+ylab("Cumulative GDD's")+xlab("day of season")


concept2<-ggpubr::ggarrange(p2,p3,labels=c("b)","c)"))
  
jpeg("figures/aronia_examp.jpeg", height=7,width=7,unit='in',res=200)
ggpubr::ggarrange(p1,concept2,ncol=1,labels=c("a)",""))
 dev.off()
}
 ####continue analysis 
 
#### remove NA's 
cg1<-filter(cg,!is.na(pgs)) #This is datasheet for "primary growing season (leafout to budset)

cg2<-filter(cg,!is.na(fgs)) #This is datasheet for "full growing season (leafout to leaf color)

###round any fraction doys to closest full doy 
cg1$leafout<-round(cg1$leafout)
cg2$leafout<-round(cg2$leafout) ### some were X.5 which messes up everything up


ref<-data.frame(year=cg1$year,start=cg1$leafout,end=cg1$budset)## a list of to Sos and EoS for each indivudal in each yeat
myvec<-c()


##this calulates the thermal growing season of primary growth for each indibidual
#his seems an important section for someone to check 
for(j in 1:length(cg1$pgs)){
temp<-filter(goober,year==cg1$year[j])
tempS<-filter(temp,doy==cg1$leafout[j])
tempE<-filter(temp,doy==cg1$budset[j])
             GDD<-tempE$GDD_10-tempS$GDD_10
   print(GDD)
              myvec<-c(myvec,GDD)
}



cg1$pgsGDD<-myvec ### add this to main dataframe

cg1$indy<-paste(cg1$spp,cg1$site,cg1$ind,cg1$plot)### give and unique identifier


##do some basic plots
if(FALSE){
ggplot(cg1,aes(pgsGDD,leafout))+geom_point()+geom_smooth(method="lm",se=FALSE)+facet_wrap(~spp)
ggplot(cg1,aes(pgs,leafout))+geom_point()+geom_smooth(method="lm",se=FALSE)+facet_wrap(~spp)

ggplot(cg1,aes(leafout,pgsGDD))+geom_point()+geom_smooth(method="lm")

ggplot(cg1,aes(pgsGDD,leafout))+geom_point(aes(color=spp))+geom_smooth(method="lm",aes(color=spp))
ggplot(cg1,aes(pgs,leafout))+geom_point(aes(color=spp))+geom_smooth(method="lm",aes(color=spp))
}


### now to the same thing for the full growing season
ref2<-data.frame(year=cg2$year,start=cg2$leafout,end=cg2$leafcolor)
myvec2<-c()


for(j in 1:length(cg2$fgs)){
  temp<-filter(goober,year==cg2$year[j])
  tempS<-filter(temp,doy==cg2$leafout[j])
  tempE<-filter(temp,doy==cg2$leafcolor[j])
  GDD<-tempE$GDD_10-tempS$GDD_10
  print(GDD)
  myvec2<-c(myvec2,GDD)
}


cg2$fgsGDD<-myvec2

if(FALSE){#more plots
jpeg("figures/gs_vs_leafout.jpeg",width=10,height=8,units = 'in',res=300)
ggpubr::ggarrange(ggplot(cg1,aes(leafout,pgsGDD))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),
ggplot(cg1,aes(leafout,pgs))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),
ggplot(cg2,aes(leafout,fgsGDD))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),
ggplot(cg2,aes(leafout,fgs))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),common.legend = TRUE)
dev.off()

ggpubr::ggarrange(ggplot(cg1,aes(pgsGDD,leafout))+geom_point(size=.4)+geom_smooth(method="lm",se=TRUE,size=1),
                  ggplot(cg1,aes(pgs,leafout))+geom_point(size=.4)+geom_smooth(method="lm",se=TRUE,size=1),
                  ggplot(cg2,aes(fgsGDD,leafout))+geom_point(size=.4)+geom_smooth(method="lm",se=TRUE,size=1),
                  ggplot(cg2,aes(fgs,leafout))+geom_point(size=.4)+geom_smooth(method="lm",se=TRUE,size=1),common.legend = TRUE)
dev.off()

cg1 %>% group_by(spp,year) %>% summarise(meanpgs=mean(pgs),meanpgsGDD=mean(pgsGDD))

ggplot(cg1,aes(pgs,pgsGDD))+geom_point()+geom_smooth(method="lm",aes(color=spp),se=FALSE)
ggplot(cg2,aes(fgs,fgsGDD))+geom_point()+geom_smooth(method="lm",aes(color=spp),se=FALSE)

jpeg("figures/gs_vs_leafout_site.jpeg",width=10,height=8,units = 'in',res=300)
ggpubr::ggarrange(ggplot(cg1,aes(leafout,pgsGDD))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),
                  ggplot(cg1,aes(leafout,pgs))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),
                  ggplot(cg2,aes(leafout,fgsGDD))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),
                  ggplot(cg2,aes(leafout,fgs))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),common.legend = TRUE)
dev.off()
}

####center variables
cg1$pgsGDD_cent<-cg1$pgsGDD-mean(cg1$pgsGDD)
cg1$leafout_cent<-cg1$leafout-mean(cg1$leafout)

cg_wgdd<-left_join(cg2,cg1)


ggplot(cg_wgdd,aes(leafcolor,budset))+geom_point()+geom_abline()


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



###make posterior predictons

new.data<-data.frame(leafout_cent=rep(c(-20,0,20),each=18),spp=rep(unique(cg1$spp),3)) ## generate a new data frame
library(tidybayes)
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


###note you can ignore treedepth warnings as it affects model run effieiceny not the posterior estimates
#### leafout varience partitioning
mod.lo<-brm(leafout~(1|spp)+(1|site)+(1|year),data=use.data,
               warmup=4000,iter=5000, control=list(adapt_delta=.99))
summary(mod.lo)

### budset varience partitioning
mod.bs<-brm(budset~(1|spp)+(1|site)+(1|year),data=use.data,
            warmup=4000,iter=5000, control=list(adapt_delta=.99)) 

#primarny gorwing season variaence parittioning
mod.pgs<-brm(pgs~(1|spp)+(1|site)+(1|year),data=use.data,
               warmup=4000,iter=5000, control=list(adapt_delta=.995))


##bake leafout plots
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
