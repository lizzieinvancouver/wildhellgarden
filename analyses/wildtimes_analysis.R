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
setwd("~/Documents/git/wildhellgarden/analyses")
cg <- read.csv("output/clean_obs_allyrs.csv")### this is the updated data with leaf color

###calculate primary growth season and full growing seasion in days
cg$pgs<-cg$budset-cg$leafout
cg$fgs<-cg$leafcolor-cg$leafout

weather<-read.csv("input/weldhill.csv") #from https://labs.arboretum.harvard.edu/weather/

weather<-filter(weather,grepl("2018|2019|2020",weather$Eastern.daylight.time)) ## pull the years we need

 goo<-weather %>% separate(Eastern.daylight.time, into = c('date', 'time'), sep = -8, convert = TRUE) ### make date and time seperate cols

 
goo<-goo%>% group_by(date) %>% summarise(maxTf=max(Temp..F ),minTf=min(Temp..F )) ###make daily mins and maxT

more20<-read.csv("input/Daily Data for Boston (Weld Hill), MA - [id=ma_weld nwon, lat=42.2953, lon=-71.1337].csv")


goo2<-goo
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
more20<-dplyr::select(more20,date,Max.Air.Temp...F.,Min.Air.Temp...F.,doy)
colnames(more20)<-colnames(goo2)
goo2<-rbind(goo2,more20)

goo2$maxTc<-(goo2$maxTf-32)*(5/9) ##convert to  C
goo2$minTc<-(goo2$minTf-32)*(5/9)


goo2$year<-substr(goo2$date, 1, 4)
goo18<-filter(goo2,year=="2018")
goo19<-filter(goo2,year=="2019")
goo20<-filter(goo2,year=="2020")



###Calculate gdd
goo18$GDD_10<- pollen::gdd(tmax = goo18$maxTc,tmin = goo18$minTc,tbase = 10,type = "B")
goo19$GDD_10<- pollen::gdd(tmax = goo19$maxTc,tmin = goo19$minTc,tbase = 10,type = "B")
goo20$GDD_10<- pollen::gdd(tmax = goo20$maxTc,tmin = goo20$minTc,tbase = 10,type = "B")



goober<-rbind(goo18,goo19,goo20)


cg1<-filter(cg,!is.na(pgs))

cg2<-filter(cg,!is.na(fgs))

cg2 %>% group_by(spp) %>% count()
toosmall<-c("ACEPEN","ACESPI","QUEALB","QUERUB","VACMYR")
cg1<-filter(cg1,!spp %in% toosmall)
cg2<-filter(cg2,!spp %in% toosmall)

cg1$leafout<-round(cg1$leafout)
cg2$leafout<-round(cg2$leafout) ### some were X.5 which messes up everythi n

ref<-data.frame(year=cg1$year,start=cg1$leafout,end=cg1$budset)
myvec<-c()



for(j in 1:length(cg1$pgs)){
temp<-filter(goober,year==cg1$year[j])
tempS<-filter(temp,doy==cg1$leafout[j])
tempE<-filter(temp,doy==cg1$budset[j])
             GDD<-tempE$GDD_10-tempS$GDD_10
   print(GDD)
              myvec<-c(myvec,GDD)
}



cg1$pgsGDD<-myvec

ggplot(cg1,aes(pgs,pgsGDD))+geom_point()

ggplot(cg1,aes(leafout,pgsGDD))+geom_point()+geom_smooth(method="lm")

ggplot(cg1,aes(pgsGDD,leafout))+geom_point(aes(color=spp))+geom_smooth(method="lm",aes(color=spp))
ggplot(cg1,aes(pgs,leafout))+geom_point(aes(color=spp))+geom_smooth(method="lm",aes(color=spp))



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

jpeg("figures/gs_vs_leafout.jpeg",width=10,height=8,units = 'in',res=300)
ggpubr::ggarrange(ggplot(cg1,aes(leafout,pgsGDD))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),
ggplot(cg1,aes(leafout,pgs))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),
ggplot(cg2,aes(leafout,fgsGDD))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),
ggplot(cg2,aes(leafout,fgs))+geom_point(aes(color=spp),size=.4)+geom_smooth(method="lm",aes(color=spp),se=FALSE)+geom_smooth(method="lm",se=FALSE,size=2),common.legend = TRUE)
dev.off()


jpeg("figures/gs_vs_leafout_site.jpeg",width=10,height=8,units = 'in',res=300)
ggpubr::ggarrange(ggplot(cg1,aes(leafout,pgsGDD))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),
                  ggplot(cg1,aes(leafout,pgs))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),
                  ggplot(cg2,aes(leafout,fgsGDD))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),
                  ggplot(cg2,aes(leafout,fgs))+geom_point(aes(color=site),size=.4)+geom_smooth(method="lm",aes(color=site),se=FALSE),common.legend = TRUE)
dev.off()


cg1$pgsGDD_cent<-cg1$pgsGDD-mean(cg1$pgsGDD)
cg1$leafout_cent<-cg1$leafout-mean(cg1$leafout)

cg_wgdd<-left_join(cg2,cg1)


ggplot(cg_wgdd,aes(leafcolor,budset))+geom_point()+geom_abline()

pgsGDD.mod<-brm(pgsGDD~leafout_cent+(leafout_cent|spp),data=cg1,control = list(adapt_delta=.95),warmup=3000,iter=4000)

pgs.mod<-brm(pgs~leafout_cent+(leafout_cent|spp),data=cg1,control = list(adapt_delta=.95),warmup=3000,iter=4000)



cg2$fgsGDD_cent<-cg2$fgsGDD-mean(cg2$fgsGDD)
cg2$leafout_cent<-cg2$leafout-mean(cg2$leafout)

fgsGDD.mod<-brm(fgsGDD~leafout_cent+(leafout_cent|spp),data=cg2,control = list(adapt_delta=.95),warmup=3000,iter=4000)

fgs.mod<-brm(fgs~leafout_cent+(leafout_cent|spp),data=cg2,control = list(adapt_delta=.95),warmup=3000,iter=4000)


fixef(pgsGDD.mod)
fixef(pgs.mod)
fixef(fgsGDD.mod)
fixef(fgs.mod)

save.image("cgseasonmods.Rda")
