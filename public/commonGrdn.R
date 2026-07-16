# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
options(mc.cores = parallel::detectCores())
graphics.off()

library(brms)
library(tidyr)
library(tidybayes)
library(gridExtra)
library(viridis)
library(ggpubr)

setwd("") # set working directory to folder

cg1<-read.csv("weldhill_budset.csv") ##read in budset based data
  cg1$leafout_cent<-cg1$leafout-mean(cg1$leafout) ## center predictors

  cg1$species<-NA
  cg1$species<-ifelse(cg1$spp=="SAMRAC","S. racemosa",cg1$species)
  cg1$species<-ifelse(cg1$spp=="VIBCAS","V. cassinoides",cg1$species)
  cg1$species<-ifelse(cg1$spp=="SPIALB","S. alba",cg1$species)
  cg1$species<-ifelse(cg1$spp=="AMECAN","A. canadensis",cg1$species)
  cg1$species<-ifelse(cg1$spp=="DIELON","D. lonicera",cg1$species)
  cg1$species<-ifelse(cg1$spp=="AROMEL","A. melanocarpa",cg1$species)
  cg1$species<-ifelse(cg1$spp=="SPITOM","S. tomentosa",cg1$species)
  cg1$species<-ifelse(cg1$spp=="BETPOP","B. populifolia",cg1$species)
  cg1$species<-ifelse(cg1$spp=="MYRGAL","M. gale",cg1$species)
  cg1$species<-ifelse(cg1$spp=="BETPAP","B. papyrifera",cg1$species)
  cg1$species<-ifelse(cg1$spp=="BETALL","B. alleghaniensis",cg1$species)
  cg1$species<-ifelse(cg1$spp=="ALNINC","A. incana",cg1$species)
  cg1$species<-ifelse(cg1$spp=="SORAME","S. americana",cg1$species)
  cg1$species<-ifelse(cg1$spp=="ACEPEN","A. pensylvanicum",cg1$species)
  cg1$species<-ifelse(cg1$spp=="ACESPI","A. spicatum",cg1$species)
  cg1$species<-ifelse(cg1$spp=="QUERUB","Q. rubra",cg1$species)
  cg1$species<-ifelse(cg1$spp=="QUEALB","Q. alba",cg1$species)
  cg1$species<-ifelse(cg1$spp=="VACMYR","V. myritilloides",cg1$species)

  ###MODELS###

pgsGDD.mod<-brm(pgsGDD~leafout_cent+(leafout_cent|species),
                data=cg1,control = list(adapt_delta=.95),
                warmup=3000,iter=4000)

pgs.mod<-brm(pgs~leafout_cent+(leafout_cent|species),
             data=cg1,control = list(adapt_delta=.95),
             warmup=3000,iter=4000)


new.data<-data.frame(leafout_cent=rep(c(-20,0,20),each=18),
                     species=rep(unique(cg1$species),3)) ## generate a new data frame

###predictions by species
PGDD<-epred_draws(pgsGDD.mod,newdata = new.data,ndraws = 1000)
Pp<-epred_draws(pgs.mod,newdata = new.data,ndraws = 1000)

##predictions of main effects only
PGDD2<-epred_draws(pgsGDD.mod,newdata = new.data,ndraws = 1000,re_formula = NA)
P2<-epred_draws(pgs.mod,newdata = new.data,ndraws = 1000,re_formula = NA)

###subset to species with adaquate sample sizes
species_order<-c("S. racemosa","V. cassinoides","S. alba","A. canadensis",
                 "D. lonicera","A. melanocarpa","S. tomentosa",
                 "B. populifolia","M. gale","B. papyrifera","B. alleghaniensis",
                 "A. incana","S. americana")

Pp2<-filter(Pp,species %in% species_order)
PGGD2<-filter(PGDD,species %in% species_order)
cg11<-filter(cg1, species %in% species_order) ##remove NA's

###results and prepare for plotting
PGDD2_summ <- PGDD2 %>%
  group_by(species, leafout_cent) %>%
  mean_qi(.epred, .width = 0.90)

P2_summ <- P2 %>%
  group_by(species, leafout_cent) %>%
  mean_qi(.epred, .width = 0.90)

PGGD2_summ <- PGGD2 %>%
  group_by(species, leafout_cent) %>%
  mean_qi(.epred, .width = 0.90)

Pp2_summ <- Pp2 %>%
  group_by(species, leafout_cent) %>%
  mean_qi(.epred, .width = 0.90)


r1 <- range(cg11$pgs, P2_summ$.lower, P2_summ$.upper)
r2 <- range(cg11$pgsGDD, PGDD2_summ$.lower, PGDD2_summ$.upper)

# scaling: pgsGDD_scaled = (pgsGDD - min2) * (diff1 / diff2) + min1
scale <- diff(r1) / diff(r2)
shift <- r1[1] - r2[1] * scale

cg11$pgsGDD_scaled <- cg11$pgsGDD * scale + shift
PGDD2_summ$lower_scaled <- PGDD2_summ$.lower * scale + shift
PGDD2_summ$upper_scaled <- PGDD2_summ$.upper * scale + shift
PGDD2_summ$epred_scaled <- PGDD2_summ$.epred * scale + shift

#################FIGURE 1C##################
ggplot() +
  ## --- Left axis (pgs) ---
  geom_jitter(data = cg11,width = 1,height=1,
              aes(x = leafout_cent, y = pgs,
                  shape = as.factor(year)),
              size = 1) +
  geom_ribbon(data = P2_summ,
              aes(x = leafout_cent, ymin = .lower, ymax = .upper),
              alpha = 0.7,fill="navy") +
  geom_line(data = P2_summ,
            aes(x = leafout_cent, y = .epred), size = 1) +
  
  ## --- Right axis (pgsGDD) ---
  geom_jitter(data = cg11,width = 1,height=1,
              aes(x = leafout_cent, y = pgsGDD_scaled,
                  shape = as.factor(year)),
              size = 1, alpha = 0.7) +
  geom_ribbon(data = PGDD2_summ,
              aes(x = leafout_cent,
                  ymin = lower_scaled,
                  ymax = upper_scaled),
              alpha = 0.7,fill="hotpink3") +
  geom_line(data = PGDD2_summ,
            aes(x = leafout_cent, y = epred_scaled),
            size = 1, linetype = 2) +
  
  ## axes
  scale_y_continuous(
    name = "Calendar growing season",
    sec.axis = sec_axis(~ (. - shift) / scale,
                        name = "Thermal growing season")
  ) +
  coord_cartesian(xlim = c(-20, 20)) +
  scale_color_viridis_d(option = "F") +
  scale_shape_manual(name = "year", values = c(0, 1, 2)) +
  ggthemes::theme_few() +
  xlab("")+theme(legend.position = "top")
#################END FIGURE 1C##################

###prep and plot figure 1d)

r_left  <- range(Pp2_summ$.lower,  Pp2_summ$.upper,  na.rm = TRUE)
r_right <- range(PGGD2_summ$.lower, PGGD2_summ$.upper, na.rm = TRUE)

scale_factor <- diff(r_left) / diff(r_right)
shift_factor <- r_left[1] - r_right[1] * scale_factor

PGGD2_summ$lower_scaled <- PGGD2_summ$.lower * scale_factor + shift_factor
PGGD2_summ$upper_scaled <- PGGD2_summ$.upper * scale_factor + shift_factor
PGGD2_summ$epred_scaled <- PGGD2_summ$.epred * scale_factor + shift_factor



###Figure 1d#################
Pp2_summ <- Pp2_summ %>%
  mutate(species = factor(species, levels = species_order))

PGGD2 <- PGGD2 %>%
  mutate(species = factor(species, levels = species_order))

ggplot() +
  
  ## --- LEFT AXIS (Pp2_summ; now the primary axis) ---
  geom_ribbon(data = Pp2_summ,
              aes(x = leafout_cent,
                  ymin = .lower, ymax = .upper),fill="navy",
              alpha = .7) +
  
geom_line(data = Pp2_summ,
           aes(x = leafout_cent, y = .epred),
            size = 0.8) +   # solid line for left axis
  
  
  ## --- RIGHT AXIS (PGGD2_summ; scaled to left-space) ---
  geom_ribbon(data = PGGD2_summ,
              aes(x = leafout_cent,
                  ymin = lower_scaled, ymax = upper_scaled), fill="hotpink3",
              alpha = 0.7)+
  
  geom_line(data = PGGD2_summ,
            aes(x = leafout_cent, y = epred_scaled),
            size = 0.8,
            linetype = 2) +  # dashed for right axis
  
  
  ## Facets
  facet_wrap(~factor(species, levels = species_order),nrow=3)+
  #facet_wrap(~species, nrow = 3)+
  
  ## Axes (flipped!)
  scale_y_continuous(
    name = "Calendar growing season",
    sec.axis = sec_axis(~ (. - shift_factor) / scale_factor,
                        name = "Thermal growing season")
  ) +
  
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  
  ggthemes::theme_few(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "italic")
  ) +
  xlab("leafout anomaly (days)")
######END FIGURE 1D#####################################

### part II varience partitition 

cor.test(cg1$leafout,cg1$budset)
round(cor.test(cg1$leafout,cg1$budset)[4]$estimate,2)
round(cor.test(cg1$leafout,cg1$budset)[9]$conf.int[1],2)
round(cor.test(cg1$leafout,cg1$budset)[9]$conf.int[2],2)

###run models
mod.lo<-brm(leafout~(1|species)+(1|site)+(1|year),data=cg1,
            warmup=4000,iter=5000, control=list(adapt_delta=.99))

mod.bs<-brm(budset~(1|species)+(1|site)+(1|year),data=use.data,
            warmup=4000,iter=5000, control=list(adapt_delta=.99)) 

### estract posteriors
frostfree<-c("Harvard Forest, MA, USA","Saint Hippolyte, QC, CA","White Mountains, NH, USA","Second College Grant, NH, USA")

lo.siteout<-mod.lo %>% spread_draws(r_site[site,Intercept])
lo.siteout$population<-NA
lo.siteout$population<-ifelse(lo.siteout$site=="WM","White Mountains, NH, USA",lo.siteout$population)
lo.siteout$population<-ifelse(lo.siteout$site=="HF","Harvard Forest, MA, USA",lo.siteout$population)
lo.siteout$population<-ifelse(lo.siteout$site=="GR","Second College Grant, NH, USA",lo.siteout$population)
lo.siteout$population<-ifelse(lo.siteout$site=="SH","Saint Hippolyte, QC, CA",lo.siteout$population)

bs.siteout<-mod.bs %>% spread_draws(r_site[site,Intercept])

bs.siteout$population<-NA
bs.siteout$population<-ifelse(bs.siteout$site=="WM","White Mountains, NH, USA",bs.siteout$population)
bs.siteout$population<-ifelse(bs.siteout$site=="HF","Harvard Forest, MA, USA",bs.siteout$population)
bs.siteout$population<-ifelse(bs.siteout$site=="GR","Second College Grant, NH, USA",bs.siteout$population)
bs.siteout$population<-ifelse(bs.siteout$site=="SH","Saint Hippolyte, QC, CA",bs.siteout$population)

lo.yearout<-mod.lo %>% spread_draws(r_year[year,Intercept])
bs.yearout<-mod.bs %>% spread_draws(r_year[year,Intercept])

lo.yearout$phase<-"leafout"
bs.yearout$phase<-"budset"

lo.siteout$phase<-"leafout"
bs.siteout$phase<-"budset"


siteout<-rbind(bs.siteout,lo.siteout)
yearout<-rbind(bs.yearout,lo.yearout)

yorder<-c("2018","2019","2020")

##FIGURE 2
pd<-ggstance::position_dodgev(height=.3)
ggplot(siteout,aes(r_site,population))+
  stat_pointinterval(.width = c(.5,.9),aes(fill=phase,color=phase),position=pd)+geom_vline(xintercept=0)+
  xlab("effect of site \n(days)")+ylab("")+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+scale_fill_manual(values=c("tan4","green4"))+scale_color_manual(values=c("tan3","darkgreen"))+
  coord_cartesian(xlim=c(-4,4))

ggplot(yearout,aes(r_year,as.factor(year)))+
  stat_pointinterval(.width = c(.5,.9),aes(fill=phase,color=phase),position=pd)+geom_vline(xintercept=0)+
  xlab("effect of year \n(days)")+
  ylab("")+xlim(-40,40)+
  ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+
  scale_fill_manual(values=c("tan4","green4"))+scale_color_manual(values=c("tan3","darkgreen"))
###END FIGURE 2


if(FALSE){
 ##supplimentary analysis, same as above but with using leaf coloration instead of budset as
  #end of season phase marker
cg2<-read.csv("weldhill_leafcolor.csv") ##read in coloration based data
    cg2$leafout_cent<-cg2$leafout-mean(cg2$leafout)  
    
    cg2$species<-NA
    cg2$species<-ifelse(cg2$spp=="SAMRAC","S. racemosa",cg2$species)
    cg2$species<-ifelse(cg2$spp=="VIBCAS","V. cassinoides",cg2$species)
    cg2$species<-ifelse(cg2$spp=="SPIALB","S. alba",cg2$species)
    cg2$species<-ifelse(cg2$spp=="AMECAN","A. canadensis",cg2$species)
    cg2$species<-ifelse(cg2$spp=="DIELON","D. lonicera",cg2$species)
    cg2$species<-ifelse(cg2$spp=="AROMEL","A. melanocarpa",cg2$species)
    cg2$species<-ifelse(cg2$spp=="SPITOM","S. tomentosa",cg2$species)
    cg2$species<-ifelse(cg2$spp=="BETPOP","B. populifolia",cg2$species)
    cg2$species<-ifelse(cg2$spp=="MYRGAL","M. gale",cg2$species)
    cg2$species<-ifelse(cg2$spp=="BETPAP","B. papyrifera",cg2$species)
    cg2$species<-ifelse(cg2$spp=="BETALL","B. alleghaniensis",cg2$species)
    cg2$species<-ifelse(cg2$spp=="ALNINC","A. incana",cg2$species)
    cg2$species<-ifelse(cg2$spp=="SORAME","S. americana",cg2$species)
    cg2$species<-ifelse(cg2$spp=="ACEPEN","A. pensylvanicum",cg2$species)
    cg2$species<-ifelse(cg2$spp=="ACESPI","A. spicatum",cg2$species)
    cg2$species<-ifelse(cg2$spp=="QUERUB","Q. rubra",cg2$species)
    cg2$species<-ifelse(cg2$spp=="QUEALB","Q. alba",cg2$species)
    cg2$species<-ifelse(cg2$spp=="VACMYR","V. myritilloides",cg2$species)    
    
    
fgsGDD.mod<-brm(fgsGDD~leafout_cent+(leafout_cent|species),
                    data=cg2,control = list(adapt_delta=.95),
                    warmup=3000,iter=4000)
    
    fgs.mod<-brm(fgs~leafout_cent+(leafout_cent|species),
                 data=cg2,control = list(adapt_delta=.95),
                 warmup=3000,iter=4000)
    
  ##same as above but using leaf coloration models
  FGDD<-epred_draws(fgsGDD.mod,newdata = new.data,ndraws = 1000)
  Fp<-epred_draws(fgs.mod,newdata = new.data,ndraws = 1000)
  
  FGDD2<-epred_draws(fgsGDD.mod,newdata = new.data,ndraws = 1000,re_formula = NA)
  Fp2<-epred_draws(fgs.mod,newdata = new.data,ndraws = 1000,re_formula = NA)


  FGDD2_summ <- FGDD2 %>%
    group_by(species, leafout_cent) %>%
    mean_qi(.epred, .width = 0.90)
  
  F2_summ <- F2 %>%
    group_by(species, leafout_cent) %>%
    mean_qi(.epred, .width = 0.90)
  
  FGGD2_summ <- FGDD2 %>%
    group_by(species, leafout_cent) %>%
    mean_qi(.epred, .width = 0.90)
  
  Fp2_summ <- Fp2 %>%
    group_by(species, leafout_cent) %>%
    mean_qi(.epred, .width = 0.90)
    
  ggplot() +
    geom_point(data = cg2,
               aes(x = leafout_cent, y = fgs), size = 0.1) +
    geom_ribbon(data = F2_summ,
                aes(x = leafout_cent,
                    ymin = .lower, ymax = .upper),
                alpha = 0.5) +
    geom_line(data = F2_summ,
              aes(x = leafout_cent, y = .epred),
              size = 1) +
    ylab("Calendar growing season") +
    coord_cartesian(xlim = c(-20, 20)) +
    ggthemes::theme_few() +
    xlab("")
  
  ggplot() +
    geom_point(data = cg2,
               aes(x = leafout_cent, y = fgsGDD),
               size = 0.1) +
    geom_ribbon(data = FGDD2_summ,
                aes(x = leafout_cent,
                    ymin = .lower, ymax = .upper),
                alpha = 0.5) +
    geom_line(data = FGDD2_summ,
              aes(x = leafout_cent, y = .epred),
              size = 1) +
    ylab("Thermal growing season") +
    coord_cartesian(xlim = c(-20, 20)) +
    ggthemes::theme_few() +
    xlab("leafout anomaly \n(days)")
  
  
ggplot(FGGD2_summ,
             aes(leafout_cent, .epred)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
    geom_line(size = 0.8)+
  facet_wrap(~factor(species, levels =species_order)) + 
  ylab("") + xlab("leafout anomaly \n(days)") +
    ggthemes::theme_few(base_size = 11)# +
  theme(strip.text = element_text(face = "italic"))
  
  
  ggplot(Fp2_summ,
              aes(leafout_cent, .epred)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
    geom_line(size = 0.8) +
    facet_wrap(~factor(species, levels =species_order)) +
    ylab("") + xlab("") +
    ggthemes::theme_few(base_size = 11) +
    theme(strip.text = element_text(face = "italic"))
  
  
  ###Varience partitioning models
  fmod.lo<-brm(leafout~(1|species)+(1|site)+(1|year),data=cg2,
               warmup=4000,iter=5000, control=list(adapt_delta=.995))
  
  fmod.lc<-brm(leafcolor~(1|species)+(1|site)+(1|year),data=cg2,
               warmup=4000,iter=5000, control=list(adapt_delta=.99))
  
  flo.siteout<-fmod.lo %>% spread_draws(r_site[site,Intercept])
  flo.siteout$population<-NA
  flo.siteout$population<-ifelse(flo.siteout$site=="WM","White Mountains, NH, USA",flo.siteout$population)
  flo.siteout$population<-ifelse(flo.siteout$site=="HF","Harvard Forest, MA, USA",flo.siteout$population)
  flo.siteout$population<-ifelse(flo.siteout$site=="GR","Second College Grant, NH, USA",flo.siteout$population)
  flo.siteout$population<-ifelse(flo.siteout$site=="SH","Saint Hippolyte, QC, CA",flo.siteout$population)
  
  
  lc.siteout<-fmod.lc %>% spread_draws(r_site[site,Intercept])
  
  lc.siteout$population<-NA
  lc.siteout$population<-ifelse(lc.siteout$site=="WM","White Mountains, NH, USA",lc.siteout$population)
  lc.siteout$population<-ifelse(lc.siteout$site=="HF","Harvard Forest, MA, USA",lc.siteout$population)
  lc.siteout$population<-ifelse(lc.siteout$site=="GR","Second College Grant, NH, USA",lc.siteout$population)
  lc.siteout$population<-ifelse(lc.siteout$site=="SH","Saint Hippolyte, QC, CA",lc.siteout$population)
  
  flo.siteout$phase="leafout"
  lc.siteout$phase="leaf coloration"
  siteout2<-rbind(flo.siteout,lc.siteout)
  
  flo.yearout<-fmod.lo %>% spread_draws(r_year[year,Intercept])
  lc.yearout<-fmod.lc %>% spread_draws(r_year[year,Intercept])
  lc.yearout$phase<-"leaf coloration"
  flo.yearout$phase<-"leafout"
  yearout2<-rbind(flo.yearout,lc.yearout)
  
ggplot(yearout2,aes(r_year,as.factor(year)))+
    stat_pointinterval(.width = c(.5,.9),aes(fill=phase,color=phase),position=pd)+geom_vline(xintercept=0)+
    xlab("effect of year \n(days)")+
    ylab("")+xlim(-40,40)+
    ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(yorder))+
    scale_fill_manual(values=c("tan4","green4"))+scale_color_manual(values=c("tan2","darkgreen"))
  
  
ggplot(siteout2,aes(r_site,population))+
    stat_pointinterval(.width = c(.5,.9),aes(fill=phase,color=phase),position=pd)+geom_vline(xintercept=0)+
    xlab("effect of site \n(days)")+ylab("")+
    ggthemes::theme_few()+scale_y_discrete(name="",limits=rev(frostfree))+scale_fill_manual(values=c("tan4","green4"))+scale_color_manual(values=c("tan3","darkgreen"))+
    coord_cartesian(xlim=c(-8,6))
  
}
if(FALSE){ #Script generates Figure 3. No actual analyses just computation ofGDDs for 4 indiviudals
  #over their growing season
clim19<-read.csv("2019_clim.csv")

#### below makes the concept figure 

delta<-diff(clim19$GDD_10)
delta<-c(0,delta)
clim19$ggdperday<-delta

early<-filter(clim19,doy %in% c(115:240)) #these are based on visual assessments of indivudual's
#growing seasons
early$GDD_10<-early$GDD_10-early$GDD_10[1]
240-115
142-115 #27 day diff
253-240 #13
27-13
# day diff
late<-filter(clim19,doy %in% c(142:253))  #these are based on visual assessments of indivudual's
#growing seasons
late$GDD_10<-late$GDD_10-late$GDD_10[1]

all<-filter(clim19,doy %in% c(115:253))

early2<-filter(clim19,doy %in% c(127:240)) #these are based on visual assessments of indivudual's
#growing seasons
early2$GDD_10<-early2$GDD_10-early2$GDD_10[1]

late2<-filter(clim19,doy %in% c(147:245)) #these are based on visual assessments of indivudual's
#growing seasons
late2$GDD_10<-late2$GDD_10-late2$GDD_10[1]

ggplot(data=all,aes(x=doy,y=ggdperday))+
  geom_rect(xmin=142,xmax=253, ymin=-2, ymax=0, alpha=0.02,fill="green3")+
  geom_rect(xmin=115,xmax=240, ymin=-3, ymax=-1, alpha=0.02,fill="darkgreen")+
  geom_rect(xmin=147,xmax=245, ymin=-6, ymax=-4, alpha=0.02,fill="skyblue1")+
  geom_rect(xmin=127,xmax=240, ymin=-7, ymax=-5, alpha=0.02,fill="skyblue3")+
  geom_line()+ggthemes::theme_few()+ylab("Daily thermal sums")+ylim(-8,20)+xlab("Day of year")

ggplot()+geom_line(data=early,aes(x=doy,y=GDD_10),color="darkgreen", linewidth=2)+
  geom_line(data=late,aes(x=doy,y=GDD_10),color="green3",size=2)+
  ggthemes::theme_few()+ylab("Thermal growing season")+xlab("day of year")+coord_cartesian(xlim=c(100,250),ylim=c(0,1250))


ggplot()+geom_line(data=early2,aes(x=doy,y=GDD_10),color="skyblue3", linewidth=2)+
  geom_line(data=late2,aes(x=doy,y=GDD_10),color="skyblue1",size=2)+
  ggthemes::theme_few()+ylab("")+xlab("day of year")+coord_cartesian(xlim=c(100,250),ylim=c(0,1250))


}