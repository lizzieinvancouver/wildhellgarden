### Started 28 March 2016 ###
### BY Lizzie ###

## What to plant in the common garden? ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/projects/treegarden/wildhellgarden/analyses")

plantneeds <- read.csv("input/Commongardenspeciesrequirements_Mar2016.csv")

# so what *can* we plant
unique(plantneeds$Availability)
plantwho <- subset(plantneeds, Availability!="none germinated" & Availability!="none collected")

shhf <- subset(plantwho, Collected.at.HF.=="Y" & Collected.at.SH.=="Y") # ack, that gives us 10 species total
shgr <- subset(plantwho, Collected.at.GR.=="Y" & Collected.at.SH.=="Y") # well, that gives us 18 species total
shwm <- subset(plantwho, Collected.at.WM.=="Y" & Collected.at.SH.=="Y") # well, that gives us 18 species total
grhf <- subset(plantwho, Collected.at.HF.=="Y" & Collected.at.GR.=="Y") # ack, that gives us 8 species total (all same as shhf)
wmhf <- subset(plantwho, Collected.at.HF.=="Y" & Collected.at.WM.=="Y") # hmm, only 14

# some indexing
shgr$Species[which(!shgr$Species %in% shhf$Species)] # in shgr, but not shhf
shgr$Species[which(!shgr$Species %in% shwm$Species)] # in shgr, but not shwm

grhf$Species[which(!grhf$Species %in% shhf$Species)] # just checking that grhf=shhf

planttheseperhaps <- c(shgr$Species, shhf$Species, shwm$Species)
sort(unique(planttheseperhaps)) # 20 species, includes Sorbus

planttheseperhaps1 <- c(shhf$Species, shwm$Species)
sort(unique(planttheseperhaps1)) # 19 species

# spp we want to still try to get
sppadd <- c("Acer pensylvanicum", "Acer rubrum", "Acer saccharum" ,
   "betula populifolia", "Fagus grandifolia", "Populus grandidentata")

# current list as of 28 March 2016
plantthese <- c(sppadd, planttheseperhaps)

# who do we lose?
plantneeds$Species[which(!plantneeds$Species %in% planttheseperhaps)]

# how much space for the 26 species?
try26 <- plantneeds[which(plantneeds$Species %in% plantthese),]

# ideally I would like 8 per site or 32 plants per species

try26$Est....of.plants.per.6m.x.6m.plot
try26$unitsof8 <- try26$Est....of.plants.per.6m.x.6m.plot/8
try26$neededunitsfor32 <- 4/try26$unitsof8
sum(try26$neededunitsfor32) # so we need 59 units of 6 m x 6 m plots, which equals 2124 m2 and we have about 3K 
