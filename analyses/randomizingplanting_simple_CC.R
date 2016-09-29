## Script made by Dan F. for Common Garden Project ##
## Updated 5 Sept 2016 by Cat with new species list ##

### Super simple version. Randomize. Figure out how many individuals of each size to go into each plot, assign to plots. Then manually locate in Excel.

library(dplyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set own wd as needed
if(length(grep("Cat Chamberlain", getwd()))==0){ setwd("~/Documents/git/wildhellgarden/analyses") }

setwd("~/Documents/git/wildhellgarden/analyses")

plantneeds <- read.csv("input/Commongardenspeciesrequirements_5Sept2016.csv")

plantneeds <- as_data_frame(plantneeds) # put in dplyr format

# make 6 letter code
plantneeds$sp = paste(unlist(lapply(strsplit(plantneeds$Species, " "), function(x) toupper(substr(x[1], 1,3)))), unlist(lapply(strsplit(plantneeds$Species, " "), function(x) toupper(substr(x[2], 1,3)))), sep ="")

d <- filter(plantneeds, Keep != "N") # 20 core species, 24 total 

# Jehane measured otut 40 8x8 plots 21-22 April. We have three groups of plants in terms of shade, and four kinds of spacing, 0.75, 1, 1.5, and 3 m per individual.
# Divide up in to full-life shade, partial-life shade, and full sun species. Within each shade set, randomize position of individuals of species according to their spacing. Put all three sets together, and sum up the total indivdiduals per species, per site.

reps = 9 # individuals per species per sie
sites = 4 # total number of sites
sitez = c("HF","SH","GR","WM") # 4 sites

# start with the full shade (fs). 5 species. Should end up with 5 plots. Same with full set of 26 sp

fs <- filter(d, Team.Height == "Shrub Shade")
# First guess: how many plots will we need of this set?
# NA for species which can fit entirely inside spacing of. 
(fsplotno <- ceiling(sum(reps*sites / fs$Team.plants.per.6m, na.rm=T)))

# Partial shade (ps) plots - 7 plots. Same again for full set.
ps <- filter(d, Team.Height == "Shrub Sun")
(psplotno <- ceiling(sum(reps*sites / ps$Team.plants.per.6m, na.rm=T)))

# And last, for trees (ts): 14 plots. Jumps to 36 wih full set of 26 sp, addition of acepen, acerub, acesac, betpop, faggra, popgra. 
# could at a minimum include 3 of these (14 plots free in the core set), and betpop has small requriement, so potentially as many as 5 of these.
ts <- filter(d, Team.Height == "Tree")
(tsplotno <- ceiling(sum(reps*sites / ts$Team.plants.per.6m, na.rm=T)))

# make table of all individuals to draw from. Col of all sp, then sites within sp, then reps within site within sp
lengthout = nrow(d)*reps*sites # 720 with core, 1728 exteneded

dat <- data.frame(
  sp = gl(nrow(d), reps*sites, length = lengthout, labels = d$sp),
  site = gl(sites, reps, length = lengthout, labels = sitez),
  rep = gl(reps, 1, length = lengthout)
)
dat$ind <- with(dat, paste(sp, site, rep, sep="_"))
dat$set <- d$Team.Height[match(dat$sp, d$sp)]
dat$space <- d$Team.spacing[match(dat$sp, d$sp)]

##### Shrubs Sun (AKA partial shade shrubs)
ps <- subset(dat, set == "Shrub Sun") 

# now draw from this data frame
ps <- ps[sample(rownames(ps)),]
ps$order = 1:nrow(ps)
# 49 individuals per plot
psplots = c(4, 8, 11, 14, 20, 35, 36)

psplotno <- vector()
psplotarea <- vector()

for(i in 1:nrow(ps)){
  
  psplotarea <- c(psplotarea, ps[i,'space'])
  
  if(sum(psplotarea) >= 49) { psplots = psplots[-1]; psplotarea = vector() }
  
  psplotno <- c(psplotno, psplots[1])
}

ps$plotno = psplotno


ts <- subset(dat, set == "Tree") 
ts <- ts[sample(rownames(ts)),]
ts$order = 1:nrow(ts)

tsplots = c(1, 2, 3, 5, 6, 9, 12, 15, 16, 17, 18, 19,
            21, 22, 24, 25, 27, 28, 29,
            30, 31, 32, 34, 37, 38, 39, 40)

tsplotno <- vector()
tsplotarea <- vector()

for(i in 1:nrow(ts)){
  
  tsplotarea <- c(tsplotarea, ts[i,'space'])
  
  if(sum(tsplotarea) >= 42) { tsplots = tsplots[-1]; tsplotarea = vector() }
  
  tsplotno <- c(tsplotno, tsplots[1])
}

ts$plotno = tsplotno

fs <- subset(dat, set == "Shrub Shade") 
fs <- fs[sample(rownames(fs)),]
fs$order = 1:nrow(fs)

fsplots = c(7, 10, 13, 23, 33)

fsplotno <- vector()
fsplotarea <- vector()

for(i in 1:nrow(fs)){
  
  fsplotarea <- c(fsplotarea, fs[i,'space'])
  
  if(sum(fsplotarea) >= 49) { fsplots = fsplots[-1]; fsplotarea = vector() }
  
  fsplotno <- c(fsplotno, fsplots[1])
}

fs$plotno = fsplotno


dat2 <- rbind(ps, ts, fs)

write.csv(dat2, file = "Inds for Planting CC.csv", row.names=F)

