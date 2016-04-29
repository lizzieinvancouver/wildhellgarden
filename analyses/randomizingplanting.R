### Based off of Lizzie's planning thoughts
### Dan

## What to plant in the common garden? ##

## housekeeping
library(dplyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/projects/treegarden/wildhellgarden/analyses")

# Set own wd as needed
if(length(grep("danflynn", getwd()))>0){ setwd("~/Documents/git/wildhellgarden/analyses") }

plantneeds <- read.csv("input/Commongardenspeciesrequirements_27Apr2016.csv")

plantneeds <- as_data_frame(plantneeds)

plantneeds$sp = paste(unlist(lapply(strsplit(plantneeds$Species, " "), function(x) toupper(substr(x[1], 1,3)))), unlist(lapply(strsplit(plantneeds$Species, " "), function(x) toupper(substr(x[2], 1,3)))), sep ="")

d <- filter(plantneeds, Keep == "Y") # 20 core species, work on this first
# d <- filter(plantneeds, Keep != "") # 26 species, full set


# plantneeds %>% 
#   filter(Keep, Keep != "")

# Jehane measured otut 40 8x8 plots 21-22 April. We have three groups of plants in terms of shade, and four kinds of spacing, 0.75, 1, 1.5, and 3 m per individual.

# Divide up in to full-life shade, partial-life shade, and full sun species. Within each shade set, randomize position of individuals of species according to their spacing. Put all three sets together, and sum up the total indivdiduals per species, per site.

reps = 9 # individuals per species per sie
sites = 4 # total number of sites
sitez = c("HF","SH","GR","WM")

# start with the full shade. 5 species. Should end up with 5 plots. Same with full set of 26 sp

fs <- filter(d, Team.Height == "Shrub Shade")
fs$sp
fs$Team.spacing

# First guess: how many plots will we need of this set?
# NA for species which can fit entirely inside spacing of. 
(fsplotno <- ceiling(sum(reps*sites / fs$Team.plants.per.6m, na.rm=T)))

# partial shade plots - 7 plots. Same again for full set.
ps <- filter(d, Team.Height == "Shrub Sun")
(psplotno <- ceiling(sum(reps*sites / ps$Team.plants.per.6m, na.rm=T)))

# And last, for trees: 14 plots. Jumps to 36 wih full set of 26 sp, addition of acepen, acerub, acesac, betpop, faggra, popgra. 
# could at a minimum include 3 of these (14 plots free in the core set), and betpop has small requriement, so potentially as many as 5 of these.
ts <- filter(d, Team.Height == "Tree")
(tsplotno <- ceiling(sum(reps*sites / ts$Team.plants.per.6m, na.rm=T)))

# working with full shade set. Make matrix for blank plot, 8 x 8 m. need to make this a grid of .25 m squares, to accomodate the smaller spacings
plt <- matrix(nrow = 8 / 0.25, ncol= 8 / 0.25)
# fill in buffer of 1 m, i.e. first four rows, first four cols...
buff = 1
plt[1:(buff/0.25),] = 0
plt[,1:(buff/0.25)] = 0
plt[(nrow(plt)-buff/0.25):nrow(plt),] = 0
plt[,(ncol(plt)-buff/0.25):ncol(plt)] = 0


# make grids of right number of these for each set
tsplot <- psplot <- fsplot <- vector()
for(i in 1:fsplotno) {fsplot <- rbind(fsplot, plt)}

for(i in 1:psplotno) {psplot <- rbind(psplot, plt)}

for(i in 1:tsplotno) {tsplot <- rbind(tsplot, plt)}

# make table of all individuals to draw from. Col of all sp, then sites within sp, then reps within site within sp
lengthout = nrow(d)*reps*sites

dat <- data.frame(
  sp = gl(nrow(d), reps*sites, length = lengthout, labels = d$sp),
  site = gl(sites, reps, length = lengthout, labels = sitez),
  rep = gl(reps, 1, length = lengthout)
)
dat$ind <- with(dat, paste(sp, site, rep, sep="_"))
dat$set <- d$Team.Height[match(dat$sp, d$sp)]

# for full shade set, now start filling in 
fs <- subset(dat, subset = 
