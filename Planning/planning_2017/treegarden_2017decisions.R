## Started 23 Aopr 2017 ##
## By Lizzie ##

## Making decisions on what to plant in common garden onward ##
## A bit of a pain but hopefully gets the job done. ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/git/projects/treegarden/wildhellgarden/Planning/planning_2017")

# Get files
gh <- read.csv("input/GH7 Inventory - Spring 2017.csv", header=TRUE)
cg.all <- read.csv("input/CommonGardenList.Updated.csv", header=TRUE)

# Some cleanup (note that extra six rows get enetered from gh, they are empty)
gh$X <- NULL
gh <- subset(gh, id!="")
names(cg.all)[names(cg.all)=="Species"] <- "id"
gh$spsite <- paste(gh$species, gh$site)

# get species out for commonn garden (cg)
idx <- strsplit(cg.all$id, "_")
cg.all$species <- unlist(lapply(idx, function(x) x[1]))
cg.all$spsite <- paste(cg.all$species, cg.all$site)
    
# What's in the garden?
cg <- subset(cg.all, Location=="CG")
table(cg$species, cg$site)
# table(gh$species, gh$site)

# What needs to be in the garden but is not?
cg.need <- subset(cg.all, Location=="none")
using <- gh[which(gh$spsite %in% unique(cg.need$spsite)),]
# table(using$spsite)
sort(gh$id[which(gh$spsite %in% unique(cg.need$spsite))])

# What is needed that we have:
# need 3, but happy to have 1: ACESPI GR 
# need 6, but happy to have 1: ACESPI WM 
# need 5, but happy to have 3: ACESPI SH
# need 6, but happy to have 2: AMECAN SH
# need 5, have 6: BETPAP GR
# need 6, have 9: BETPAP HF
# need 1, have 6: MYRGAL WM
# need 6, have 5: MYRGAL GR
# need 2, have 37: SAMRAC GR
# need 1, have 18: SORAME SH
# need 6, have 31: SPITOM GR
# need 6, have 8: SPITOM SH
# need 6, have 1: VIBCAS HF
# need 2, have 30: SPIALB SH (somehow did not show up as none in cg location)
# need 4, have 21: SPIALB WM (somehow did not show up as none in cg location)

# What are we not planning on needing for garden?
notusing <- gh[which(!gh$spsite %in% unique(cg.need$spsite)),]
table(notusing$spsite)

# What to add to garden?
# Bump up to 8 some of the species reps:
# 2 x 4 ALNINC (only 1 more at WM so 7 to add total)
# 2 x 3 BETALL
# 2 x 3 BETPOP
# 2 x 3 DIELON
# 2 x 4 SPIALB
# 2 x 4 SPITOM
# 2 x 3 VIBCAS

# Add these new species
# 2 x 2 QUERUB
# 2 x 2 VACMYR


####################
# Sad, but true, I made up a list by hand in treegarden_2017add.xlsx
# Let's see how it looks .... 
####################

addme <- read.csv("input/treegarden_2017add.csv", header=TRUE)
addme$spsite <- paste(addme$species, addme$site)
table(addme$species, addme$site)
table(gh$species, gh$site) # note that 1 WM ALNINC is incorrectly labelled, we really only have 1 at WM to add

table(addme$species, addme$site)
table(cg$species, cg$site)

# Note! This list does not consider death .... we should fill in dead things if possible

# What is left?
# aggregate by ID to start ...
addme.agg <- aggregate(addme["species"], addme["id"], FUN=length)
gh.agg <- aggregate(gh["species"], gh["id"], FUN=length)
# now merge ...and get diff
notusing.take2 <- merge(addme.agg, gh.agg, by="id", suffixes=c(".add", ".tot.gh"), all.y=TRUE)
notusing.take2$species.add[is.na(notusing.take2$species.add)==TRUE] <- 0

notusing.take2$leftover <- notusing.take2$species.tot.gh-notusing.take2$species.add

leftover <- subset(notusing.take2, leftover>0)
leftover <- subset(leftover, select=c("id", "leftover"))
leftover$species <- substr(leftover$id, 1,6)

write.csv(leftover, "output/gh_unusedplants.csv", row.names=FALSE)

# What is left empty in common garden? 
cg.present <- subset(cg, Location=="RB"|Location=="CG")
cg.present.bind <- subset(cg.present, select=c("spsite", "species", "site"))
addme.bind <- subset(addme, select=c("spsite", "species", "site"))
cg.2017 <- rbind(cg.present.bind, addme.bind)
table(cg.2017$species, cg.2017$site)
