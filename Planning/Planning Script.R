## Script made by Dan F. for Common Garden Project ##
## Updated 5 Sept 2016 by Cat with new species list ##

### Super simple version. Randomize. Figure out how many individuals of each size to go into each plot, assign to plots. Then manually locate in Excel.

library(dplyr)
library(tidyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set own wd as needed
if(length(grep("Cat Chamberlain", getwd()))==0){ setwd("~/Documents/git/wildhellgarden/analyses") }

setwd("~/Documents/git/wildhellgarden/analyses")

d<-read.csv("Inds for Planting CC.csv", sep=",", header=TRUE)
attach(d)

plots<- as.data.frame(table(d$plotno))

## Common Garden Planning ##
## Started 31 Aug 2016 by Cat ##

## Establish a script to sort out the greenhouse and raised beds as well as create a more fluid
## data sheet for mapping

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors = FALSE)
graphics.off()

# Install Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggplot2", "rshape2", "dplyr", "tidyr")
ipak(packages)

# Set Working Directory
setwd("~/Documents/git/wildhellgarden/Planning")

d <- read.csv("Inds List by Plot.csv", header=TRUE, sep=",")
attach(d)

# Get the site by getting the last two characters of the undercomp rownames
d <- d %>%
  select(Individual, Plotno)
cleaner <- d %>%
  separate(Individual, c("Individual", "Site", "Number"))

plots<-as.data.frame(table(cleaner$plotno))

species<-as.data.frame(table(cleaner$Individual))
site<-as.data.frame(table(cleaner$Site))

## Filter to only show included species
target <- c("ACEPEN", "ACESPI", "ALNINC", "AMECAN", "AROMEL", "BETALL", "BETPAP",
            "BETPOP", "DIELON", "MYRGAL", "SAMRAC", "SORAME", "SPIALB", "SPITOM", 
            "VACMYR", "VIBCAS")
clean <- filter(clean, Species %in% target)

## Export to Excel
write.csv(clean, file = "cg.csv")

## Number of Individuals per Species per Site
d<- clean %>%
  group_by(Site) %>%
  count(Species) %>%
  arrange(Species)
write.csv(d, file ="inds.csv")

###############################
setwd("~/Documents/git/wildhellgarden/Planning")

planning <- read.csv("Inds for Planting Order CC.six.csv", header=TRUE, sep=",")
attach(planning)

sp<-as.data.frame(table(planning$sp))
plots<-as.data.frame(table(planning$plotno))

site<-as.data.frame(table(planning$site))

acespi<- planning %>%
  select(sp, site, ind, plotno) %>%
  filter(sp=="ACESPI")

ace<-as.data.frame(table(acespi$site))

# Get the site by getting the last two characters of the undercomp rownames
plot <- planning %>%
  select(Individual, Plotno)
cleaner <- d %>%
  separate(Individual, c("Individual", "Site", "Number"))

###########################
# Set Working Directory
setwd("~/Documents/git/wildhellgarden/Planning")

final<-read.csv("Common Garden List.csv", sep=",", header=TRUE)
attach(final)

cg<-as.data.frame(table(final$Location))

# Get the site by getting the last two characters of the undercomp rownames
spp <- final %>%
  separate(ID, c("Species", "Site", "Number"))

spec<-as.data.frame(table(spp$Species))
site<-as.data.frame(table(spp$Site))

missing<- spp%>%
  filter(Location == "none") %>%
  unite(spp, Species, Site, sep="_", remove=FALSE)
compl<-as.data.frame(table(missing$spp))

sp.miss<-as.data.frame(table(missing$Species))
site.miss<-as.data.frame(table(missing$Site))

specie<-c("ACESPI","AMECAN","BETPAP","SORAME","SPITOM")
missing.more <-filter(missing, Species %in% specie)
missing.more<-missing.more[order(missing.more$Species, missing.more$Site),]


