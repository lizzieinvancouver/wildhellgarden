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

# Integrate Phenology Data and compare methodologies
# Set Working Directory
setwd("~/Documents/Temporal Ecology/Common Garden")

gh <- read.csv("gh.data.csv", header=TRUE, sep=",")
attach(gh)

# Get the site by getting the last two characters of the undercomp rownames
gh <- gh %>%
  select(ID, Number, Location)
clean <- gh %>%
  separate(ID, c("Individual", "Site"))

clean$Species <- substring(gh1[,1], 1, 6) 

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
