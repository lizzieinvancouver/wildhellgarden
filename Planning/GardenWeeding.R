## Common Garden List

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)

# Set Working Directory
setwd("~/Documents/git/wildhellgarden")
d<-read.csv("Planning/garden.list.csv", header=TRUE)

# Make site code
d$site<-substr(d$Species, 8, 9)

write.csv(d, "Planning/CommonGardenList.Updated.csv", row.names = FALSE)
