## Integrating climate data into CG data
# Cat - 11 Feb 2021

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(egg)

# Set Working Directory
setwd("~/Documents/git/treespotters/analysis/input")
b<-read.csv("individual_phenometrics_data.csv",header=TRUE)

cols <-viridis_pal(option="viridis")(3)


### Let's add in Climate data now
clim <- read.csv("~/Documents/git/microclimates/analyses/output/clean_addinclimate.csv", header=TRUE)
clim <- clim[(clim$climatetype=="weldhill"),]
clim <- clim[(clim$year>2017),]
clim <- clim[!duplicated(clim),]

spring <- clim[(clim$doy>=1 & clim$doy<=140),]

climate <- ggplot(spring, aes(x=doy, y=tmean, col=as.factor(year))) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(aes(col=as.factor(year), fill=as.factor(year)), stat="smooth", method="loess", se=TRUE, span=0.9) + 
  scale_color_manual(name = "Year", values=cols, labels = c("2018", "2019", "2020")) +
  scale_fill_manual(name = "Year", values=cols, labels = c("2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"))

quartz()
climate

clim2018 <- ggplot(spring[(spring$year==2018),], aes(x=doy, y=tmean), col=cols[1]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[3], fill=cols[3], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")

clim2019 <- ggplot(spring[(spring$year==2019),], aes(x=doy, y=tmean), col=cols[2]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[4], fill=cols[4], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")

clim2020 <- ggplot(spring[(spring$year==2020),], aes(x=doy, y=tmean), col=cols[3]) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(col=cols[5], fill=cols[5], stat="smooth", method="loess", se=TRUE, span=0.9) + 
  #scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  #scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018", "2019", "2020")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"),
                                                                 legend.position = "none")

climall <- ggarrange(clim2018, clim2019, clim2020,  ncol=3)


clim$mst <- ave(clim$tmean, clim$year)

###### Now let's add in the clean bbch data
bbch <- read.csv("~/Documents/git/wildhellgarden/analyses/")





