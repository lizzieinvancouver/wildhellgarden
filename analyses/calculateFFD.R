# code started November 18, 2024 by Deirdre
# aim: to use tmin data from daymet to calculate frost free days for our four common garden sites
# note: daymet data downloaded in check_daymet.R
require(sharpshootR)
rm(list = ls())
options(stringsAsFactors = FALSE)

#setwd
if(length(grep("deirdre", getwd())) > 0){
  setwd("~/Documents/github/wildhellgarden")
}else {   setwd("~/Documents/github/wildhellgarden")}

d <- read.csv("analyses/daymetData/dayMet.csv")

d$datetime <- as.Date(sprintf('%s %s', d$year, d$doy), format="%Y %j")
d$value <- d$tmin

d <- subset(d, year < 2022)
coords <- c("Harvard forest", "White Mountains", "Grant", "St. Hippolyte")
timeSeries <- unique(d$year)
frostFree <- data.frame(site = rep(sites, each = length(timeSeries)), year = rep(timeSeries, times = 4))
  
ffdData <-  vector()

for(s in 1: length(coords)){

  temp <- subset(d, site == coords[s])
  
  for(y in 1: length(unique(d$year))){
    tempY <- subset(temp, year == timeSeries[y])
    
    ffd <- FFD(tempY, returnDailyPr = TRUE, frostTemp = 0)
    
    ffdData <- rbind(ffdData, ffd$summary)
    
  }
}

f <- cbind(frostFree, ffdData)

write.csv(f, "analyses/daymetData/frostFreeDays.csv", row.names = F)
