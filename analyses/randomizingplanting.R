### Based off of Lizzie's planning thoughts
### Dan

## What to plant in the common garden? ##

## housekeeping
library(dplyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set own wd as needed
if(length(grep("danflynn", getwd()))==0){ setwd("~/Documents/git/projects/treegarden/wildhellgarden/analyses") }

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
plt <- matrix(nrow = 8 / 0.25 + 1, ncol= 8 / 0.25 + 1)
# fill in buffer of 1 m, i.e. first four rows, first four cols...
buff = 1
plt[1:(buff/0.25),] = 0
plt[,1:(buff/0.25)] = 0
plt[((nrow(plt)+1)-buff/0.25):nrow(plt),] = 0
plt[,((ncol(plt)+1)-buff/0.25):ncol(plt)] = 0

# make grids of right number of these for each set
tsplot <- psplot <- fsplot <- vector()
for(i in 1:fsplotno) {fsplot <- rbind(fsplot, plt)}
# fill in the NAs with index values
for(i in 1:length(fsplot)){ if(is.na(fsplot[i])) fsplot[i] = i }

for(i in 1:psplotno) {psplot <- rbind(psplot, plt)}
# fill in the NAs with index values
for(i in 1:length(psplot)){ if(is.na(psplot[i])) psplot[i] = i }

for(i in 1:tsplotno) {tsplot <- rbind(tsplot, plt)}
# fill in the NAs with index values
for(i in 1:length(tsplot)){ if(is.na(tsplot[i])) tsplot[i] = i }


# make table of all individuals to draw from. Col of all sp, then sites within sp, then reps within site within sp
lengthout = nrow(d)*reps*sites

dat <- data.frame(
  sp = gl(nrow(d), reps*sites, length = lengthout, labels = d$sp),
  site = gl(sites, reps, length = lengthout, labels = sitez),
  rep = gl(reps, 1, length = lengthout)
)
dat$ind <- with(dat, paste(sp, site, rep, sep="_"))
dat$set <- d$Team.Height[match(dat$sp, d$sp)]
dat$space <- d$Team.spacing[match(dat$sp, d$sp)]

##### start with  sun shrubs
ps <- subset(dat, set == "Shrub Sun") 

# now draw from this data frame and fill in the matrix, using the size requirement for that species
# first, shuffle the data frame
ps <- ps[sample(rownames(ps)),]

xy <- vector() # for position

# now go row by row through the data frame, filling in as necessary
for(i in 1:nrow(ps)){ # i = 1
  fx <- ps[i,]
  
  # start filling this in from the top left of first plot
  start <- min(which(psplot!=0 & psplot != buffname & !psplot %in% ps[-1,"ind"]))

  # make a buffer around that individual
  buffspace = (fx$space/0.25) - 1 # this many cells around the individual will be the buffer
  buffname = "buff"#paste(fx$ind, "\n buff")
  
  if(i == 1) {
    
    psplot[start] = fx$ind
    lastrow = which(apply(psplot, 1, function(x) any(x == fx$ind)))
    lastcol = which(apply(psplot, 2, function(x) any(x == fx$ind)))
    
    toprow = lastrow-buffspace; if(toprow<0) toprow = 0
    bottomrow = lastrow+buffspace; if(bottomrow>nrow(psplot)) bottomrow = nrow(psplot)
    leftcol =  lastcol-buffspace; if(leftcol<0) leftcol = 0
    rightcol = lastcol+buffspace; if(rightcol>ncol(psplot)) rightcol = ncol(psplot)
    
    psplot[toprow:bottomrow, leftcol:rightcol] = buffname
    psplot[start] = fx$ind
  }
  
  if(i != 1) { # if not the first one in this set, need to check buffer space
    
    # see if this start space will work
    psplot[start] = fx$ind
    lastrow = which(apply(psplot, 1, function(x) any(x == fx$ind)))
    lastcol = which(apply(psplot, 2, function(x) any(x == fx$ind)))
    
    toprow = lastrow-buffspace; if(toprow<0) toprow = 0
    bottomrow = lastrow+buffspace; if(bottomrow>nrow(psplot)) bottomrow = nrow(psplot)
    leftcol =  lastcol-buffspace; if(leftcol<0) leftcol = 0
    rightcol = lastcol+buffspace; if(rightcol>ncol(psplot)) rightcol = ncol(psplot)
    
    buffrange = psplot[toprow:bottomrow, leftcol:rightcol]
    
    # if there is another individual in the proposed buffer, need to offset. Easy test: is the value shorter than 10 characters
    if(!all(buffrange == fx$ind | nchar(buffrange) < 10)){
      
      bufflastrow = which(apply(buffrange, 1, function(x) any(x == ps[i-1,"ind"])))
      bufflastcol = which(apply(buffrange, 2, function(x) any(x == ps[i-1,"ind"])))
      
      offsetrc = which(c(bufflastrow, bufflastcol)==min(c(bufflastrow, bufflastcol))) # 1 row, 2 = col
      
      if(offsetrc == 1) {
        start = start+bufflastrow
      } # end of offset by row 
    }
    psplot[toprow:bottomrow, leftcol:rightcol] = buffname
    psplot[start] = fx$ind
  } # end not first one 

  # find the xy coordinates for this position
  rowx = which(apply(psplot, 1, function(x) any(x == fx$ind)))
  colx = which(apply(psplot, 2, function(x) any(x == fx$ind)))
  
  xy <- rbind(data.frame(ind = fx$ind, row = rowx, col = colx))
  
} # end 

write.csv(psplot, row.names=F, file = "Shrub Sun Map.csv")

write.csv(xy, row.names=F, file = "Shrub Sun Location.csv")

# did we use all of them? 

length(unique(psplot[nchar(psplot) > 10])) == nrow(ps)

### FIRST ONE GETTING OVERWRITTEN  ?? Fix. Problem came when changing from NA to numeric index. ####


##### Trees
# This works if the n-1 individual is present in the buffer, but not otherwise (e.g., when not on the first column any more.)
      
ts <- ts[sample(rownames(ts)),]

xy <- vector() # for position

for(i in 1:nrow(ts)){ # i = 1
  fx <- ts[i,]
  
  # start filling this in from the top left of first plot
  start <- min(which(is.na(tsplot)))
  
  # make a buffer around that individual
  buffspace = (fx$space/0.25) - 1 # this many cells around the individual will be the buffer
  buffname = "buff"#paste(fx$ind, "\n buff")
  
  if(i == 1) {
    
    tsplot[start] = fx$ind
    lastrow = max(which(!apply(tsplot, 1, function(x) all(is.na(x) | x == "0" | x == buffname)))) 
    lastcol = max(which(!apply(tsplot, 2, function(x) all(is.na(x) | x == "0" | x == buffname)))) 
    
    toprow = lastrow-buffspace; if(toprow<0) toprow = 0
    bottomrow = lastrow+buffspace; if(bottomrow>nrow(tsplot)) bottomrow = nrow(tsplot)
    leftcol =  lastcol-buffspace; if(leftcol<0) leftcol = 0
    rightcol = lastcol+buffspace; if(rightcol>ncol(tsplot)) rightcol = ncol(tsplot)
    
    tsplot[toprow:bottomrow, leftcol:rightcol] = buffname
    tsplot[start] = fx$ind
  }
  
  if(i != 1) { # if not the first one in this set, need to check buffer space
    
    # see if this start space will work
    tsplot[start] = fx$ind
    lastrow = which(apply(tsplot, 1, function(x) any(x == fx$ind)))
    lastcol = which(apply(tsplot, 2, function(x) any(x == fx$ind)))
    
    toprow = lastrow-buffspace; if(toprow<0) toprow = 0
    bottomrow = lastrow+buffspace; if(bottomrow>nrow(tsplot)) bottomrow = nrow(tsplot)
    leftcol =  lastcol-buffspace; if(leftcol<0) leftcol = 0
    rightcol = lastcol+buffspace; if(rightcol>ncol(tsplot)) rightcol = ncol(tsplot)
    
    buffrange = tsplot[toprow:bottomrow, leftcol:rightcol]
    
    # if there is another individual in the proposed buffer, need to offset
    if(!all(buffrange == "buff" | buffrange == "0" | is.na(buffrange) | buffrange == fx$ind)){
      
      # find the max row which is occupied by another individual
      bufflastrow = max(which( apply(buffrange, 1, function(x) any(match(x, ts[-i,"ind"]))) ))
      # same for columns
      bufflastcol = max(which(apply(buffrange, 2, function(x) any(match(x, ts[-i,"ind"])))))
      
      # which is smaller of these two. either move down or move over, depending on row or column the smaller one to adjust
      offsetrc = which(c(bufflastrow, bufflastcol) == min(c(bufflastrow, bufflastcol))) # 1 row, 2 = col
      
      # moving down rows is easy. Moving over columns is trickier
      tsplot[start]
      dim(tsplot)
      
      which(tsplot[lastrow, lastcol+bufflastcol])
      start = ifelse(offsetrc == 1,
             start+bufflastrow,
             start+bufflastcol)
      
    }
    tsplot[toprow:bottomrow, leftcol:rightcol] = buffname
    tsplot[start] = fx$ind
  } # end not first one 
  
  # find the xy coordinates for this position
  rowx = which(apply(tsplot, 1, function(x) any(x == fx$ind)))
  colx = which(apply(tsplot, 2, function(x) any(x == fx$ind)))
  
  xy <- rbind(data.frame(ind = fx$ind, row = rowx, col = colx))
  
} # end 

write.csv(tsplot, row.names=F, file = "Trees Map.csv")

write.csv(xy, row.names=F, file = "Trees Location.csv")

# did we use all of them? 

length(unique(tsplot[tsplot != "buff" & tsplot != 0 & !is.na(tsplot)])) == nrow(ps)

