library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)


#Could you clean and combine all the different height data files into one?
# 1. There is height data from 2019 which is clean and in m: analyses/2019_data/2019_heightanddbh.csv
# Could you convert this file to just be ind, plot, and height columns without the blank rows?
# 2. There is also height data in the 2020_data that has not been cleaned and the values will need to be 
# corrected for the measuring rod issues and converted to m
# 3. And of course the 2022 data needs to be cleaned and corrected as well
# 4-7. Same instructions to look for outliers and make plots as Grace above.

#Notes on the measure stick can be found here: misc/Jun2021measuringstick/measuringstick.txt

# 2019 Cleaning
wildhell_2019 <- read_csv("Documents/WL2022/WildHell/wildhellgarden/analyses/2019_data/2019_heightanddbh.csv")
View(wildhell_2019)

wildhell_2019 <- wildhell_2019 %>%
  select(IND, Plot, Height)

wildhell_2019 <- wildhell_2019[complete.cases(wildhell_2019),]

# 2020 Cleaning
wildhell_2020 <- read_csv("Documents/WL2022/WildHell/wildhellgarden/analyses/2020_data/2020_CG_datasheet.csv")
View(wildhell_2020)
colnames(wildhell_2020)

wildhell_2020 <- pivot_longer(wildhell_2020,
             cols = 3:45,
             names_to = "Date",
             values_to = "Height"
)

# 2022 Cleaning
wildhell_2022 <- read_csv("Documents/WL2022/WildHell/height_2022.csv")

# clean height data

## trying with my own measurements ##
# 1ft = 0.305m
1/0.305
# conversion factor is 3.2787

## -----------------------------------------------------------------------------------------------------------##
# first compare the actual conversions with our rod conversions in m
# 1ft = 3.2808m

#4/3.2808 # = 1.219215m
# our rod conversion is 1.14m
#5.6/3.2808 # = 1.706901
#our rod conversion is 1.61
#7/3.2808 # = 2.133626m
# our rod conversion is 2.02m #### DOUBLE CHECK THIS###
#7.2/3.2808 # = 2.194587
# our rod conversion is 2.07m
#7.3/3.2808 # = 2.225067
# our rod conversion is 2.11m

# for 4ft
#1.219215 - 1.14 # 0.079215

#for 5.6ft
#1.706901 - 1.61 # 0.096901

#for 7ft
#2.133626 - 2.02 # 0.11362

#for 7.2ft
#2.194587 - 2.07 # 0.124587

#for 7.3ft
#2.225067 - 2.11 # 0.115067

#x <- c(2, 3, 4, 5.6, 7, 7.2, 7.3)
#y <- c(0.08961, 0.0944, 0.079215, 0.096901, 0.11362, 0.124587, 0.115067)
#conversion <- cbind(x, y)
#plot(conversion)

#ft <- c(2, 3, 4, 5.6, 7, 7.2, 7.3)
#change_in_m <- c(0.293984, 0.309744, 0.2599, 0.3179, 0.3728, 0.408744, 0.377512 )
#ft_m <- cbind(ft, change_in_m)
#plot(ft_m)

#2 - (0.52*3.2808)

#5.6/5.282

## attempting a new stragety ##

# converting our given m values to ft
# 1.14m = 3.74ft
# 1.61m = 5.282ft
# 2.02m = 6.627ft
# 2.07m = 6.791ft
# 2.11m = 6.922ft

# finding conversion factor
# for 4ft
#4/3.74 #1.0695

# for 5.6ft
#5.6/5.282 #1.0602

# for 7ft
#7/6.627 #1.0563

# for 7.2ft
#7.2/6.791 #1.0602

# for 7.3ft
#7.3/6.922 #1.0546

#conversion factor is somewhere between 1.054 and 1.07
#a <- c(1.0695, 1.0602, 1.0563, 1.0602, 1.0546)
#summary(a)

# average is 1.060 so we can use 1.060 to convert the height

## trying with my own measurements ##
# 1ft = 0.305m
#1/0.305
# conversion factor is 3.2787

#11/3.2787
