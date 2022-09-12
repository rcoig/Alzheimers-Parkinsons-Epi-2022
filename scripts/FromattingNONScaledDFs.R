library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)

###load data
catcode<-read.csv("../data/mort_long_cat_code.csv",header=T)

###################fill each df NAs with zeros before center and scale
catcode_wide<-melt(catcode[,c(1,4,7)], id=c("age","cat_code"))
catcode_wide<-dcast(catcode_wide, variable + age ~ cat_code)
row.names(catcode_wide)<-catcode_wide$age
catcode_wide$variable<-NULL
catcode_wide$age<-NULL
## Remove columns with more than 75% NA
catcode_wide<-catcode_wide[, which(colMeans(!is.na(catcode_wide)) > 0.75)]
catcode_wide[is.na(catcode_wide)] = 0
write.csv(catcode_wide,"mort_wide_catcode.csv")

