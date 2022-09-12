library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)

###load data
codes <- read.csv("../data/codes.csv",header=T)
allcause<-read.csv("../data/mort_long_all_cause.csv",header=T)
catcode<-read.csv("../data/mort_long_cat_code.csv",header=T)
ch_grp<-read.csv("../data/mort_long_ch_grp.csv",header=T)

###################fill each df NAs with zeros before center and scale
allcause_wide<-melt(allcause[,c(1,4:5)], id=c("age","ch_grp"))
allcause_wide<-dcast(allcause_wide, variable + age ~ ch_grp)
row.names(allcause_wide)<-allcause_wide$age
allcause_wide$variable<-NULL
allcause_wide$age<-NULL
write.csv(allcause_wide,"mort_wide_allcause.csv")

ch_grp_wide<-melt(ch_grp[,c(1,4:5)], id=c("age","ch_grp"))
ch_grp_wide<-dcast(ch_grp_wide, variable + age ~ ch_grp)
row.names(ch_grp_wide)<-ch_grp_wide$age
ch_grp_wide$variable<-NULL
ch_grp_wide$age<-NULL
write.csv(ch_grp_wide,"mort_wide_ch_grp.csv")

length(unique(catcode$cat_code))#759 codes to start
catcode_wide<-melt(catcode[,c(1,4,7)], id=c("age","cat_code"))
catcode_wide<-dcast(catcode_wide, variable + age ~ cat_code)
row.names(catcode_wide)<-catcode_wide$age
catcode_wide$variable<-NULL
catcode_wide$age<-NULL
## columns with more than 75% NA
NA75list<-catcode_wide[, which(colMeans(!is.na(catcode_wide)) < 0.75)]
NA75list<-as.data.frame(t(NA75list))
NA75list<-row.names(NA75list)
## Remove columns with more than 75% NA
names.use <- names(catcode_wide)[!(names(catcode_wide) %in% NA75list)]
catcode_wide <- catcode_wide[, names.use]#402 left
## columns with low variance (no change across lifespan) variance <.02
library(resample)
lowvariancelist<-catcode_wide[, which(colVars(catcode_wide) <= 0.02)]#88
lowvariancelist<-as.data.frame(t(lowvariancelist))
lowvariancelist<-row.names(lowvariancelist)
#BEFORE SCALING, remove those that have low variance (no change across lifespan)
#remove columns that have less than .02 variance
names.use <- names(catcode_wide)[!(names(catcode_wide) %in% lowvariancelist)]
catcode_wide <- catcode_wide[, names.use]#314 left
#catcode_wide[is.na(catcode_wide)] = 0
write.csv(catcode_wide,"mort_wide_catcode_highervariance.csv")


mort_wide<-cbind(allcause_wide,ch_grp_wide,catcode_wide)
mort_wide_scale<-as.data.frame(scale(mort_wide))
#mort_wide_noNaN<-mort_wide_scale[, colSums(is.na(mort_wide_scale)) != nrow(mort_wide_scale)]
write.csv(mort_wide_scale,"mort_wide_scale_highervariance.csv")

mort_long_scale<-as.data.frame(t(mort_wide_scale))
write.csv(mort_long_scale,"mort_long_scale_highervariance.csv")

mort_wide_scale$age<-row.names(mort_wide_scale)
mort_long_plot<-melt(mort_wide_scale, id=c("age"))
mort_long_plot<-rename(mort_long_plot, diff = value)
mort_long_plot<-rename(mort_long_plot, group = variable)
mort_long_plot$type<-rep(c("all","ch_grp","cat_code"), times = c(7,77,2198))
write.csv(mort_long_plot,"mort_long_plot_highervariance.csv")




