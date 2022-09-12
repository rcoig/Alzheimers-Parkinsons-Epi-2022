library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)

#groups representing 1% or less of total deaths were removed from these data in query:
#"D50-D89"
#"H00-H57"
#"H60-H93"
#"L00-L98"
#"M00-M99"
#"O00-O99"
#"P00-P96"
#"Q00-Q99"
#"U00-U99"

###load data
data_GACh <- read_tsv("../data/1999-2019_Gender-Age-byICDChapter.txt")

data1 <- janitor::clean_names(data_GACh)

#remove notes from dfs
data1<-as.data.frame(data1[-c(155:229),])

data1<-rename(data1, age = ten_year_age_groups_code)
data1<-rename(data1, ucd = ucd_icd_chapter)
data1<-rename(data1, ch_grp = ucd_icd_chapter_code)
data1<-rename(data1, MR = crude_rate, MRSE = crude_rate_standard_error)
data1$logMR<-log(data1$MR)

mort_wide <- melt(data1[,c(2,5,7,13)], id=c("gender","age","ch_grp"))
mort_long<-dcast(mort_wide, variable + age + ch_grp ~ gender)
mort_long$diff<-mort_long$Male-mort_long$Female

write.csv(mort_long,"mort_long_ch_grp.csv")