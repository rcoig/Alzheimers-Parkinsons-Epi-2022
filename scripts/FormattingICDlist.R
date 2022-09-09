library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)

###load data
codes <- read.csv("../data/icd10cm_tabular_2022.csv",header=T)
codes$ch_grp<-str_sub(codes$ch_desc, start= -8)
codes$ch_grp<-substr(codes$ch_grp, 1,7)
codes<-codes[,c(19,1:18)]
codes<-codes[-c(54107:54118),]
codes$ch_desc = substr(codes$ch_desc,1,nchar(codes$ch_desc)-9)
codes$subch_desc = substr(codes$subch_desc,1,nchar(codes$subch_desc)-9)
codes<-codes[,c(1:4,18,5:19)]
codes<-codes[,-c(8:16)]
codes<-codes[,-c(8,10)]
write.csv(codes,"codes.csv")
