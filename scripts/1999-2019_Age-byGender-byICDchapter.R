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
data_GA <- read_tsv("../data/1999-2019_Gender-Age.txt")

data1 <- janitor::clean_names(data_GACh)
data2 <- janitor::clean_names(data_GA)

#remove notes from dfs
data1<-as.data.frame(data1[-c(155:229),])
data2<-as.data.frame(data2[-c(15:85),])

data1<-rename(data1, age = ten_year_age_groups_code)
data1<-rename(data1, ucd = ucd_icd_chapter)
data1<-rename(data1, ucd_group = ucd_icd_chapter_code)
data1<-rename(data1, MR = crude_rate, MRSE = crude_rate_standard_error)

data2<-rename(data2, age = ten_year_age_groups_code)
data2<-rename(data2, MR = crude_rate, MRSE = crude_rate_standard_error)
data2$ucd_group<-"Combined"
data2$labels<-"Combined"


percentages <- read_tsv("../data/1999-2019_Gender-ICDpercentages.txt")
percentages <- janitor::clean_names(percentages)
percentages<-as.data.frame(percentages[-c(41:112),])
percentages$labels <- paste(percentages$ucd_icd_chapter_code,percentages$percent_of_total_deaths)
percentages<-rename(percentages, ucd_group = ucd_icd_chapter_code)
percentages$percent<-substr(percentages$percent_of_total_deaths,1,nchar(percentages$percent_of_total_deaths)-1)
#remove groups representing 1% or less of total deaths
percentages<-percentages[percentages$ucd_group!="H00-H57",]
percentages<-percentages[percentages$ucd_group!="H60-H93",]
percentages<-percentages[percentages$ucd_group!="O00-O99",]
percentages<-percentages[percentages$ucd_group!="P00-P96",]
percentages<-percentages[percentages$ucd_group!="Q00-Q99",]
percentages<-percentages[percentages$ucd_group!="L00-L98",]
percentages<-percentages[percentages$ucd_group!="U00-U99",]
percentages<-percentages[percentages$ucd_group!="M00-M99",]
percentages<-percentages[percentages$ucd_group!="D50-D89",]

#add labels to groups with percentages
data1<-merge(data1,percentages[,c(2,5,11)],by=c("ucd_group","gender"),all=F)
sum(as.numeric(percentages$percent))

#combine data sets
data1<-data1[,c(2,1,6,8:11,13)]
data2<-data2[,c(2,11,5,6:9,12)]
data<-rbind(data1,data2)

#log the Mortality rates for plotting
data$logMR<-log(data$MR)

data$ucd_group<-as.factor(data$ucd_group)
data$ucd_group<-droplevels(data$ucd_group)
levels(data$ucd_group)
data$gender<-as.factor(data$gender)

data<-data %>% mutate(across(ucd_group, factor, levels=c("Combined","I00-I99","C00-D48",
              "J00-J98","V01-Y89","G00-G98","E00-E88","F01-F99","K00-K92","A00-B99",     
                         "N00-N98","R00-R99"))) 

#plot male and female mortality by ICD chapter separately
data %>%
  ggplot(aes(x = age, y = logMR,group = interaction(ucd_group,gender),fill = gender, label=labels)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.9,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-2, 2))+
  #scale_color_viridis(breaks = c(1999,2019),labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("log Mortality Rate (per 100,000)") +
  ggtitle("Mortality by Sex and ICD Chapter \n(1999-2019, United States)")+
  #facet_grid(age ~ gender) +
  facet_wrap(~ucd_group)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  #theme(legend.position = "none")+ 
  #theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.text.x = element_blank())+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  #guides(fill=F)+
  #scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

#centered and scaled, for hierarchical clustering
data_wide<-data[,c(1,2,3,9)]
data_wide$genderage<-paste(data_wide$gender,data_wide$age)
data_wide2<-melt(data_wide[,c(2,4,5)], id=c("genderage","ucd_group"))
data_long<-dcast(data_wide2, variable + genderage ~ ucd_group)
#data_scale<-data_long
#rownames(data_scale) <- data_scale[,2]
#data_scale<-data_scale[,-c(1:2)]
data_scale <- scale(data_long[,3:14])  
#data_scaleF<-data_scale[,c(1:7)]
#data_scaleM<-data_scale[,c(8:14)]
#columns<-c("25-34","35-44","45-54","55-64","65-74","75-84","85+")
data_scale<-as.data.frame(data_scale)
data_scale$genderage<-data_long$genderage
data_scale$age<-c("25-34","35-44","45-54","55-64","65-74","75-84","85+")
gender<-c("Female","Male")
data_scale$gender<-rep(gender, each=7)
data_scale$genderage<-NULL

data_scale_wide<-melt(data_scale, id=c("gender","age"))
data_scale_wide<-rename(data_scale_wide, ucd_group = variable)
data_scale_wide<-data_scale_wide %>% mutate(across(ucd_group, factor, levels=c("Combined","I00-I99","C00-D48",
                                                         "J00-J98","V01-Y89","G00-G98","E00-E88","F01-F99","K00-K92","A00-B99",     
                                                         "N00-N98","R00-R99"))) 

#replot as scaled to better view shape differences
data_scale_wide %>%
  ggplot(aes(x = age, y = value,group = interaction(ucd_group,gender),fill = gender)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.9,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-2, 2))+
  #scale_color_viridis(breaks = c(1999,2019),labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("log MR scaled by ucd group") +
  ggtitle("centered and scaled")+
  #facet_grid(age ~ gender) +
  facet_wrap(~ucd_group)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  #theme(legend.position = "none")+ 
  #theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.text.x = element_blank())+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  #guides(fill=F)+
  #scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())





