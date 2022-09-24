library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)


###load data
data <- read_tsv("../data/1999-2020_Gender-Age-byYear-byICD.txt")
data <- janitor::clean_names(data)

data2<-as.data.frame(data[-c(6161:6229),])
data2<-rename(data2, age = ten_year_age_groups_code)
data2<-rename(data2, ucd = ucd_icd_chapter)
data2<-rename(data2, ucd_group = ucd_icd_chapter_code)
data2<-rename(data2, MR = crude_rate, MRSE = crude_rate_standard_error)

percentages <- read_tsv("../data/DiseasePercentages.txt")
percentages <- janitor::clean_names(percentages)
percentages$labels <- paste(percentages$ucd_icd_chapter_code,percentages$percent_of_total_deaths)
percentages<-rename(percentages, ucd_group = ucd_icd_chapter_code)
percentages$percent<-substr(percentages$percent_of_total_deaths,1,nchar(percentages$percent_of_total_deaths)-1)


#remove groups representing 1% or less of total deaths
data2<-data2[data2$ucd_group!="H00-H57",]
data2<-data2[data2$ucd_group!="H60-H93",]
data2<-data2[data2$ucd_group!="O00-O99",]
data2<-data2[data2$ucd_group!="P00-P96",]
data2<-data2[data2$ucd_group!="Q00-Q99",]
data2<-data2[data2$ucd_group!="L00-L98",]
data2<-data2[data2$ucd_group!="U00-U99",]
data2<-data2[data2$ucd_group!="M00-M99",]
data2<-data2[data2$ucd_group!="D50-D89",]

#removing 2020 to avoid COVID
data2<-data2[data2$year!="2020",]

#replace deaths less than 20 with 0 so will be removed later
data2$deaths[data2$deaths < 20] <- 0

data2$MR<-data2$deaths/data2$population*100000
data2$MR<-round(data2$crude_rate, digits = 1)

data_agg<-data2 %>% 
  group_by(age, gender, year) %>% 
  summarise(deaths = sum(deaths), population = sum(population))
data_agg$MR<-data_agg$deaths/data_agg$population
data_agg$logMR<-log(data_agg$MR)

#male versus female all-cause mortality
data_agg %>%
  ggplot(aes(x = age, y = logMR,group = interaction(gender, year),color = gender)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  scale_color_manual(values = c("orange", "purple"))+
  #geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_viridis()+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("log Mortality Rate") +
  ggtitle("All-Cause Mortality by Sex \n(1999-2019, United States)")+
  #facet_grid(frailty ~ type) +
  #facet_wrap(~type)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  #theme(legend.position = "none")+ 
  theme(legend.key=element_blank())+ 
  theme(legend.position = c(0.87, 0.25))+
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
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

data3<-data2[,c(2,5,6,9,12)]

mort_wide <- melt(data3, id=c("gender","year","age","ucd_group"))
mort_long<-dcast(mort_wide, variable + age + year + ucd_group ~ gender)
mort_long$logMR_diff<-log(mort_long$Male/mort_long$Female)
mort_long[mort_long == "Inf"] <- NA
mort_long[mort_long == "-Inf"] <- NA
mort_long[mort_long == "NaN"] <- NA
mort_long<-merge(mort_long,percentages[,c(3,8)],by="ucd_group",all=F)

mort_wide2 <- melt(data_agg, id=c("gender","year","age"))
mort_long2<-dcast(mort_wide2, variable + age + year ~ gender)
mort_long2<-mort_long2[mort_long2$variable=="MR",]
mort_long2$logMR_diff<-log(mort_long2$Male/mort_long2$Female)
mort_long2$ucd_group<-"All Groups"
mort_long2$labels<-"All Groups"

mort_combined<-rbind(mort_long,mort_long2)

#sex difference in all-cause mortality
mort_long2 %>%
  ggplot(aes(x = age, y = logMR_diff,group = year,color = year,fill = logMR_diff)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  scale_color_continuous(breaks = c(1999,2019),
                         labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Sex Difference in Mortality Rate \nlog (Male/Female)") +
  ggtitle("Sex Difference in All-Cause Mortality \n(1999-2019, United States)")+
  #facet_grid(frailty ~ type) +
  #facet_wrap(~type)+
  ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  #theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  #theme(axis.text.x = element_blank())+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(fill=F)+
  #scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())



#sex difference in mortality by ICD-Chapter
mort_combined %>%
  na.omit() %>%
  mutate(across(labels, factor, levels=c("All Groups","I00-I99 33.9%","C00-D48 23.3%",
                                         "J00-J98 9.8%","V01-Y89 6.6%","G00-G98 5.9%",
                                         "E00-E88 4.3%","F01-F99 4.2%","K00-K92 3.8%",
                                         "A00-B99 2.6%","N00-N98 2.5%","R00-R99 1.2%"))) %>%
  ggplot(aes(x = age, y = logMR_diff,group = interaction(labels,year),color = year,fill = logMR_diff)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.9,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-2, 2))+
  scale_color_viridis(breaks = c(1999,2019),labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Sex Difference in Mortality Rate \nlog (Male/Female)") +
  ggtitle("Sex Difference in Mortality by ICD Chapter \n(1999-2019, United States)")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~labels)+
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
  guides(fill=F)+
  #scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

