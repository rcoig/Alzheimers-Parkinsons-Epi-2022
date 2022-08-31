library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)


###set working drive
setwd("C:/Users/renec/Desktop/Promislow/CDC/data")
data <- read_tsv("1999-2020_Gender-Age-byYear.txt")
data <- janitor::clean_names(data)

data2<-data[-c(356:402),]
data2<-as.data.frame(data2)
data2<-data2[!is.na(data2$ten_year_age_groups),]#removes totals
data2<-data2[,c(2,4,7,10)]
data2<-rename(data2, age = ten_year_age_groups_code)

mort_wide <- melt(data2, id=c("gender","year","age"))
mort_long<-dcast(mort_wide, variable + age + year ~ gender)
mort_long$logMR_diff<-log(mort_long$Male/mort_long$Female)

#sex difference in all-cause mortality
mort_long %>%
  ggplot(aes(x = age, y = logMR_diff,group = year,color = year,fill = logMR_diff)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  scale_color_viridis()+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Sex Difference in Mortality Rate \nlog (Male/Female)") +
  ggtitle("Sex Difference in All-Cause Mortality \n(1999-2020, United States)")+
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

#male versus female all-cause mortality
data2 %>%
  ggplot(aes(x = age, y = log(crude_rate),group = interaction(gender, year),color = gender)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  #geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_viridis()+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("log Mortality Rate") +
  ggtitle("All-Cause Mortality by Sex \n(1999-2020, United States)")+
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