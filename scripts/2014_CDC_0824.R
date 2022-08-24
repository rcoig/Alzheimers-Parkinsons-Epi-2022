library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(viridis)
library(scales)
###set working drive
setwd("C:/Users/renec/Desktop/Promislow/CDC")
data_85 = read_tsv("85+ top50 causes by gender.txt")
data_45 = read_tsv("45-54 top50 causes by gender.txt")
data_85 <- janitor::clean_names(data_85)
data_45 <- janitor::clean_names(data_45)

data_85$age<-"85+"
frail<-as.data.frame(data_85[-c(546:727),])#removing excess rows
frail$notes<-as.character(frail$notes)
frail_totals<-frail[!is.na(frail$notes),]
frail<-frail[is.na(frail$notes),]

data_45$age<-"45-55"
young<-as.data.frame(data_45[-c(546:727),])#removing excess rows
young$notes<-as.character(young$notes)
young_totals<-young[!is.na(young$notes),]
young<-young[is.na(young$notes),]

frail$underlying_cause_of_death_code <- substr(frail$underlying_cause_of_death_code, 0, 3)
young$underlying_cause_of_death_code <- substr(young$underlying_cause_of_death_code, 0, 3)

frail %>% 
  group_by(age, gender, ucd = underlying_cause_of_death_code, population) %>% 
  summarise(deaths = sum(deaths)) -> frail_agg
young %>% 
  group_by(age, gender, ucd = underlying_cause_of_death_code, population) %>% 
  summarise(deaths = sum(deaths)) -> young_agg

data_all<-as.data.frame(rbind(young_agg,frail_agg))
options(digits=2)
options(scipen = 100)
data_all$crude<-data_all$deaths/data_all$population*100000

write.csv(data_all,"1999-2020_agg_top50_AgeGender.csv")

data_all$logMR<-log(data_all$crude)

mort_wide <- melt(data_all, id=c("age","gender","ucd"))
mort_long<-dcast(mort_wide, variable + age + ucd ~ gender)
mort_long$logMR_diff<-log(mort_long$Male/mort_long$Female)
diseases = read.csv("Diseasetypes.csv")

Plot<-merge(mort_long,diseases,by="ucd")

show_col(viridis_pal()(50))




#plotlabels<-c("Infectious/Parasitic diseases","Circulatory diseases","Metabolic diseases","Cancer",
      #        "Mental and behavioral disorders","Nervous system diseases","Respiratory diseases",
      #        "Digestive diseases","Genitourinary diseases","External causes")
#plotlabels<-as.data.frame(plotlabels)
#plotlabels$group_desc<-unique(Plot_nomid$group_desc)

Plot<-Plot[Plot$logMR_diff!="Inf" & Plot$logMR_diff!="-Inf",]
Plot<-Plot[Plot$ucd!="C50",]

Plot_crude<-Plot[Plot$variable=="crude",]
Plot_deaths<-Plot[Plot$variable=="deaths",]
Plot_deaths$total<-as.integer(Plot_deaths$Female+Plot_deaths$Male)
Plot_crude<-merge(Plot_crude,Plot_deaths[,c(1,3,11)],by=c("ucd","age"))
write.csv(Plot_crude,"Plot_crude.csv")
Plot_crude<-read.csv("Plot_crude.csv")

aggregate(data=Plot_crude,logMR_diff~group_desc,FUN=mean)->agg
Plot_crude$group_desc<-as.factor(Plot_crude$group_desc)
Plot_crude$group_desc<-factor(Plot_crude$group_desc, levels = c("Digestive system", 
    "Cancer, Lymphoid","Respiratory system","Sepsis",
    "Metabolic diseases","Circulatory system, ischaemic", "Circulatory system, other heart",
    "External causes (Falls)","Circulatory system, hypertensive","Cerebrovascular",
    "Dementias & Nervous system","Genitourinary system"))

Plot_crude %>% 
ggplot(aes(x = age, y = logMR_diff, group = ucd,label = labels, fill = logMR_diff))+
  geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  geom_line() +
  geom_point(aes(size=total,color="black",fill=logMR_diff),shape=21,alpha=.3) +
  scale_size(range=c(2,25),breaks = c(293, 50000, 2322323))+
  scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  scale_color_manual(values = alpha(c("black", "black", "black"),2))+
  geom_label_repel(box.padding = unit(0.1, "lines"),hjust=0.5,size=3, segment.size=0, segment.color=NA, nudge_x=-.3, direction="y")+
  xlab("Age") + 
  ylab("Sex Difference in ASMR, log (M/F)") +
  ggtitle("Top 50 U.S. causes of death over age 45 from 1999-2020")+
  facet_wrap(~group_desc)+
  scale_y_continuous(breaks=c(-.5, 0, 1.5), limits=c(-.7,1.8))+
  scale_x_discrete(labels=c("45-55" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(legend.key=element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1),
        plot.title = element_text(size = 20, hjust = .5),
        axis.title.x = element_text(),
        text = element_text(size=14,  family="Bookman"),
        panel.background = element_rect(fill = "white",color = "white",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",color = "white"), 
        panel.grid.minor = element_blank())+
  labs(size = "N deaths", scale_fill = "N deaths")+
  guides(color = F,
         scale_size = guide_legend(title="N deaths",direction = "vertical",show.limits=T),
         fill = F)+
  theme(legend.title.align=0.5) 
  


Plot %>%
  ggplot(aes(x = age, y = logMR_diff,group=desc,col=group_desc)) +
  geom_line(aes(line=desc)) +
  geom_point(size=3,) +
  scale_color_viridis(discrete=T)+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  #xlab("Age Group") + 
  ylab("Sex Difference in ASMR (log M - log F)") +
  ggtitle("Top 50 U.S. causes of death over age 45 in 2014")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~type)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  #theme(axis.text.x = element_blank())+
  theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(color=guide_legend(title="Cause of Death Category"))+
  scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

Plot_cancer %>%
  ggplot(aes(x = age_group, y = logMR_diff,group=desc,col=icd,label=Plot_cancer[,3])) +
  geom_line() +
  geom_point(size=3) +
  geom_text_repel(color="black")+
  #scale_color_viridis(option="A",discrete=T)+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  #xlab("Age Group") + 
  ylab("Sex Difference in ASMR (log M - log F)") +
  ggtitle("Top Cancer deaths over age 45 in 2014")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~desc)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.key=element_blank())+ 
  theme(legend.position="none")+
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = .5))+
  #theme(axis.text.x = element_blank())+
  theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  #guides(color=guide_legend(title="Cause of Death Category"))+
  scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=14,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())