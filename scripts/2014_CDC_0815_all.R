library("tidyverse")
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)


###set working drive
setwd("C:/Users/renec/Desktop/Promislow/CDC")
data_all = read_tsv("2014_CDC.txt")
data_all <- janitor::clean_names(data_all)
data_all$cause_of_death_code <- substr(data_all$cause_of_death_code, 0, 3)

#all<-data_all[complete.cases(data_all[,2]),]#dropping NAs
#all<-all[all$Deaths<5,]#dropping deaths <5
all<-data_all[,-c(1,2,4,6,10)]#removing unnecessary columns
#all<-rename(all, mort = deaths)

aggregate(deaths~cause_of_death_code, all, FUN=sum) -> top_causes

top_causes %>% slice_max(deaths, n = 179) -> top

arrange(top, desc(deaths))->top
top<-top$cause_of_death_code

all_top <- all[all$cause_of_death_code %in% top,] 
all_top$cause_of_death_code<-as.factor(all_top$cause_of_death_code)
all_top$gender_code<-as.factor(all_top$gender_code)
all_top$age_group_code<-as.factor(all_top$age_group_code)
#all causes of death with at least 1000 deaths in 2014

all_agg<-aggregate(deaths~age_group_code+gender_code+cause_of_death_code, all_top, FUN=sum)
pop_agg<-aggregate(population~age_group_code+gender_code, all_top, FUN=sum)

all_agg<-merge(all_agg,pop_agg,by=c("age_group_code","gender_code"))
write.csv(all_agg,"2014_agg_over1000_AgeGender.csv")
table(all_top)

#clean and reload to environment
data = read.csv("2014_agg_over1000_AgeGender.csv")
diseases = read.csv("Diseasecats.csv")


data[,1:3] <- lapply(data[,1:3],as.factor)
data[,4.5] <- lapply(data[,4:5],as.numeric)

data$MR<-data$deaths/data$population
warnings()
data$logMR<-log(data$MR)

mort<-data[,c(1:3,7)]

mort_wide <- melt(mort, id=c("age_group","gender","icd"))
mort_F<-mort_wide[mort_wide$gender=="F",]
mort_F<-rename(mort_F, logMR_F = value)
mort_F<-mort_F[,c(1,3,5)]
mort_M<-mort_wide[mort_wide$gender=="M",]
mort_M<-rename(mort_M, logMR_M = value)
mort_M<-mort_M[,c(1,3,5)]

Mort_plot<-merge(mort_F,mort_M,by=c("age_group","icd"))
Mort_plot$logMR_diff<-Mort_plot$logMR_M-Mort_plot$logMR_F
Mort_plot$icd
Plot<-merge(Mort_plot,diseases,by="icd")

#removing sex-specific cancers (ovarian,prostate) and breast because it's so different
Plot<-Plot[Plot$icd!="C50",]
Plot_nomid<-Plot[Plot$age_group!="55-64",]
Plot_nomid<-Plot_nomid[Plot_nomid$age_group!="65-74",]
Plot_nomid<-Plot_nomid[Plot_nomid$age_group!="75-84",]
Plot_nomid$age_group<-droplevels(Plot_nomid$age_group)

library(viridis)
library(scales)
show_col(viridis_pal()(50))

write.csv(Plot,"Top50_Diffs.csv")

Plot_nomid %>%
  ggplot(aes(x = age_group, y = logMR_diff,group=desc,col=group_desc)) +
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

Plot_nomid %>%
  ggplot(aes(x = age_group, y = logMR_diff,group=desc,col=group_desc)) +
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
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = -1.5))+
  #theme(axis.text.x = element_blank())+
  theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(color=guide_legend(title="Cause of Death Category"))+
  scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=14,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

plotlabels<-c("Infectious/Parasitic diseases","Circulatory diseases","Metabolic diseases","Cancer",
              "Mental and behavioral disorders","Nervous system diseases","Respiratory diseases",
              "Digestive diseases","Genitourinary diseases","External causes")
plotlabels<-as.data.frame(plotlabels)
plotlabels$group_desc<-unique(Plot_nomid$group_desc)

Plot_nomid<-merge(Plot_nomid,plotlabels,by="group_desc")
Plot_nomid<-Plot_nomid[Plot_nomid$icd!="Q90",]
Plot_nomid<-Plot_nomid[Plot_nomid$icd!="C50",]
Plot_nomid<-Plot_nomid[Plot_nomid$icd!="X70",]
Plot_nomid<-Plot_nomid[Plot_nomid$icd!="X72",]
Plot_nomid<-Plot_nomid[Plot_nomid$icd!="X73",]
Plot_nomid<-Plot_nomid[Plot_nomid$icd!="X74",]

Plot_cancer<-Plot_nomid[Plot_nomid$group_desc=="Cancer",]
library(ggrepel)
Plot_nomid %>%
  ggplot(aes(x = age_group, y = logMR_diff,group=icd,col=icd,label=Plot_nomid[,2])) +
  geom_line() +
  geom_point(size=3) +
  geom_text_repel(color="black",label.size=.1)+
  #scale_color_viridis(option="A",discrete=T)+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  #xlab("Age Group") + 
  ylab("Sex Difference in ASMR (log M - log F)") +
  ggtitle("U.S. causes of death over age 45 in 2014")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~group_desc)+
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

#add standard error to plots

