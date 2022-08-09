library("tidyverse")
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)

data_all = read_tsv("2013_CDC.txt")


data_acute = read_tsv("2013_CDC_Acute.txt")
data_chronic = read_tsv("2013_CDC_Chronic.txt")

#Question is whether Acute or Chronic have different mortality rate differences between males and females
acute<-data_acute[complete.cases(data_acute[,1]),]#dropping NAs
acute<-acute[complete.cases(acute[,6]),]#dropping NAs
acute<-acute[,-c(1,2,5,7:9)]#removing empty columns
acute <- janitor::clean_names(acute)
acute[,4:9] <- sapply(acute[,4:9],as.numeric)

acute %>% 
  group_by(age_group_code,gender) %>% 
  summarise(acute_deaths = sum(deaths)) -> acute_deaths
acute_deaths <- melt(acute_deaths, id.vars=1:2, variable.name = c("acute_deaths"))
acute_deaths<-dcast(acute_deaths,age_group_code ~ gender + acute_deaths)

acute %>% 
  group_by(gender, age_group_code) %>% 
  summarise(acute_pop = sum(population)) -> acute_pop
acute_pop <- melt(acute_pop, id.vars=1:2, variable.name = c("acute_pop"))
acute_pop<-dcast(acute_pop,age_group_code ~ gender + acute_pop)

acute_mort<-merge(acute_deaths,acute_pop,by="age_group_code")
acute_mort$F_acute_mort<-acute_mort$Female_acute_deaths/acute_mort$Female_acute_pop
acute_mort$F_acute_mort<-log10(acute_mort$F_acute_mort)
acute_mort$M_acute_mort<-acute_mort$Male_acute_deaths/acute_mort$Male_acute_pop
acute_mort$M_acute_mort<-log10(acute_mort$M_acute_mort)
acute_mort$ICD<-"acute"



chronic<-data_chronic[complete.cases(data_chronic[,1]),]#dropping NAs
chronic<-chronic[complete.cases(chronic[,6]),]#dropping NAs
chronic<-chronic[,-c(1,2,5,7:9)]#removing empty columns
chronic <- janitor::clean_names(chronic)
chronic[,4:9] <- sapply(chronic[,4:9],as.numeric)

chronic %>% 
  group_by(age_group_code,gender) %>% 
  summarise(chronic_deaths = sum(deaths)) -> chronic_deaths
chronic_deaths <- melt(chronic_deaths, id.vars=1:2, variable.name = c("chronic_deaths"))
chronic_deaths<-dcast(chronic_deaths,age_group_code ~ gender + chronic_deaths)

chronic %>% 
  group_by(gender, age_group_code) %>% 
  summarise(chronic_pop = sum(population)) -> chronic_pop
chronic_pop <- melt(chronic_pop, id.vars=1:2, variable.name = c("chronic_pop"))
chronic_pop<-dcast(chronic_pop,age_group_code ~ gender + chronic_pop)

chronic_mort<-merge(chronic_deaths,chronic_pop,by="age_group_code")
chronic_mort$F_chronic_mort<-chronic_mort$Female_chronic_deaths/chronic_mort$Female_chronic_pop
chronic_mort$F_chronic_mort<-log10(chronic_mort$F_chronic_mort)
chronic_mort$M_chronic_mort<-chronic_mort$Male_chronic_deaths/chronic_mort$Male_chronic_pop
chronic_mort$M_chronic_mort<-log10(chronic_mort$M_chronic_mort)
chronic_mort$ICD<-"chronic"

acute_mort<-acute_mort[,-c(2:5)]
chronic_mort<-chronic_mort[,-c(2:5)]
acute_mort<-dplyr::rename(acute_mort, F_mortality = F_acute_mort)
acute_mort<-dplyr::rename(acute_mort, M_mortality = M_acute_mort)
chronic_mort<-dplyr::rename(chronic_mort, F_mortality = F_chronic_mort)
chronic_mort<-dplyr::rename(chronic_mort, M_mortality = M_chronic_mort)


Mort<-rbind(acute_mort,chronic_mort)
Mort$Diff<-Mort$M_mortality-Mort$F_mortality
MortF<-Mort[,c(1,2,4)]
MortF<-dplyr::rename(MortF, Mortality = F_mortality)
MortF$gender<-"Female"
MortM<-Mort[,c(1,3,4)]
MortM<-dplyr::rename(MortM, Mortality = M_mortality)
MortM$gender<-"Male"
MortD<-Mort[,c(1,4,5)]
MortD<-dplyr::rename(MortD, Mortality = Diff)
MortD$gender<-"Diff"
#Diff should be log male mortality - log female mortality

Mort_plot<-rbind(MortF,MortM,MortD)
#Mort_plot$log<-log(Mort_plot$Mortality)
#plot(log(Crude_Mortality)~age_group_code+gender,data=Mort_plot)

Mort_plot$gender<-as.character(Mort_plot$gender)
Mort_plot$gender < - factor(Mort_plot$gender, levels=c("Female", "Male", "Diff"))


levels(Mort_plot$gender)
       
devtools::install_github("zeehio/facetscales")
library(g)
library(facetscales)
scales_y <- list(
  `Diff` = scale_y_continuous(limits = c(0, 0.5)),
  `Female` = scale_y_continuous(limits = c(0-4,-1.5)),
  `Male` = scale_y_continuous(limits = c(-4,-1.5))
)
Mort_plot$group <- factor(Mort_plot$gender, levels=c("Female", "Male", "Diff"))
Mort_plot$group<- rep(c("Both","Diff"),times=c(12,6))

Mort_plot %>%
ggplot(aes(x = age_group_code, y = Mortality, group = gender, col = ICD)) +
  #geom_line(size=1) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + ylab("Log mortality") +
  ggtitle("CDC 2013 Mortality Data")+
  facet_grid(~group, scales = "free_y")+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(plot.title = element_text(size = 20, hjust = 0.5))   

#plot female,male, diff for all diseases on one plot
#for acute and one plot for 