library("tidyverse")
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)


data_acute = read_tsv("2014_CDC_Acute.txt")
data_chronic = read_tsv("2014_CDC_Chronic.txt")

#Question is whether Acute or Chronic have different mortality rate differences between males and females
acute<-data_acute[complete.cases(data_acute[,2]),]#dropping NAs
acute<-acute[,-c(1,2,5,7,10)]#removing empty columns
acute <- janitor::clean_names(acute)
acute[,4:5] <- sapply(acute[,4:5],as.numeric)
acute$mort<-acute$deaths/acute$population
acute$log_mort<-log10(acute$mort)
acute_log<-acute[,c(1:3,7)]

acute_deaths <- melt(acute_log, id.vars=1:3, variable.name = c("log_mort"))
acute_mort<-dcast(acute_deaths,age_group_code ~ gender + icd_sub_chapter)
acute_F<-acute_mort[,c(1:7)]
acute_M<-acute_mort[,c(1,8:13)]
acute_D<-acute_M[,2:7]-acute_F[,2:7]
acute_D$age_group_code<-acute_M$age_group_code
acute_F$ICD_type<-"acute"
acute_F$gender<-"female"
acute_F$group<-"both"
acute_M$ICD_type<-"acute"
acute_M$gender<-"male"
acute_M$group<-"both"
acute_D$ICD_type<-"acute"
acute_D$gender<-"diff"
acute_D$group<-"diff"
colnames(acute_F)[2:7] = stringr::str_sub(colnames(acute_F)[2:7], 7)
acute_F <- janitor::clean_names(acute_F)
colnames(acute_M)[2:7] = stringr::str_sub(colnames(acute_M)[2:7], 5)
acute_M <- janitor::clean_names(acute_M)
colnames(acute_D)[1:6] = stringr::str_sub(colnames(acute_D)[1:6], 5)
acute_D <- janitor::clean_names(acute_D)
acute_D<-acute_D[,c(7,1:6,8:10)]

acute_mort<-rbind(acute_F,acute_M,acute_D)

acute_long <- melt(acute_mort, id.vars = c("age_group_code", "icd_type","gender","group"),
                           measure.vars = c("influenza_and_pneumonia", "intentional_self_harm", "other_bacterial_diseases","other_external_causes_of_accidental_injury",
                                            "sequelae_of_external_causes_of_morbidity_and_mortality","transport_accidents"))

chronic<-data_chronic[complete.cases(data_chronic[,2]),]#dropping NAs
chronic<-chronic[,-c(1,2,5,7,10)]#removing empty columns
chronic <- janitor::clean_names(chronic)
chronic[,4:5] <- sapply(chronic[,4:5],as.numeric)
chronic$mort<-chronic$deaths/chronic$population
chronic$log_mort<-log10(chronic$mort)
chronic_log<-chronic[,c(1:3,7)]

chronic_deaths <- melt(chronic_log, id.vars=1:3, variable.name = c("log_mort"))
chronic_mort<-dcast(chronic_deaths,age_group_code ~ gender + icd_sub_chapter)
chronic_F<-chronic_mort[,c(1:6)]
chronic_M<-chronic_mort[,c(1,7:11)]
chronic_D<-chronic_M[,2:6]-chronic_F[,2:6]
chronic_D$age_group_code<-chronic_M$age_group_code
chronic_F$ICD_type<-"chronic"
chronic_F$gender<-"female"
chronic_F$group<-"both"
chronic_M$ICD_type<-"chronic"
chronic_M$gender<-"male"
chronic_M$group<-"both"
chronic_D$ICD_type<-"chronic"
chronic_D$gender<-"diff"
chronic_D$group<-"diff"
colnames(chronic_F)[2:6] = stringr::str_sub(colnames(chronic_F)[2:6], 7)
chronic_F <- janitor::clean_names(chronic_F)
colnames(chronic_M)[2:6] = stringr::str_sub(colnames(chronic_M)[2:6], 5)
chronic_M <- janitor::clean_names(chronic_M)
colnames(chronic_D)[1:5] = stringr::str_sub(colnames(chronic_D)[1:5], 5)
chronic_D <- janitor::clean_names(chronic_D)
chronic_D<-chronic_D[,c(6,1:5,7:9)]

chronic_mort<-rbind(chronic_F,chronic_M,chronic_D)

chronic_long <- melt(chronic_mort, id.vars = c("age_group_code", "icd_type","gender","group"),
                   measure.vars = c("chronic_lower_respiratory_diseases", "chronic_rheumatic_heart_diseases", "diabetes_mellitus","diseases_of_liver",
                                    "malignant_neoplasms"))

Mort_plot<-rbind(acute_long,chronic_long)
       
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

levels(Mort_plot$variable)

Mort_plot$variable <- factor(Mort_plot$variable, labels = c("Flu","Suicide","Bacterial","Ext Accidents","Septicemia","Car Accidents","Respiratory","Heart","Diabetes","Liver","Cancer"))

Mort_plot %>%
ggplot(aes(x = age_group_code, y = value, group = gender, col = gender)) +
  geom_line(size=1) +
  geom_point(size=2) +
  #stat_summary(fun = mean, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + ylab("Log mortality") +
  ggtitle("CDC 2014 Mortality Data")+
  facet_grid(~variable, scales = "free_y")+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))
   

#plot female,male, diff for all diseases on one plot
#for acute and one plot for chronic

Mort_plot %>%
  ggplot(aes(x = age_group_code, y = value, group = gender, col = gender)) +
  #geom_line(size=1,aes(group=gender)) +
  geom_point(size=2) +
  geom_smooth(method="loess",se=FALSE,fullrange=TRUE,
              aes(color=gender))+
  #stat_summary(group = group, fun = mean, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + ylab("Log mortality") +
  ggtitle("CDC 2014 Mortality Data")+
  facet_grid(~icd_type, scales = "free_y")+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))+
  theme(axis.title.x=element_text(margin=margin(15,0,0,0)))

###set working drive
setwd("C:/Users/renec/Desktop/Promislow/CDC")
data_all = read_tsv("2014_CDC.txt")

all<-data_all[complete.cases(data_all[,2]),]#dropping NAs
all<-all[all$Deaths<5,]#dropping deaths <5
all<-all[,-c(1,2,5,10)]#removing empty columns
all <- janitor::clean_names(all)
all[,5:6] <- sapply(all[,5:6],as.numeric)
all$mort<-all$deaths/all$population
all$log_mort<-log10(all$mort)
all_log<-all[,c(1:2,4,8)]

all_F<-all_log[all_log$gender=="Female",]
all_M<-all_log[all_log$gender=="Male",]
all<-merge(all_F,all_M,by=c("age_group_code","cause_of_death_code"),all=F)

all$diff<-all$log_mort.y-all$log_mort.x

all_F<-all[,c(1,2,4)]
all_F$gender<-"female"
all_M<-all[,c(1,2,6)]
all_M$gender<-"male"
all_D<-all[,c(1,2,7)]
all_D$gender<-"diff"
all_D<-rename(all_D, mort = diff)
all_F<-rename(all_F, mort = log_mort.x)
all_M<-rename(all_M, mort = log_mort.y)

all_F$group<-"both"
all_M$group<-"both"
all_D$group<-"diff"

all_mort<-rbind(all_F,all_M,all_D)

Tally<-all_mort %>% dplyr::count(cause_of_death_code,gender)
Tally<-Tally[Tally$n==3,]
keep<-Tally$cause_of_death_code
keep<-unique(keep)

all_mort<-all_mort[all_mort$cause_of_death_code %in% keep,]

keep100<-keep[145:164]
sub<-all_mort[all_mort$cause_of_death_code %in% keep100,]

sub %>%
  ggplot(aes(x = age_group_code, y = mort, group = gender, col = gender)) +
  geom_line(size=1) +
  geom_point(size=2) +
  #stat_summary(fun = mean, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + ylab("Log mortality") +
  ggtitle("CDC 2014 Mortality Data")+
  facet_wrap(~cause_of_death_code, scales = "free_y",nrow=6,ncol=6)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  theme(legend.position="none")

table(all_mort)

