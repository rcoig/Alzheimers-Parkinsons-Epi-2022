library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)


###set working drive
setwd("C:/Users/renec/Desktop/Promislow/CDC")
data_2019_full <- read.csv("mortality_2019_nber_us.csv")
data_2019_full <- janitor::clean_names(data_2019_full)

data_2019<-data_2019_full[,c(6,7,23,29:40)]

data_2019[data_2019 == ""] <- NA
#how many co-morbidities for each of several columns
data_2019$nMCD<- rowSums(!is.na(data_2019[, 4:15]))

#Plot age at death by # comorbidities on death certificate

#data_2019<-data_2019_full %>% 
 # group_by(age, gender, ucd = underlying_cause_of_death_code, population) %>% 
#  summarise(deaths = sum(deaths)) -> frail_agg

#all<-data_all[,-c(1,2,4,6,10)]#removing unnecessary columns
#all<-rename(all, mort = deaths)
#top_causes %>% slice_max(deaths, n = 179) -> top

#arrange(top, desc(deaths))->top
#top<-top$cause_of_death_code

#all_top <- all[all$cause_of_death_code %in% top,] 
#all_top$cause_of_death_code<-as.factor(all_top$cause_of_death_code)
#all_top$gender_code<-as.factor(all_top$gender_code)
#all_top$age_group_code<-as.factor(all_top$age_group_code)
#all causes of death with at least 1000 deaths in 2014

data_2019<-as.data.frame(data_2019)
data_2019$age<-as.numeric(data_2019$age)

data_older<-data_2019[data_2019$age < 1999,]
data_older$age <- substring(data_older$age, 2)
data_older$age<-as.numeric(data_older$age)

data_older %>% 
  group_by(age, sex) %>% 
  summarise(avgMCD = mean(nMCD), sdMCD = sd(nMCD)) -> MCD_agg

data_older$ucod<-substr(data_older$ucod, 1, 3) 

data_older %>% 
  filter (ucod == "G30")  %>% 
  group_by(age, sex) %>% 
  summarise(avgMCD = mean(nMCD), sdMCD = sd(nMCD)) -> MCD_agg_G30

data_older %>% 
  filter (ucod == "I21")  %>% 
  group_by(age, sex) %>% 
  summarise(avgMCD = mean(nMCD), sdMCD = sd(nMCD)) -> MCD_agg_I21

data_older %>%
  ggplot(aes(x = sex, y = nMCD)) +
  #geom_violin() +
  geom_boxplot() +
  geom_jitter()


MCD_agg %>%
  ggplot(aes(x = age, y = avgMCD,color=sex)) +
  #geom_violin() +
  #geom_boxplot()+
  geom_point(size=3) +
  geom_errorbar(aes(ymin=avgMCD-sdMCD, ymax=avgMCD+sdMCD), width=.2)+ 
  ggtitle("Multiple Causes of Death by Sex in 2019")+
  xlab("Age") +
  ylab("Mean Number of Additional Causes") +
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  #guides(color=guide_legend(title="Cause of Death Category"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())



MCD_agg_G30 %>%
  ggplot(aes(x = age, y = avgMCD,color=sex)) +
  #geom_violin() +
  #geom_boxplot()+
  geom_point(size=3) +
  geom_errorbar(aes(ymin=avgMCD-sdMCD, ymax=avgMCD+sdMCD), width=.2)+ 
  ggtitle("Multiple Causes of Death by Sex in 2019 for G30 deaths")+
  xlab("Age") +
  ylab("Mean Number of Additional Causes") +
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  #guides(color=guide_legend(title="Cause of Death Category"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

MCD_agg_I21 %>%
  ggplot(aes(x = age, y = avgMCD,color=sex)) +
  #geom_violin() +
  #geom_boxplot()+
  geom_point(size=3) +
  geom_errorbar(aes(ymin=avgMCD-sdMCD, ymax=avgMCD+sdMCD), width=.2)+ 
  ggtitle("Multiple Causes of Death by Sex in 2019 for I21 deaths")+
  xlab("Age") +
  ylab("Mean Number of Additional Causes") +
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  #guides(color=guide_legend(title="Cause of Death Category"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

fit<-glm(formula = nMCD ~ ucod, family = "poisson", data = data_older)
summary(fit)
