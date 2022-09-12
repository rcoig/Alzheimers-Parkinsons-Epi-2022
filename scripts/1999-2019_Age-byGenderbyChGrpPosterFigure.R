library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)

###load data
data_GA <- read_tsv("../data/1999-2019_Gender-AgeAllCauses.txt")
data <- janitor::clean_names(data_GA)

#remove notes from dfs
data<-as.data.frame(data[-c(15:64),])

data<-rename(data, age = ten_year_age_groups_code)
data<-rename(data, MR = crude_rate, MRSE = crude_rate_standard_error)

#log the Mortality rates for plotting
data$logMR<-log(data$MR)

sum(data_GA[1:14,7])

#reformat
mort_wide <- melt(data[,c(2,5,6,8,9,11)], id=c("gender","age"))
mort_long<-dcast(mort_wide, variable + age ~ gender)
mort_long$diff<-mort_long$Male-mort_long$Female

mort_wide$gender<-as.factor(mort_wide$gender)
mort_wide$gender <- factor(mort_wide$gender, levels = c("Male", "Female"))

#all-cause mortality by sex
mort_wide %>%
  filter (variable == "logMR") %>%
  ggplot(aes(x = age, y = value,group = gender,color = gender)) +
  geom_line(size=2) +
  scale_color_manual(values=c("#5D3A9B", "#E66100"))+
  #annotate("text", x=2, y=7.5, hjust = -.5, label="N = 4.2 billion", color = "black")+
  #geom_point(size=3,color="black",shape=21,alpha=.3) +
  #geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_viridis()+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("log Mortality Rate (per 100,000)") +
  ggtitle("All-Cause Mortality by Sex \n(1999-2019, United States)")+
  #facet_grid(frailty ~ type) +
  #facet_wrap(~type)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.position = c(0.8, 0.2))+
  guides(color=guide_legend(title=""))+ 
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 17, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  theme(axis.title.y = element_text(margin=margin(0,20,0,0)))+
  theme(axis.title.x = element_text(vjust=-2,margin=margin(0,0,20,0)))+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())
    

#sex difference in all-cause mortality
mort_long %>%
  filter (variable == "logMR") %>%
  ggplot(aes(x = age, y = diff,group=1,color=diff)) +
  geom_line(size=2) +
  scale_color_gradient(high = "#5D3A9B", low = "white")+
  annotate("text", x=1, y=.03, hjust = 0, label="No sex difference", color = "darkgray",size=5)+
  geom_point(size=3,alpha=.8) +
  geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_color_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_viridis()+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Sex Difference in log Mortality Rate \n log(Male/Female)") +
  ggtitle("Sex Difference in All-Cause Mortality \n(1999-2019, United States)")+
  #facet_grid(frailty ~ type) +
  #facet_wrap(~type)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.position = c(0.8, 0.2))+
  guides(color=F)+ 
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 17, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  theme(axis.title.y = element_text(margin=margin(0,10,0,0)))+
  theme(axis.title.x = element_text(vjust=-2,margin=margin(0,0,20,0)))+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

#sex difference in all-cause mortality
mort_long %>%
  filter (variable == "MR") %>%
  ggplot(aes(x = age, y = diff,group=1,color=diff)) +
  geom_line(size=2) +
  scale_color_gradient(high = "#5D3A9B", low = "gray")+
  annotate("text", x=4, y=70, hjust = 0, label="No sex difference", color = "darkgray",size=5)+
  geom_point(size=3,alpha=.8) +
  geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_color_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_viridis()+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Sex Difference in Mortality Rate \n Male - Female") +
  ggtitle("Sex Difference in All-Cause Mortality \n(1999-2019, United States)")+
  #facet_grid(frailty ~ type) +
  #facet_wrap(~type)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  #theme(legend.position = c(0.8, 0.2))+
  guides(color=F)+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 17, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  theme(axis.title.y = element_text(margin=margin(0,10,0,0)))+
  theme(axis.title.x = element_text(vjust=-2,margin=margin(0,0,20,0)))+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

mort_long$ch_grp<-"All Cause"
mort_long$subch_grp<-"All Cause"	
mort_long$cat_code<-"All Cause"	
mort_long$ucd_code<-"All Cause"
write.csv(mort_long,"mort_long_all_cause.csv")
mort_wide$ch_grp<-"All Cause"
mort_wide$subch_grp<-"All Cause"	
mort_wide$cat_code<-"All Cause"	
mort_wide$ucd_code<-"All Cause"
write.csv(mort_wide,"mort_wide_all_cause.csv")
