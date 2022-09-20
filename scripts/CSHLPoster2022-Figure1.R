library(tidyverse)
library(tidyr)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)
library(ggpubr)

#Figures for first 2 poster figures
theme_set(theme_pubr())

###load data for Fig1A and 1B
data_GA <- read_tsv("../data/1999-2019_Gender-AgeAllCauses.txt")
data <- janitor::clean_names(data_GA)

#remove notes from dfs
data<-as.data.frame(data[-c(15:64),])
data<-rename(data, age = ten_year_age_groups_code)
data<-rename(data, MR = crude_rate, MRSE = crude_rate_standard_error)

#log the Mortality rates for plotting
data$logMR<-log(data$MR)

sum(data_GA[1:14,7])#Total N is 4.2 billion

#reformat
mort_wide <- melt(data[,c(2,5,6,8,9,11)], id=c("gender","age"))
mort_long<-dcast(mort_wide, variable + age ~ gender)
mort_long$diff<-mort_long$Male-mort_long$Female

mort_wide$gender<-as.factor(mort_wide$gender)
mort_wide$gender <- factor(mort_wide$gender, levels = c("Male", "Female"))

rm(data,data_GA)

#QCing data for clustering
###load data
data_GACod <- read.csv("../data/1999-2019_Gender-Age-byCauseofDeathALL.csv")
pop<-read.csv("../data/1999-2019populationbyGenderand10-year.csv",header=T)
codes<-read.csv("../data/Hierarchy.csv",header=T)
codes$cat_code<-substr(codes$ucd_code,1,3)
hierarchy<-distinct(codes[,c(2,4,6,9)])
rm(codes)

data1 <- janitor::clean_names(data_GACod)
data1<-rename(data1, age = ten_year_age_groups_code)
data1<-rename(data1, ucd = underlying_cause_of_death)
data1<-rename(data1, ucd_code = underlying_cause_of_death_code)
data1<-rename(data1, CR = crude_rate)
data1 <- data1[!is.na(data1$population), ]#remove empty rows
data1$cat_code<-substr(data1$ucd_code,1,3)
data1<-merge(data1,hierarchy,by="cat_code")
length(unique(data1$cat_code))#1544 causes of death
rm(data_GACod)
deaths<-data1[,c(2,5,8,1,13,14,12,10)]
rm(data1)

#aggregate codes by ch_grp
data_agg_by_ch_grp<-deaths %>% 
  group_by(age, ch_grp) %>% 
  summarise(deaths = sum(deaths))
dcastcheck<-dcast(data_agg_by_ch_grp,ch_grp ~ age)
dcastcheck$mincheck<-apply(dcastcheck[,2:8], 1, FUN = min)

#min cannot be less than 500 (~25 deaths min per year per gender)
keep<-as.data.frame(dcastcheck[dcastcheck$mincheck>=500,])
keep <- keep[complete.cases(keep),]
reliablechgrps<-unique(keep$ch_grp)#15 total
rm(data_agg_by_ch_grp,dcastcheck)

#Process 15 codes as ch_grps
overlap1<-(deaths$ch_grp %in% reliablechgrps)
deaths$remove<-overlap1
ch_grps_keep<-deaths[deaths$remove != "FALSE",]
length(unique(ch_grps_keep$ch_grp))

rm(deaths,dcastcheck,overlap1,keep)

#should have 15 ch_grps
ch_grps<-ch_grps_keep %>% 
  group_by(age, gender,ch_grp) %>% 
  summarise(deaths = sum(deaths))
ch_grps<-dcast(ch_grps, gender + ch_grp ~ age)
ch_grps<-merge(ch_grps,ch_grps_keep[,c(4,7)],by="ch_grp")
ch_grps$cat_code<-NA
ch_grps<-distinct(ch_grps)
ch_grps$type<-"ch_grp"
rm(ch_grps_keep)
ch_grps$code<-ch_grps$ch_grp
ch_grps<-ch_grps[,c(2:9,10,1,12,11)]

FINALdata<-ch_grps

############################################
#FORMAT FOR CENTERING BY DIFF

hierarchy<-distinct(FINALdata[,c(9:12)])
FINALdata<-FINALdata[,c(1:8,11)]
melt<-melt(FINALdata, id=c("code","gender"))
melt<-rename(melt, age = variable)
melt<-rename(melt, deaths = value)
melt<-merge(melt,pop,by=c("gender","age"))

melt$MR<-melt$deaths/melt$population*100000
melt$logMR<-log(melt$MR)

logMR_by_gender<-dcast(melt[,c(1,2,3,7)], code + age ~ gender)
logMR_by_gender$diff<-logMR_by_gender$Male-logMR_by_gender$Female
logMR_by_gender$code<-droplevels(as.factor(logMR_by_gender$code))
logMR_by_gender$labels<-logMR_by_gender$ch_grp
logMR_by_gender<-logMR_by_gender[logMR_by_gender$code!="L00-L98",]
logMR_by_gender<-logMR_by_gender[logMR_by_gender$code!="M00-M99",]
logMR_by_gender<-logMR_by_gender[logMR_by_gender$code!="Q00-Q99",]
logMR_by_gender<-logMR_by_gender[logMR_by_gender$code!="A00-B99",]
logMR_by_gender<-logMR_by_gender[logMR_by_gender$code!="D50-D89",]

logMR_by_gender$code<-droplevels(logMR_by_gender$code)
levels(logMR_by_gender$code)

logMR_by_gender$code <- factor(logMR_by_gender$code,
        levels=c("I00-I99","C00-D48",
         "J00-J98","V01-Y89","G00-G98",
         "E00-E88","F01-F99","K00-K92", "N00-N98", "R00-R99"))

logMR_by_gender$labels <- as.factor(logMR_by_gender$code)
levels(logMR_by_gender$labels) <- c("I00-I99  (33.9%)","C00-D48  (23.3%)",
                 "J00-J98   (9.8%)","V01-Y89  (6.6%)","G00-G98   (5.9%)",
                 "E00-E88   (4.3%)","F01-F99   (4.2%)","K00-K92   (3.8%)",
                 "A00-B99   (2.6%)","N00-N98   (2.5%)","R00-R99   (1.2%)")




#logMRdiff_by_age<-dcast(logMR_by_gender[,c(1,2,5)], code ~ age)
#logMRdiff_by_age$code->row.names(logMRdiff_by_age)
#logMRdiff_by_age$code<-NULL

#logMRdiff_by_cat<-as.data.frame(t(logMRdiff_by_age))
#logMRdiff_by_cat_centered<-as.data.frame(scale(logMRdiff_by_cat,center=T,scale=F))

#write.csv(logMRdiff_by_cat_centered,"logMRdiff_by_cat_centered.csv")

#logMRdiff_by_age_centered<-as.data.frame(t(logMRdiff_by_cat_centered))
###########################
###########################################POSTER FIGURES
############################

#Fig1A all-cause mortality by sex
A<-mort_wide %>%
  filter (variable == "logMR") %>%
  ggplot(aes(x = age, y = value,group = gender,color = gender)) +
  geom_line(size=2) +
  scale_color_manual(values=c("#5D3A9B", "#E66100"))+
  xlab("Age Group") + 
  ylab("log Mortality Rate (per 100,000)") +
  ggtitle("All-Cause Mortality by Sex")+
  theme(legend.position = c(0.8, 0.2))+
  guides(color=guide_legend(title=""))+ 
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.margin = margin(30,30,30,30)) + #top, right, bottom, left
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


#Fig 1B sex difference in all-cause mortality
B<-mort_long %>%
  filter (variable == "logMR") %>%
  ggplot(aes(x = age, y = diff,group=1,color=diff)) +
  geom_line(size=2) +
  scale_color_gradient(high = "#5D3A9B", low = "E66100")+
  annotate("text", x=1, y=.03, hjust = 0, label="No sex difference", color = "darkgray",size=5)+
  geom_point(size=3,alpha=.8) +
  geom_hline(yintercept=0,color="gray",alpha=0.3,linetype=2,size=1)+
  xlab("Age Group") + 
  ylab("Sex Difference in log Mortality Rate \n log(Male/Female)") +
  ggtitle("Sex Difference in All-Cause Mortality")+
  theme(legend.position = c(0.8, 0.2))+
  guides(color=F)+ 
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(plot.margin = margin(30,30,30,30)) + #top, right, bottom, left
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


mytheme <- theme_minimal() + theme(
  panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),
  strip.background = element_rect(color = "black", size = 1),
  axis.text.x = element_text(angle = -45, vjust = 0.2, hjust=0.5),
  #axis.ticks = element_blank(),
  #axis.title = element_blank(),
  #legend.key=element_blank(),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"),
  panel.grid.minor = element_blank(),
  panel.spacing = unit(0, "lines"),
  panel.border = element_rect(color = "black", fill = NA, size = .5), 
  plot.title = element_text(hjust = 0.5),
  #strip.text = element_text(size = rel(3.0), vjust = -4.0),
  text=element_text(size=16)
)

#sex difference in mortality by ICD-Chapter
logMR_by_gender %>% 
  ggplot(aes(x = age, y = diff, group = code)) +
  geom_hline(yintercept=0,color="gray",alpha=0.9,linetype=2,size=1)+
  geom_line(size=3) +
  geom_point(size=3,alpha=.8) +
  xlab("Age Group") + 
  ylab("Sex Difference in Mortality Rate \nlog (Male/Female)") +
  ggtitle("Sex Difference in Mortality by ICD Chapter")+
  facet_wrap(~labels,nrow=2)+
  #ylim(-1,4)+
  guides(fill=F,color=F)+
  mytheme


figure <- ggarrange(A ,B)

png("C:/Users/renec/Desktop/PhD/My Posters and Talks/CSHL/Fig1.png", 2800, 3300)
figure
dev.off()
file.show("C:/Users/renec/Desktop/PhD/My Posters and Talks/CSHL/Fig1.png")
