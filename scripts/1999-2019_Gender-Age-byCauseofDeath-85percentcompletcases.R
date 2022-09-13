library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)
#QCing data for clustering

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

#group and scale data by cause of death category for clustering

###load data
data_GACod <- read.csv("../data/1999-2019_Gender-Age-byCauseofDeath.csv")
pop<-read.csv("../data/1999-2019populationbyGenderand10-year.csv",header=T)


data1 <- janitor::clean_names(data_GACod)
data1<-as.data.frame(data1[-c(32683:32955),])#remove notes from dfs
data1<-rename(data1, age = ten_year_age_groups_code)
data1<-rename(data1, ucd = underlying_cause_of_death)
data1<-rename(data1, ucd_code = underlying_cause_of_death_code)
data1<-rename(data1, CR = crude_rate)
data1$cat_code<-substr(data1$ucd_code,1,3)

#aggregate deaths by cat_code
data_agg<-data1 %>% 
  group_by(age, gender,cat_code) %>% 
  summarise(deaths = sum(deaths), population = max(population))

length(unique(data_agg$cat_code))

#Estimates with a RSE of < 30% meet the standard of reliability and is displayed. 
#Estimates with a RSE of 30%-50% meet a lower standard of reliability and is displayed 
#but should be interpreted with caution. Estimates with a RSE of > 50% are statistically 
#unreliable and not displayed.
data_agg$MR<-data_agg$deaths/data_agg$population*100000
data_agg$MRSE<-data_agg$MR/(sqrt(data_agg$deaths))
data_agg$RSE<-data_agg$MRSE/data_agg$MR*100

data_agg$RSE<-round(data_agg$RSE, 3)

#cleaning data, remove any codes that are RSE < 34%
data_keep<-data_agg[data_agg$RSE < 34, ]

#construct data frame
data<-melt(data_keep[,1:4], id=c("gender","age","cat_code"))
data_by_age<-dcast(data, variable + cat_code + gender ~ age)
length(unique(data_by_age$cat_code))#803 codes

## removing rows with more than 85% missing data
NA75list<-data_by_age[which(rowMeans(!is.na(data_by_age)) < 0.85),]
NA75list<-unique(NA75list$cat_code)#remove 284 codes
data_keep <- data_by_age[!data_by_age$cat_code %in% NA75list,]
length(unique(data_keep$cat_code))#577 codes
data_by_age<-data_keep
table(data_by_age$cat_code)

#onlykeep complete cases
data_by_age_filtered <- data_by_age[complete.cases(data_by_age), ]#524 

#remove mean deaths <100
data_by_age_filtered$mean<-rowMeans(data_by_age_filtered[,c(4:10)])
lessthan100list<-data_by_age_filtered[which(rowMeans(data_by_age_filtered[,c(4:10)]) < 100),]
lessthan100list<-unique(lessthan100list$cat_code)#remove 103 codes
data_by_age_filtered <- data_by_age_filtered[!data_by_age_filtered$cat_code %in% lessthan100list,]
length(unique(data_keep$cat_code))#551 codes

#remove codes that are not represented by both groups
data_by_age_filtered %>%
  group_by(cat_code) %>%
  filter(n() != 1) %>%
  ungroup -> data_by_age_filtered
table(data_by_age_filtered$cat_code)
length(unique(data_by_age_filtered$cat_code))#356 codes




data_wide<-melt(data_by_age_filtered[,2:10], id=c("gender","cat_code"))
data_wide<-rename(data_wide, age = variable)
data_wide<-rename(data_wide, deaths = value)

data_wide<-merge(data_wide,pop,by=c("gender","age"))
data_wide$MR<-data_wide$deaths/data_wide$population*100000
data_wide$MRSE<-data_wide$MR/(sqrt(data_wide$deaths))
data_wide$RSE<-data_wide$MRSE/data_wide$MR*100
data_wide$logMR<-log(data_wide$MR)

logMR_by_gender<-dcast(data_wide[,c(1,2,3,9)], cat_code + age ~ gender)
logMR_by_gender$diff<-logMR_by_gender$Male-logMR_by_gender$Female

logMRdiff_by_age<-dcast(logMR_by_gender[,c(1,2,5)], cat_code ~ age)
logMRdiff_by_age[is.na(logMRdiff_by_age)] = 0

logMRdiff_by_age$cat_code->row.names(logMRdiff_by_age)
logMRdiff_by_age$cat_code<-NULL
logMRdiff_by_cat<-as.data.frame(t(logMRdiff_by_age))

logMRdiff_by_cat_scaled<-as.data.frame(scale(logMRdiff_by_cat),center=T)
logMRdiff_by_age_scaled<-as.data.frame(t(logMRdiff_by_cat_scaled))

write.csv(logMRdiff_by_cat_scaled,"logMRdiff_by_cat_scaled.csv")
write.csv(logMRdiff_by_age_scaled,"logMRdiff_by_age_scaled.csv")

logMRdiff_by_age_scaled$cat_code<-row.names(logMRdiff_by_age_scaled)
logMRdiff_scaled_melt<-melt(logMRdiff_by_age_scaled, id=c("cat_code"))
logMRdiff_scaled_melt<-rename(logMRdiff_scaled_melt, age = variable)
logMRdiff_scaled_melt<-rename(logMRdiff_scaled_melt, diff = value)

###########################

codes <- read.csv("../data/codes.csv",header=T)
allcause<-read.csv("../data/mort_long_all_cause.csv",header=T)
ch_grp<-read.csv("../data/mort_long_ch_grp.csv",header=T)

###################fill each df NAs with zeros before center and scale
allcause_wide<-melt(allcause[,c(1,4:5)], id=c("age","ch_grp"))
allcause_wide<-dcast(allcause_wide, variable + age ~ ch_grp)
row.names(allcause_wide)<-allcause_wide$age
allcause_wide$variable<-NULL
allcause_wide$age<-NULL
write.csv(allcause_wide,"mort_wide_allcause.csv")
allcause_wide_scale<-as.data.frame(scale(allcause_wide))


ch_grp_wide<-melt(ch_grp[,c(1,4:5)], id=c("age","ch_grp"))
ch_grp_wide<-dcast(ch_grp_wide, variable + age ~ ch_grp)
row.names(ch_grp_wide)<-ch_grp_wide$age
ch_grp_wide$variable<-NULL
ch_grp_wide$age<-NULL
write.csv(ch_grp_wide,"mort_wide_ch_grp.csv")
ch_grp_wide_scale<-as.data.frame(scale(ch_grp_wide))


mort_wide_scaled<-cbind(allcause_wide_scale,ch_grp_wide_scale,logMRdiff_by_cat_scaled)
#mort_wide_noNaN<-mort_wide_scale[, colSums(is.na(mort_wide_scale)) != nrow(mort_wide_scale)]
write.csv(mort_wide_scaled,"mort_wide_scaled.csv")

mort_wide_scaled$age<-row.names(mort_wide_scaled)
mort_long_plot<-melt(mort_wide_scaled, id=c("age"))
mort_long_plot<-rename(mort_long_plot, diff = value)
mort_long_plot<-rename(mort_long_plot, group = variable)
mort_long_plot$type<-rep(c("all","ch_grp","cat_code"), times = c(7,77,2492))
write.csv(mort_long_plot,"mort_long_plot.csv")

###load codes
codes<-read.csv("../data/codes.csv",header=T)
codes<-codes[,c(1,5)]

mort_long_plot$cat_code<-mort_long_plot$group
mort_long_notcat<-mort_long_plot[1:84,]
mort_long_cat<-mort_long_plot[85:4284,]

mort_long_cat<-merge(mort_long_plot,codes,by="cat_code",ALL=T)

mort_long_notcat<-rename(mort_long_notcat, ch_grp = cat_code)
mort_long_notcat$cat_code<-NA
mort_long_notcat<-mort_long_notcat[,c(6,1:5)]
mort_long<-rbind(mort_long_notcat,mort_long_cat)

mort_long$group<-as.factor(mort_long$group)
table(mort_long$type)#600 causes of death,1 all cause, 11 chapter groups
levels(mort_long$group)
mort_long$ch_grp<-as.factor(mort_long$ch_grp)
mort_long<-as.data.frame(mort_long)

mort_long$ch_grp <- factor(mort_long$ch_grp, levels = c("All Cause",
                                                        "I00-I99","C00-D49",
                                                        "J00-J99","V00-Y99","G00-G99",
                                                        "E00-E89","F01-F99","K00-K95",
                                                        "A00-B99","N00-N99","R00-R99"))
mort_long$labels<-mort_long$ch_grp
levels(mort_long$labels) <- c("All-Cause","I00-I99 33.9%","C00-D48 23.3%",
                              "J00-J98 9.8%","V01-Y89 6.6%","G00-G98 5.9%",
                              "E00-E88 4.3%","F01-F99 4.2%","K00-K92 3.8%",
                              "A00-B99 2.6%","N00-N98 2.5%","R00-R99 1.2%")
mort_long$type<-as.factor(mort_long$type)
mort_long$type <- factor(mort_long$type, levels = c("all","ch_grp","cat_code"))

#sex difference in all-cause mortality
mort_long %>%
  filter (type != "cat_code") %>%
  ggplot(aes(x = age, y = diff,group=group,color=type)) +
  geom_line(aes(size=type)) +
  scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  scale_color_manual(values = c(all = "purple", cat_code = "red", ch_grp = "green"))+
  #geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_continuous(breaks = c(1999,2019),
  #labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Scaled log (Male/Female)") +
  ggtitle("Shapes of Sex Differences in Mortality")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~labels,nrow=2)+
  ylim(-2,2)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(fill=F,color=F,size=F)+
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
  filter (type != "ch_grp") %>%
  ggplot(aes(x = age, y = diff,group=group)) +
  geom_line(aes(size=type)) +
  scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  #scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  #geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_continuous(breaks = c(1999,2019),
  #labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Scaled log (Male/Female)") +
  ggtitle("Shapes of Sex Differences in Mortality")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~labels,nrow=2)+
  ylim(-2,2)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(fill=F,color=F,size=F)+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

#clustering on sex diffs this time...
mort_long_cat<-mort_long[mort_long$type=="cat_code",]

#clustering
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/factoextra")
#install.packages("cluster")
#install.packages("dendextend")
#install.packages("corrplot")
#install.packages("tidyverse")
#install.packages("NbClust")
library(factoextra)    #clustering visualization
library(cluster)       #clustering algorithms
library(dendextend)    #for comparing two dendrograms
library(corrplot)      #correlation between dendrograms
library(tidyverse)     #data manupulation
library(NbClust)       #determine optimal no. of clusters


list<-unique(mort_long_cat$group)
list<-droplevels(list)

CoD<-logMRdiff_by_age_scaled[,-8]

#hc_d <- diana(CoD)
#hc_d$dc

hc_a <- agnes(CoD,method="complete")

#pltree(hc_d, cex = 0.6, hang = -1, main = "Dendrogram of diana")
pltree(hc_a, cex = 0.6, hang = -1, main = "Dendrogram of agnes")



set.seed(31)
# function to compute total within-cluster sum of squares
gap_stat <- clusGap(CoD, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

#cutree(as.hclust(hc_d), k=8)->clusters
cutree(as.hclust(hc_a), k=9)->clusters

fviz_cluster(list(data = CoD, cluster = clusters))
#CoD$clusters<- cutree(as.hclust(hc_d), k=8)
CoD$clusters<- cutree(as.hclust(hc_a), k=9)

#return to plotting the SD using cluster groupings
CoD$cat_code<-row.names(CoD)
clust<-as.data.frame(CoD[,8:9])

mort_long_cat$group<-NULL
mort_long_cat<-merge(mort_long_cat,clust,by="cat_code")

mort_long_allcause<-mort_long[mort_long$type=="all",]
mort_long_allcause$clusters<-0
mort_long_allcause$cat_code<-"none"
mort_long_allcause$group<-NULL
mort_long_allcause<-mort_long_allcause[,c(2,1,3:7)]

mort_long_combined<-rbind(mort_long_allcause,mort_long_cat)

#sex difference by cluster
mort_long_combined %>%
  filter (type != "ch_grp") %>%
  ggplot(aes(x = age, y = diff,group=cat_code)) +
  geom_line(aes(size=type)) +
  scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  #geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_continuous(breaks = c(1999,2019),
  #labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Scaled log (Male/Female)") +
  ggtitle("Clusters by Cause of Death")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~clusters,nrow=4)+
  #ylim(-2,2)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(fill=F,color=F,size=F)+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

codes<-read.csv("../data/codes.csv",header=T)
codes<-codes[,5:6]
mort_long_combined<-merge(mort_long_combined,codes,by="cat_code",all=F)
mort_long_combined<-distinct(mort_long_combined)


mort_long_combined[mort_long_combined$clusters==1,]->Cluster1
Cluster1<-distinct(Cluster1[,c(1,7,8)])#8


unscaled_combined<-merge(logMR_by_gender,mort_long_combined[,-c(3)],by=c("cat_code","age"))

write.csv(mort_long_combined,"ClusteredScaledDiff.csv")
write.csv(Cluster1,"Cluster1.csv")


#UNSCALED sex difference by cluster
unscaled_combined %>%
  #filter (clusters == 5) %>%
  filter (ch_grp == "A00-B99") %>%
  
  ggplot(aes(x = age, y = diff,group=1)) +
  geom_line() +
  scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  #geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_continuous(breaks = c(1999,2019),
  #labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("log (Male/Female)") +
  ggtitle("Cluster 5")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~cat_code,nrow=4)+
  #ylim(-2,2)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(fill=F,color=F,size=F)+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())



