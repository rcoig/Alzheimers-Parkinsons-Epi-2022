library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)
#QCing data for clustering
###load data
data_GACod <- read.csv("../data/1999-2019_Gender-Age-byCauseofDeathALL.csv")
pop<-read.csv("../data/1999-2019populationbyGenderand10-year.csv",header=T)
codes<-read.csv("../data/Hierarchy.csv",header=T)
codes$cat_code<-substr(codes$ucd_code,1,3)
hierarchy<-distinct(codes[,c(2,4,6,9)])
#rm(codes)

data1 <- janitor::clean_names(data_GACod)
data1<-rename(data1, age = ten_year_age_groups_code)
data1<-rename(data1, ucd = underlying_cause_of_death)
data1<-rename(data1, ucd_code = underlying_cause_of_death_code)
data1<-rename(data1, CR = crude_rate)
data1 <- data1[!is.na(data1$population), ]#remove empty rows
data1$cat_code<-substr(data1$ucd_code,1,3)
data1<-merge(data1,hierarchy,by="cat_code")
length(unique(data1$cat_code))#1544 causes of death
#rm(data_GACod)
deaths<-data1[,c(2,5,8,1,13,14,12,10)]
rm(data1)
cat_code_list<-unique(deaths$cat_code)#1544 codes

deaths_by_age_gender<-deaths %>% 
  group_by(age, gender) %>% 
  summarise(deaths = sum(deaths))

deaths_by_age_gender$pop <- ifelse (deaths_by_age_gender$gender == "Female", 433305919,
                      ifelse(deaths_by_age_gender$gender == "Male", 440713904, "no"))

deaths_by_age_gender$deaths<-as.numeric(as.character(deaths_by_age_gender$deaths))
deaths_by_age_gender$pop<-as.numeric(as.character(deaths_by_age_gender$pop))


deaths_by_age_gender$deathsacc<-deaths_by_age_gender$pop-deaths_by_age_gender$deaths

deaths_by_age_gender %>%
  ggplot( aes(x=age, y=deaths, group=gender, color=gender)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Gender Difference in number of deaths by age") +
  ylab("Number of deaths")->p

require(scales)
p + scale_y_continuous(labels=comma)


#aggregate all codes by cat_code
agg<-deaths %>% 
  group_by(age, gender,cat_code) %>% 
  summarise(deaths = sum(deaths))

agg_wide<-melt(agg[,c(1:4)], id=c("age","cat_code","gender"))
aggcast<-dcast(agg_wide, cat_code + gender ~ age)
rm(agg_wide)
#replace NAs here with zeros for there are zero reported deaths
aggcast[is.na(aggcast)] <- 0
aggcast$mincheck<-apply(aggcast[,3:9], 1, FUN = min)
#write.csv(aggcast,"cat_code_Deaths.csv")
Deathsbyage<-aggcast[,3:9]
Deathsbyage<-as.matrix(Deathsbyage)

#merge pop data again to recalculate the MRs
agg2<-melt(aggcast[,1:9],id=c("cat_code","gender"))
agg2<-rename(agg2, age = variable)
agg2<-rename(agg2, deaths = value)
agg2<-merge(agg2,pop,by=c("gender","age"))
agg2$CR<-agg2$deaths/agg2$population
agg2$cat_code<-as.factor(agg2$cat_code)
agg2$MRSE<-agg2$CR/(sqrt(agg2$deaths))
agg2$RSE<-agg2$MRSE/agg2$CR*100
agg2$logCR<-log(agg2$CR)

agg2_wide<-melt(agg2[,c(1:3,8)], id=c("age","cat_code","gender"))
agg2cast<-dcast(agg2_wide, cat_code + gender ~ age)
agg2cast$mincheck<-apply(agg2cast[,3:9], 1, FUN = min)
RSEbyage<-agg2cast[,3:9]
RSEbyage<-as.matrix(RSEbyage)
#write.csv(agg2cast,"cat_code_RSEs.csv")

plot(as.vector(Deathsbyage),as.vector(RSEbyage))
abline(h=22, col="blue")
abline(v=20, col="blue")
abline(h=8, col="blue")
abline(v=200, col="blue")

plot(as.vector(Deathsbyage),as.vector(RSEbyage),xlim=c(0,200))
abline(h=30, col="blue")
abline(v=20, col="blue")
abline(h=8, col="blue")
abline(v=200, col="blue")

par(mfrow=c(4,2))
library(Hmisc)
hist.data.frame(agg2cast[,3:9])

par(mfrow=c(1,1))
plot(RSE~deaths,data=agg,xlim=c(0,2000))
abline(h=10, col="blue")
abline(v=100, col="blue")

plot(RSE~deaths,data=agg)

#remove male and female specific codes
agg2cast %>%
  group_by(cat_code) %>%
  filter(n() >= 2) -> RSEs
aggcast %>%
  group_by(cat_code) %>%
  filter(n() >= 2) -> Deaths

length(unique(RSEs$cat_code))#1302 non-sex specific codes
RSEs_long<-melt(RSEs[,c(1:9)], id=c("cat_code","gender"))
Deaths_long<-melt(Deaths[,c(1:9)], id=c("cat_code","gender"))
Deaths_long<-rename(Deaths_long, age = variable)
Deaths_long<-rename(Deaths_long, deaths = value)
Deaths_long<-merge(Deaths_long,pop,by=c("gender","age"))
data_wide<-Deaths_long
data_wide$MR<-data_wide$deaths/data_wide$population*100000
data_wide$MRSE<-data_wide$MR/(sqrt(data_wide$deaths))
data_wide$RSE<-data_wide$MRSE/data_wide$MR*100
data_wide$logMR<-log(data_wide$MR)

logMR_by_gender<-dcast(data_wide[,c(1,2,3,9)], cat_code + age ~ gender)
logMR_by_gender$diff<-logMR_by_gender$Male-logMR_by_gender$Female

logMRdiff_by_age<-dcast(logMR_by_gender[,c(1,2,5)], cat_code ~ age)
par(mfrow=c(3,3))
plot(RSEs_F$`25-34`~RSEs_M$`25-34`)
plot(RSEs_F$`35-44`~RSEs_M$`35-44`)
plot(RSEs_F$`45-54`~RSEs_M$`45-54`)
plot(RSEs_F$`55-64`~RSEs_M$`55-64`)
plot(RSEs_F$`65-74`~RSEs_M$`65-74`)
plot(RSEs_F$`75-84`~RSEs_M$`75-84`)
plot(RSEs_F$`85+`~RSEs_M$`85+`)

#plotting these by gender again
data_wide %>%
  #filter (clusters == 5) %>%
  #filter (cat_code == "G30") %>%
  ggplot(aes(x = age, y = MR,group=interaction(cat_code,gender),color=gender,fill=cat_code)) +
  geom_line() +
  #scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  #scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  #geom_point(size=3,color="black",shape=21,alpha=.3) +
  #geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_continuous(breaks = c(1999,2019),
  #labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  #xlab("Age Group") + 
  #ylab("log (Male/Female)") +
  #ggtitle("Cluster 5")+
  #facet_grid(frailty ~ type) +
  #facet_wrap(~cat_code,nrow=4)+
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
  #guides(fill=F,color=F,size=F)+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())




logMRdiff_by_age$code->row.names(logMRdiff_by_age)
logMRdiff_by_age$code<-NULL

logMRdiff_by_cat<-as.data.frame(t(logMRdiff_by_age))
logMRdiff_by_cat_centered<-as.data.frame(scale(logMRdiff_by_cat,center=T,scale=F))

write.csv(logMRdiff_by_cat_centered,"logMRdiff_by_cat_centered.csv")

logMRdiff_by_age_centered<-as.data.frame(t(logMRdiff_by_cat_centered))

###########################

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


CoD<-logMRdiff_by_age_centered

#hc_d <- diana(CoD)
#hc_d$dc
codes<-unique(substr(CoD$code,1,1))

#hc_a <- agnes(CoD,method="average")
#hc_a <- agnes(CoD,method="single")
hc_a <- agnes(CoD,method="complete")
#hc_a <- agnes(CoD,method="ward")
#hc_a <- agnes(CoD,method="weighted")#fewer than 16
#hc_a <- agnes(CoD,method="flexible")
#hc_a <- agnes(CoD,method="gaverage")

#pltree(hc_d, cex = 0.6, hang = -1, main = "Dendrogram of diana")
pltree(hc_a, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hc_a, k = 16, border = 2:10)

set.seed(31)
# function to compute total within-cluster sum of squares
#gap_stat <- clusGap(CoD, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
#fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

#cutree(as.hclust(hc_d), k=11)->clusters
cutree(as.hclust(hc_a), k=16)->clusters

#########fviz_cluster(list(data = CoD, cluster = clusters))
#CoD$clusters<- cutree(as.hclust(hc_d), k=11)
CoD$clusters<- cutree(as.hclust(hc_a), k=16)

#return to plotting the SD using cluster groupings
CoD$code<-row.names(CoD)
clust<-as.data.frame(CoD[,8:9])

plot_clusters_centered<-melt(CoD[,c(1:7,9)], id=c("code"))
plot_clusters_centered<-rename(plot_clusters_centered, diff = value)
plot_clusters_centered<-rename(plot_clusters_centered, age = variable)
plot_clusters_centered<-merge(plot_clusters_centered,CoD[,8:9],by="code")
plot_clusters_centered<-merge(plot_clusters_centered,logMR_by_gender[,1:2],by="code")
plot_clusters_centered<-distinct(plot_clusters_centered)

codes#represents 16 different ICD-chapters

#sex difference by cluster
plot_clusters_centered %>%
  #filter (ch_grp == "E00-E89") %>%
  #filter (clusters == "1") %>%
  ggplot(aes(x = age, y = diff,group=code)) +
  geom_line() +
  #geom_smooth(method = "loess")+
  scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  #geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  geom_vline(xintercept=2,color="gray",alpha=0.7,linetype=2,size=1)+
  geom_vline(xintercept=3,color="gray",alpha=0.7,linetype=2,size=1)+
  geom_vline(xintercept=4,color="gray",alpha=0.7,linetype=2,size=1)+
  geom_vline(xintercept=5,color="gray",alpha=0.7,linetype=2,size=1)+
  geom_vline(xintercept=6,color="gray",alpha=0.7,linetype=2,size=1)+
  
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  #scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-.9, 1.9))+
  #scale_color_continuous(breaks = c(1999,2019),
  #labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Centered log (Male/Female)") +
  ggtitle("Clusters by Cause of Death")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~clusters,nrow=4)+
  ylim(-1.5,1.5)+
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
  theme(axis.text.x = element_blank())+
  guides(fill=F,color=F,size=F)+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())

write.csv(CoD,"CoD.csv")

#sex difference by cluster
plot_clusters_centered %>%
  #filter (ch_grp == "A00-B99") %>%
  #filter (type != "ch_grp") %>%
  filter (clusters == "1" | clusters == "4") %>%
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
  ylab("Centered log (Male/Female)") +
  ggtitle("Clusters by Cause of Death")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~code,nrow=4)+
  #ylim(-1,1)+
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
allcause_by_age<-as.data.frame(t(allcause_wide))

#allcause_wide_scale<-as.data.frame(scale(allcause_wide))


ch_grp_wide<-melt(ch_grp[,c(1,4:5)], id=c("age","ch_grp"))
ch_grp_wide<-dcast(ch_grp_wide, variable + age ~ ch_grp)
row.names(ch_grp_wide)<-ch_grp_wide$age
ch_grp_wide$variable<-NULL
ch_grp_wide$age<-NULL
write.csv(ch_grp_wide,"mort_wide_ch_grp.csv")
ch_grp_by_age<-as.data.frame(t(ch_grp_wide))
#ch_grp_wide_scale<-as.data.frame(scale(ch_grp_wide))


mort_by_age<-rbind(allcause_by_age,ch_grp_by_age,logMRdiff_by_age)
#mort_wide_noNaN<-mort_wide_scale[, colSums(is.na(mort_wide_scale)) != nrow(mort_wide_scale)]
#write.csv(mort_wide_scaled,"mort_wide_scaled.csv")

mort_by_age$group<-row.names(mort_by_age)
#mort_wide_scaled$age<-row.names(mort_wide_scaled)
mort_by_age_plot<-melt(mort_by_age, id=c("group"))
mort_by_age_plot<-rename(mort_by_age_plot, diff = value)
mort_by_age_plot<-rename(mort_by_age_plot, age = variable)
mort_by_age_plot$type<-rep(c("all","ch_grp","cat_code"), times = c(1,11,3313))
write.csv(mort_by_age_plot,"mmort_by_age_plot.csv")
mort_long_plot<-mort_by_age_plot
###load codes
codes<-read.csv("../data/codes.csv",header=T)
codes<-codes[,c(1,5)]

mort_long_plot$cat_code<-mort_long_plot$group
mort_long_notcat<-mort_long_plot[1:12,]
mort_long_cat<-mort_long_plot[13:3325,]

mort_long_cat<-merge(mort_long_cat,codes,by="cat_code")
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
  ggplot(aes(x = age, y = diff,group=1)) +
  geom_line() +
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

#clustering on sex diffs this time...
mort_long_cat<-mort_long[mort_long$type=="cat_code",]

