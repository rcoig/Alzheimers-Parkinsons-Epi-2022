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

#aggregate reliable codes by cat_code
data_agg_by_cat_code<-deaths %>% 
  group_by(age, cat_code) %>% 
  summarise(deaths = sum(deaths))
dcastcheck<-dcast(data_agg_by_cat_code,cat_code ~ age)
dcastcheck$mincheck<-apply(dcastcheck[,2:8], 1, FUN = min)

#min cannot be less than 500 (~25 deaths min per year per gender)
keep<-as.data.frame(dcastcheck[dcastcheck$mincheck>=500,])
keep <- keep[complete.cases(keep),]
reliablecatcodes<-unique(keep$cat_code)#130
rm(data_agg_by_cat_code,dcastcheck)

#Process 130 codes as cat_codes
overlap1<-(deaths$cat_code %in% reliablecatcodes)
deaths$remove<-overlap1
cat_codes_keep<-deaths[deaths$remove != "FALSE",]
length(unique(cat_codes_keep$cat_code))

deaths<-deaths[deaths$remove != "TRUE",]
length(unique(deaths$cat_code))#now remaining death data is not duplicated

#aggregate remaining deaths by cat range 
data_agg_by_cat_range<-deaths %>% 
  group_by(age, cat_range) %>% 
  summarise(deaths = sum(deaths))
dcastcheck<-dcast(data_agg_by_cat_range,cat_range ~ age)
dcastcheck$mincheck<-apply(dcastcheck[,2:8], 1, FUN = min)

#min cannot be less than 450 (~25 deaths min per year per gender)
keep<-as.data.frame(dcastcheck[dcastcheck$mincheck>=450,])
keep <- keep[complete.cases(keep),]
reliablecatranges<-unique(keep$cat_range)#53
rm(data_agg_by_cat_range,dcastcheck)

#Process 57 codes as cat_ranges
overlap1<-(deaths$cat_range %in% reliablecatranges)
deaths$remove<-overlap1
cat_ranges_keep<-deaths[deaths$remove != "FALSE",]
length(unique(cat_ranges_keep$cat_range))

deaths<-deaths[deaths$remove != "TRUE",]
length(unique(deaths$cat_code))#now remaining death data is not duplicated

#aggregate remaining deaths by subch_grp 
data_agg_by_subch_grp<-deaths %>% 
  group_by(age, subch_grp) %>% 
  summarise(deaths = sum(deaths))
dcastcheck<-dcast(data_agg_by_subch_grp,subch_grp ~ age)
dcastcheck$mincheck<-apply(dcastcheck[,2:8], 1, FUN = min)

#min cannot be less than 500 (~25 deaths min per year per gender)
keep<-as.data.frame(dcastcheck[dcastcheck$mincheck>=500,])
keep <- keep[complete.cases(keep),]
reliablesubchgrps<-unique(keep$subch_grp)#2
rm(data_agg_by_subch_grp,dcastcheck)

#Process 2 codes as subch_grps
overlap1<-(deaths$subch_grp %in% reliablesubchgrps)
deaths$remove<-overlap1
subch_grps_keep<-deaths[deaths$remove != "FALSE",]
length(unique(subch_grps_keep$subch_grp))

deaths<-deaths[deaths$remove != "TRUE",]
length(unique(deaths$cat_code))#now remaining death data is not duplicated

#aggregate remaining deaths by ch_grp 
data_agg_by_ch_grp<-deaths %>% 
  group_by(age, ch_grp) %>% 
  summarise(deaths = sum(deaths))
dcastcheck<-dcast(data_agg_by_ch_grp,ch_grp ~ age)
dcastcheck$mincheck<-apply(dcastcheck[,2:8], 1, FUN = min)

#min cannot be less than 500 (~25 deaths min per year per gender)
keep<-as.data.frame(dcastcheck[dcastcheck$mincheck>=500,])
keep <- keep[complete.cases(keep),]
reliablechgrps<-unique(keep$ch_grp)#12
rm(data_agg_by_ch_grp,dcastcheck)

#Process 11 codes as ch_grps
overlap1<-(deaths$ch_grp %in% reliablechgrps)
deaths$remove<-overlap1
ch_grps_keep<-deaths[deaths$remove != "FALSE",]
length(unique(ch_grps_keep$ch_grp))

deaths<-deaths[deaths$remove != "TRUE",]
length(unique(deaths$cat_code))#now remaining death data is not duplicated

#aggregate remaining deaths by cat_code, THESE REMOVED FROM DATA 
cat_codes_UNUSED<-deaths %>% 
  group_by(age, gender, cat_code) %>% 
  summarise(deaths = sum(deaths))
dcastcheck<-dcast(cat_codes_UNUSED, gender + cat_code ~ age)
dcastcheck$mincheck<-apply(dcastcheck[,2:8], 1, FUN = min)
rm(deaths,dcastcheck,overlap1,keep,cat_codes_UNUSED)

#should have 130 cat_codes + 53 cat ranges + 2 subch_grps + 12 ch_grps
cat_codes<-cat_codes_keep %>% 
  group_by(age, gender,cat_code) %>% 
  summarise(deaths = sum(deaths))
cat_codes<-dcast(cat_codes, gender + cat_code ~ age)
cat_codes<-merge(cat_codes,cat_codes_keep[,4:7],by="cat_code")
cat_codes<-distinct(cat_codes)
cat_codes$type<-"cat_code"
rm(cat_codes_keep)
#remove male and female specific
cat_codes<-cat_codes[cat_codes$cat_code!="C56",]
cat_codes<-cat_codes[cat_codes$cat_code!="C53",]
cat_codes$code<-cat_codes$cat_code
table(cat_codes$cat_code)
cat_codes<-cat_codes[,c(2:9,1,11,10,12,13,14)]

cat_ranges<-cat_ranges_keep %>% 
  group_by(age, gender,cat_range) %>% 
  summarise(deaths = sum(deaths))
cat_ranges<-dcast(cat_ranges, gender + cat_range ~ age)
cat_ranges<-merge(cat_ranges,cat_ranges_keep[,4:7],by="cat_range")
cat_ranges$cat_code<-NA
cat_ranges<-distinct(cat_ranges)
cat_ranges$type<-"cat_range"
rm(cat_ranges_keep)
#remove male and female specific
cat_ranges<-cat_ranges[cat_ranges$cat_range!="C51-C58",]
cat_ranges<-cat_ranges[cat_ranges$cat_range!="C60-C63",]
table(cat_ranges$cat_range)
cat_ranges$code<-cat_ranges$cat_range
cat_ranges<-cat_ranges[,c(2:9,10,1,11:13,14)]

subch_grps<-subch_grps_keep %>% 
  group_by(age, gender,subch_grp) %>% 
  summarise(deaths = sum(deaths))
subch_grps<-dcast(subch_grps, gender + subch_grp ~ age)
subch_grps<-merge(subch_grps,subch_grps_keep[,4:7],by="subch_grp")
subch_grps$cat_code<-NA
subch_grps$cat_range<-NA
subch_grps<-distinct(subch_grps)
subch_grps$type<-"subch_grp"
rm(subch_grps_keep)
subch_grps$code<-subch_grps$subch_grp
subch_grps<-subch_grps[,c(2:9,10,11,1,12,13,14)]


ch_grps<-ch_grps_keep %>% 
  group_by(age, gender,ch_grp) %>% 
  summarise(deaths = sum(deaths))
ch_grps<-dcast(ch_grps, gender + ch_grp ~ age)
ch_grps<-merge(ch_grps,ch_grps_keep[,4:7],by="ch_grp")
ch_grps$cat_code<-NA
ch_grps$cat_range<-NA
ch_grps$subch_grp<-NA
ch_grps<-distinct(ch_grps)
ch_grps$type<-"ch_grp"
rm(ch_grps_keep)
ch_grps$code<-ch_grps$ch_grp
ch_grps<-ch_grps[,c(2:9,10,12,11,1,13,14)]

FINALdata<-rbind(cat_codes,cat_ranges,subch_grps,ch_grps)
length(unique(FINALdata$code))#196 FINAL codes
sum(FINALdata[,2:8])#50,705,600 deaths

#write.csv(FINALdata,"FINALdata.csv")
#rm(cat_codes,cat_ranges,ch_grps,subch_grps,reliablecatcodes,reliablecatranges,reliablechgrps,reliablesubchgrps)

#calculate total deaths and average diff for each code
FINALdata$average<-rowMeans(FINALdata[,c(2:8)])
FINALdata$total<-rowSums(FINALdata[,c(2:8)])
AVGdata<-FINALdata[,c(1,9:11)]
FINALdata<-FINALdata[,-c(10:11)]

############################################
#FORMAT FOR CENTERING BY DIFF

hierarchy<-distinct(FINALdata[,c(9:14)])
FINALdata<-FINALdata[,c(1:8,14)]
FINALdata$code->row.names(FINALdata)
melt<-melt(FINALdata, id=c("code","gender"))
melt<-rename(melt, age = variable)
melt<-rename(melt, deaths = value)
melt<-merge(melt,pop,by=c("gender","age"))

melt$MR<-melt$deaths/melt$population*100000
melt$logMR<-log(melt$MR)

logMR_by_gender<-dcast(melt[,c(1,2,3,7)], code + age ~ gender)
logMR_by_gender$diff<-logMR_by_gender$Male-logMR_by_gender$Female

logMRdiff_by_age<-dcast(logMR_by_gender[,c(1,2,5)], code ~ age)
logMRdiff_by_age$code->row.names(logMRdiff_by_age)
logMRdiff_by_age$code<-NULL

logMRdiff_by_cat<-as.data.frame(t(logMRdiff_by_age))
logMRdiff_by_cat_centered<-as.data.frame(scale(logMRdiff_by_cat,center=T,scale=F))

#write.csv(logMRdiff_by_cat_centered,"logMRdiff_by_cat_centered.csv")

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

#clustering is done on the centered data, row-wise = by cause of death
CoD<-logMRdiff_by_age_centered
length(unique(CoD$code))#196

#hc_d <- diana(CoD) #Diana doesn't work great
#hc_d$dc

#hc_a <- agnes(CoD,method="average")
#hc_a <- agnes(CoD,method="complete")
#hc_a <- agnes(CoD,method="ward")
#hc_a <- agnes(CoD,method="weighted")#fewer than 16
#hc_a <- agnes(CoD,method="flexible")
hc_a <- agnes(CoD,method="gaverage") # gaverage does the best job

#pltree(hc_d, cex = 0.6, hang = -1, main = "Dendrogram of diana")
pltree(hc_a, cex = 0.6, hang = -1, main = "Hierarchy by Sex Difference in Cause of Death")
rect.hclust(hc_a, k = 16, border = 2:10)

#set.seed(31)
# function to compute total within-cluster sum of squares
#gap_stat <- clusGap(CoD, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
#fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

#cutree(as.hclust(hc_d), k=16)->clusters
cutree(as.hclust(hc_a), k=16)->clusters

fviz_cluster(list(data = CoD, cluster = clusters))
#CoD$clusters<- cutree(as.hclust(hc_d), k=16)
CoD$clusters<- cutree(as.hclust(hc_a), k=16)

#return to plotting the SD using cluster groupings
CoD$code<-row.names(CoD)
codes<-unique(substr(CoD$code,1,1))#19letters
clust<-as.data.frame(CoD[,8:9])

plot_clusters_centered<-melt(CoD[,c(1:7,9)], id=c("code"))
plot_clusters_centered<-rename(plot_clusters_centered, diff = value)
plot_clusters_centered<-rename(plot_clusters_centered, age = variable)
plot_clusters_centered<-merge(plot_clusters_centered,CoD[,8:9],by="code")
#plot_clusters_centered<-merge(plot_clusters_centered,logMR_by_gender[,1:2],by="code")
plot_clusters_centered<-distinct(plot_clusters_centered)

codes#represents 16 different ICD-chapters so clustered on k = 16 for comparison

#sex difference by cluster
plot_clusters_centered %>%
  ggplot(aes(x = age, y = diff,group=code)) +
  geom_line() +
  scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  geom_vline(xintercept=2,color="gray",alpha=0.7,linetype=2,size=.5)+
  geom_vline(xintercept=3,color="gray",alpha=0.7,linetype=2,size=.5)+
  geom_vline(xintercept=4,color="gray",alpha=0.7,linetype=2,size=.5)+
  geom_vline(xintercept=5,color="gray",alpha=0.7,linetype=2,size=.5)+
  geom_vline(xintercept=6,color="gray",alpha=0.7,linetype=2,size=.5)+
  xlab("Age Group") + 
  ylab("Centered log (Male/Female)") +
  ggtitle("Clusters by Cause of Death")+
  facet_wrap(~clusters,nrow=4)+
  ylim(-1.5,1.5)+
  theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  guides(fill=F,color=F,size=F)+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())


#Results<-merge(CoD,hierarchy,by=c("code"))
#write.csv(Results,"Results.csv")


#Plot clusters by original value rather than by centered
#add clusters to noncentered data
CoD2<-logMRdiff_by_age
CoD2$code<-row.names(CoD2)
CoD2$clusters<-CoD$clusters
plot_clusters<-melt(CoD2[,c(1:8)], id=c("code"))
plot_clusters<-rename(plot_clusters, diff = value)
plot_clusters<-rename(plot_clusters, age = variable)
plot_clusters<-merge(plot_clusters,plot_clusters_centered[,c(1,4)],by="code")
plot_clusters<-distinct(plot_clusters)


Liver<-plot_clusters[plot_clusters$code == "K70" | plot_clusters$code == "K70-K76" | plot_clusters$code == "B15-B19"
                     | plot_clusters$code == "K72" | plot_clusters$code == "K74" | plot_clusters$code == "K76"
                     | plot_clusters$code == "C22" | plot_clusters$code == "B18",]

#sex difference by cluster
Liver %>%
  #filter (clusters == "16" ) %>%
  ggplot(aes(x = age, y = diff, group = code)) +
  geom_line() +
  #scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  #scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  xlab("Age Group") + 
  ylab("log (Male/Female) Mortality") +
  ggtitle("Liver Diseases")+
  #ylim(-2,3)+
  facet_wrap(~clusters,ncol=5)+
    theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #guides(fill=F,color=F,size=F)+
  theme(text=element_text(size=16))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())