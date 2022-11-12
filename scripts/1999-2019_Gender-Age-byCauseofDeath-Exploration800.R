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
desc<-read.csv("../data/Descriptions.csv",header=T)

diffrange<-read.csv("../data/diffrange2.csv",header=T)
plot(abs(diffrange$average.sex.bias)~diffrange$range,ylab="Abs. Value of Average Sex Ratio MR",xlab="Total Range of Sex Ratio MR Across 25-85+")
boxplot(range~clusters,data=diffrange)
boxplot(average.sex.bias~clusters,data=diffrange)
cor(abs(diffrange$average.sex.bias), diffrange$range) 

simple.fit = lm(abs(average.sex.bias)~range, data=diffrange)
summary(simple.fit)
hist(abs(diffrange$average.sex.bias))
hist(abs(diffrange$range))

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

#removing 25-44 age range to increase the number of codes that are included in the analysis
dcastcheck$`25-34`<-NULL
dcastcheck$`35-44`<-NULL

dcastcheck$mincheck<-apply(dcastcheck[,2:6], 1, FUN = min)

#min cannot be less than 800 (~20 deaths min per year per gender)
keep<-as.data.frame(dcastcheck[dcastcheck$mincheck>=800,])
keep <- keep[complete.cases(keep),]
reliablecatcodes<-unique(keep$cat_code)
rm(data_agg_by_cat_code,dcastcheck)

#Process reliable codes 
overlap1<-(deaths$cat_code %in% reliablecatcodes)
deaths$remove<-overlap1
cat_codes_keep<-deaths[deaths$remove != "FALSE",]
length(unique(cat_codes_keep$cat_code))

#remove the 25-44 age groups
cat_codes_keep<-cat_codes_keep %>% 
  filter(age!="25-34") 

cat_codes_keep<-cat_codes_keep %>% 
  filter(age!="35-44") 

#should have 222
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
cat_codes<-cat_codes[cat_codes$cat_code!="C51",]
cat_codes<-cat_codes[cat_codes$cat_code!="C54",]
cat_codes<-cat_codes[cat_codes$cat_code!="C55",]
cat_codes<-cat_codes[cat_codes$cat_code!="C57",]
cat_codes<-cat_codes[cat_codes$cat_code!="C61",]

cat_codes$code<-cat_codes$cat_code
table(cat_codes$cat_code)

FINALdata<-cat_codes

length(unique(FINALdata$cat_code))#215 FINAL codes
sum(FINALdata[,2:8])#47,296,624 deaths


#calculate total deaths and average diff for each code
FINALdata$average<-rowMeans(FINALdata[,c(3:7)])
FINALdata$total<-rowSums(FINALdata[,c(3:7)])

write.csv(FINALdata,"FINALdataOLDER.csv")
#rm(cat_codes,cat_ranges,ch_grps,subch_grps,reliablecatcodes,reliablecatranges,reliablechgrps,reliablesubchgrps)

############################################
#FORMAT FOR CENTERING BY DIFF

hierarchy<-distinct(FINALdata[,c(1,8:10)])
FINALdata<-FINALdata[,c(1:7)]
FINALdata$cat_code->row.names(FINALdata)
melt<-melt(FINALdata, id=c("cat_code","gender"))
melt<-rename(melt, age = variable)
melt<-rename(melt, deaths = value)
melt<-merge(melt,pop,by=c("gender","age"))

melt$MR<-melt$deaths/melt$population*100000
melt$logMR<-log(melt$MR)

#melt2<-melt(AVGdata, id=c("code","gender"))
#melt2<-rename(melt2, age = variable)
#melt2<-rename(melt2, deaths = value)
#pop2<-pop %>% 
  group_by(gender) %>% 
  summarise(population = sum(population))
#melt2<-merge(melt2,pop2,by=c("gender"))

#melt2$MR<-melt2$deaths/melt2$population*100000
#melt2$logMR<-log(melt2$MR)

logMR_by_gender<-dcast(melt[,c(1,2,3,7)], cat_code + age ~ gender)
logMR_by_gender$diff<-logMR_by_gender$Male-logMR_by_gender$Female

#logMRAVG_by_gender<-dcast(melt2, age + code ~ gender)
#logMRAVG_by_gender$diff<-logMRAVG_by_gender$Male-logMRAVG_by_gender$Female


logMRdiff_by_age<-dcast(logMR_by_gender[,c(1,2,5)], cat_code ~ age)
logMRdiff_by_age$cat_code->row.names(logMRdiff_by_age)
logMRdiff_by_age$cat_code<-NULL

logMRdiff_by_cat<-as.data.frame(t(logMRdiff_by_age))
logMRdiff_by_cat_centered<-as.data.frame(scale(logMRdiff_by_cat,center=T,scale=F))

write.csv(logMRdiff_by_cat_centered,"logMRdiff_by_cat_centeredOLDER.csv")

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
length(unique(CoD$cat_code))#196

#hc_d <- diana(CoD) #Diana doesn't work great
#hc_d$dc

#hc_a <- agnes(CoD,method="average")
#hc_a <- agnes(CoD,method="complete")
#hc_a <- agnes(CoD,method="ward")
hc_a <- agnes(CoD,method="weighted")#fewer than 16
#hc_a <- agnes(CoD,method="flexible")
#hc_a <- agnes(CoD,method="gaverage") # gaverage does the best job

#pltree(hc_d, cex = 0.6, hang = -1, main = "Dendrogram of diana")
pltree(hc_a, cex = 0.6, hang = -1, main = "Hierarchy by Sex Difference in Cause of Death")
rect.hclust(hc_a, k = 16, border = 2:10)

#set.seed(31)
# function to compute total within-cluster sum of squares
#gap_stat <- clusGap(CoD, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
#fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

#cutree(as.hclust(hc_d), k=16)->clusters
cutree(as.hclust(hc_a), k=19)->clusters

fviz_cluster(list(data = CoD, cluster = clusters))
#CoD$clusters<- cutree(as.hclust(hc_d), k=16)
CoD$clusters<- cutree(as.hclust(hc_a), k=19)

#return to plotting the SD using cluster groupings
CoD$code<-row.names(CoD)
codes<-unique(substr(CoD$code,1,1))#19letters
clust<-as.data.frame(CoD[,6:7])

plot_clusters_centered<-melt(CoD[,c(1:5,7)], id=c("code"))
plot_clusters_centered<-rename(plot_clusters_centered, diff = value)
plot_clusters_centered<-rename(plot_clusters_centered, age = variable)
plot_clusters_centered<-merge(plot_clusters_centered,CoD[,6:7],by="code")
#plot_clusters_centered<-merge(plot_clusters_centered,logMR_by_gender[,1:2],by="code")
plot_clusters_centered<-distinct(plot_clusters_centered)

codes#represents 16 different ICD-chapters so clustered on k = 16 for comparison
#THIS TIME ITS ACTUALLY 19???

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


Results<-merge(CoD,hierarchy,by=c("code"))
Results<-merge(Results,logMRAVG_by_gender[logMRAVG_by_gender$age=="total",],by="code",)
Results<-merge(Results,desc,by="code")
write.csv(Results,"ResultsWITHtotals.csv")
melt2<-merge(melt2,desc,by="code")
write.csv(melt2,"SexStratResultsWithavgandtotaldeaths.csv")

Cluster1<-Results[Results$clusters==1,]
total<-melt2[melt2$age=="total",]
total<-merge(total,Results[,c(1,9)],by="code")
Cluster1total<-total[total$clusters=="1",]
sum(Cluster1total$deaths)

Cluster13total<-total[total$clusters=="13",]
sum(Cluster13total$deaths)

Cluster4total<-total[total$clusters=="4",]
sum(Cluster4total$deaths)

Results$cat_code<-as.factor(Results$cat_code)

totalagg<-total %>% 
  group_by(clusters) %>% 
  summarise(deaths = sum(deaths))

codetally<-Results %>% 
  group_by(clusters,cat_range) %>% 
  tally()

cat_codetally<-codetally %>%
summarise(Ncat_codes = sum(n))
sum(cat_codetally$Ncat_codes)

totalagg<-merge(totalagg,cat_codetally,by="clusters")
write.csv(totalagg,"clusterproperties.csv")

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

Cluster1<-plot_clusters[plot_clusters$clusters==1,]
length(unique(Cluster1$code))#34
Cluster7<-plot_clusters[plot_clusters$clusters==7,]
length(unique(Cluster7$code))#3
Cluster3<-plot_clusters[plot_clusters$clusters==3,]
length(unique(Cluster3$code))#41
Cluster6<-plot_clusters[plot_clusters$clusters==6,]
length(unique(Cluster6$code))#17
Cluster4<-plot_clusters[plot_clusters$clusters==4,]
length(unique(Cluster4$code))#2

#sex difference by cluster
plot_clusters_centered %>%
  filter (clusters == "16") %>%
  ggplot(aes(x = age, y = diff,group=code)) +
  geom_line() +
  scale_size_manual(values = c(all = 2, cat_code = .5, ch_grp = 2))+
  scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=1)+
  xlab("Age Group") + 
  ylab("Centered log (Male/Female) Mortality") +
  ggtitle("Cluster 16")+
  ylim(-2,3)+
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

Cluster1External<-plot_clusters[c(1:56,722:728,736:742,1058:1372),]
Cluster1Physiological<-plot_clusters[c(57:721,729:735,743:1057),]

#sex difference by cluster
Cluster1Physiological %>%
  filter (clusters == "1" ) %>%
  ggplot(aes(x = age, y = diff,group=code,color=diff)) +
  geom_line(size=2) +
  #scale_color_gradientn(high = "#5D3A9B", low = "gray")+
  scale_color_gradientn(colors = c("gray","gray","#E66100"),
                         values = c(2,.7,.2,-1))+ 
    #scale_size_manual(values = c(all = 4, cat_code = .5, ch_grp = 2))+
  #scale_color_manual(values = c(all = "gray", cat_code = "gray", ch_grp = "green"))+
  geom_hline(yintercept=0,color="gray",alpha=0.7,linetype=2,size=3)+
  xlab("Age Group") + 
  ylab("Log mortality sex ratio") +
  ggtitle("Cluster 1 Physiological")+
  ylim(-1,2.1)+
  theme(legend.position = "none")+ 
  theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(fill="none",color="none",size="none",scale="none")+
  theme(text=element_text(size=20))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())