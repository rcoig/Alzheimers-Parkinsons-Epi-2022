library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)

###load scaled diff data by group and age
mort_wide<-read.csv("../data/mort_wide_catcode.csv",header=T)

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


row.names(mort_wide)<-mort_wide$X
mort_wide$X<-NULL

CoD<-t(mort_wide)
CoD<-as.data.frame(CoD)

hc_d <- diana(CoD)
hc_d$dc

pltree(hc_d, cex = 0.6, hang = -1, main = "Dendrogram of diana")
cutree(as.hclust(hc_d), k=20)->clusters
fviz_cluster(list(data = CoD, cluster = clusters))
CoD$clusters<- cutree(as.hclust(hc_d), k=20)

#return to plotting the SD using cluster groupings
CoD$cat_code<-row.names(CoD)
clust<-as.data.frame(CoD[,8:9])

codes<-read.csv("../data/codes.csv",header=T)
codes2<-codes[,c(5:6)]

list<-unique(CoD$cat_code)
mort_long<-read.csv("../data/mort_long_cat_code.csv",header=T)

mort_long<-mort_long[mort_long$cat_code %in% list,]
mort_long<-merge(mort_long,clust,by="cat_code")
mort_long<-distinct(mort_long)

#sex difference by cluster
mort_long %>%
  #filter (type != "ch_grp") %>%
  ggplot(aes(x = age, y = diff,group=cat_code)) +
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


mort_long_combined<-merge(mort_long_combined,codes2,by="cat_code",all=F)
mort_long_combined<-distinct(mort_long_combined)


mort_long_combined[mort_long_combined$clusters==1,]->Cluster1
Cluster1<-distinct(Cluster1[,c(1,7,8)])#8
mort_long_combined[mort_long_combined$clusters==2,]->Cluster2
Cluster2<-distinct(Cluster2[,c(1,7,8)])#51
mort_long_combined[mort_long_combined$clusters==3,]->Cluster3
Cluster3<-distinct(Cluster3[,c(1,7,8)])#40
mort_long_combined[mort_long_combined$clusters==4,]->Cluster4
Cluster4<-distinct(Cluster4[,c(1,7,8)])#148
mort_long_combined[mort_long_combined$clusters==5,]->Cluster5
Cluster5<-distinct(Cluster5[,c(1,7,8)])#27
mort_long_combined[mort_long_combined$clusters==6,]->Cluster6
Cluster6<-distinct(Cluster6[,c(1,7,8)])#97
mort_long_combined[mort_long_combined$clusters==7,]->Cluster7
Cluster7<-distinct(Cluster7[,c(1,7,8)])#66
mort_long_combined[mort_long_combined$clusters==8,]->Cluster8
Cluster8<-distinct(Cluster8[,c(1,7,8)])#39
mort_long_combined[mort_long_combined$clusters==9,]->Cluster9
Cluster9<-distinct(Cluster9[,c(1,7,8)])#23
mort_long_combined[mort_long_combined$clusters==10,]->Cluster10
Cluster10<-distinct(Cluster10[,c(1,7,8)])#5
mort_long_combined[mort_long_combined$clusters==11,]->Cluster11
Cluster11<-distinct(Cluster11[,c(1,7,8)])#13

N<-c(98,51,40,148,27,97,66,39,23,5,13)

write.csv(mort_long_combined,"ClusteredScaledDiff.csv")
write.csv(Cluster1,"Cluster1.csv")
write.csv(Cluster2,"Cluster2.csv")
write.csv(Cluster3,"Cluster3.csv")
write.csv(Cluster4,"Cluster4.csv")
write.csv(Cluster5,"Cluster5.csv")
write.csv(Cluster6,"Cluster6.csv")
write.csv(Cluster7,"Cluster7.csv")
write.csv(Cluster8,"Cluster8.csv")
write.csv(Cluster9,"Cluster9.csv")
write.csv(Cluster10,"Cluster10.csv")
write.csv(Cluster11,"Cluster11.csv")



mort_long_Take2<-read.csv("../data/LogMortalitybyCatcode1999-2019Take2.csv",header=T)
mort_long_Take2<-merge(mort_long_Take2,clust,by="cat_code")
mort_long_Take2<-distinct(mort_long_Take2)


#UNSCALED sex difference by cluster
mort_long_Take2 %>%
  filter (clusters == 1) %>%
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
  ggtitle("Cluster 2")+
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



