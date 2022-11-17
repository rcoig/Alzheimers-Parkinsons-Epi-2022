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

hc_d <- diana(CoD) #Diana doesn't work great
#hc_d$dc

hc_d$merge->merge
merge<-as.data.frame(merge)
hc_d$height->merge$height
hc_d$or

as.dendrogram(hc_d)->hc_d_dend
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
plot(hc_d,  xlab = "Height",
     nodePar = nodePar, horiz = TRUE)

plot(hc_d,  xlab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))

write.csv(hc_d_dend,"hc_d_dend.csv")
print()
#Agnes is basically the same as the regular hclust function in r
#Mona would be used for binary data

#hc_a <- agnes(CoD,method="average")
#hc_a <- agnes(CoD,method="complete")
#hc_a <- agnes(CoD,method="ward")
#hc_a <- agnes(CoD,method="weighted")#fewer than 16
#hc_a <- agnes(CoD,method="flexible")
#hc_a <- agnes(CoD,method="gaverage") # gaverage does the best job

pltree(hc_d, cex = 0.6, hang = -1, main = "Dendrogram of diana")
pltree(hc_a, cex = 0.6, hang = -1, main = "Hierarchy by Sex Difference in Cause of Death")
rect.hclust(hc_a, k = 2, border = 2:10)

bannerplot(x, w = rev(x$height), fromLeft = TRUE, main=NULL, sub=NULL, xlab = "Height", adj = 0, col = c(2, 0), border = 0, axes = TRUE, frame.plot = axes, rev.xax = !fromLeft, xax.pretty = TRUE, labels = NULL, nmax.lab = 35, max.strlen = 5, yax.do = axes && length(x$order) <= nmax.lab, yaxRight = fromLeft, y.mar = 2.4 + max.strlen/2.5, ...)

#set.seed(31)
# function to compute total within-cluster sum of squares
#gap_stat <- clusGap(CoD, FUN = kmeans, nstart = 30, K.max = 24, B = 50)
#fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")

cutree(as.hclust(hc_d), k=2)->clusters
cutree(as.hclust(hc_a), k=50)->clusters
print(clusters)

#install.packages("ape")
library("ape")
# Default plot
plot(as.phylo(hc_d_dend), cex = 0.6, label.offset = .01,type="fan",no.margin=T)

#install.packages("ggdendro")
library("ggdendro")
dend_data <- ggdendro::dendro_data(hc_d_dend, type = "rectangle")




fviz_cluster(list(data = CoD, cluster = clusters))
CoD$clusters<- cutree(as.hclust(hc_d), k=40)
CoD$clusters<- cutree(as.hclust(hc_a), k=50)

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