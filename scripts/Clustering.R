library(tidyverse)
library(dplyr)
library(janitor)
library(reshape2)
library(ggplot2)
library(viridis)

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
data_GACod <- read_tsv("../data/1999-2019_Gender-Age-byCauseofDeath.txt")

data1 <- janitor::clean_names(data_GACod)

#remove notes from dfs
data1<-as.data.frame(data1[-c(38915:38997),])

data1<-rename(data1, age = ten_year_age_groups_code)
data1<-rename(data1, ucd = underlying_cause_of_death)
data1<-rename(data1, ucd_group = underlying_cause_of_death_code)
data1<-rename(data1, MR = crude_rate, MRSE = crude_rate_standard_error)
data1$ucd_group<-substr(data1$ucd_group,1,3)

#aggregate deaths by ucd_group
data_agg<-data1 %>% 
  group_by(age, gender,ucd_group) %>% 
  summarise(deaths = sum(deaths), population = population)
dups<-duplicated(data_agg)
data_agg$dups<-dups
data_agg<-as.data.frame(data_agg)
data_agg<-data_agg[data_agg$dups=="FALSE",]
data_agg$MR<-data_agg$deaths/data_agg$population*100000
data_agg$MR<-round(data_agg$MR, 4)

#log the Mortality rates
data_agg$logMR<-log(data_agg$MR)
data_agg$ucd_group<-as.factor(data_agg$ucd_group)
data_agg$ucd_group<-droplevels(data_agg$ucd_group)
data_agg$gender<-as.factor(data_agg$gender)
data_agg$dups<-NULL

#centered and scaled, for hierarchical clustering
data_wide<-data_agg[,c(1,2,3,7)]
data_wide2<-melt(data_wide, id=c("gender","age","ucd_group"))
data_long<-dcast(data_wide2, variable + gender+ age ~ ucd_group)

#replace missing data with equivalent of no mortality
data_long[is.na(data_long)] = -8.5
data_scale <- scale(data_long[,c(4:1171)])  
data_scale<-as.data.frame(data_scale)
data_scale$gender<-data_long$gender
data_scale$age<-data_long$age
data_scale$genderage<-paste(data_scale$gender,data_scale$age)
rownames(data_scale) <- data_scale[,1171]
data_scale<-data_scale[,-c(1169:1171)]

write.csv(data_scale,"CoDscaled.csv")

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

data_clust<-as.data.frame(data_clust)
hc <- hclust(dist(data_scale, method = "euclidean"), method = "ward.D2")
plot(hc,hang = -0.01, cex = 0.7)


CoD<-t(data_scale)
CoD<-as.data.frame(CoD)
hc <- hclust(dist(CoD, method = "euclidean"), method = "ward.D2")
plot(hc,hang = -0.01, cex = 0.7)

plot(hc)
rect.hclust(hc,k=11, border="red")

fit <- cutree(hc, k=11)
fit                     #cluster labels for the data
CoD_clust<-CoD
CoD_clust$clusters<- cutree(hc, k = 11)

fviz_cluster(list(data = CoD, cluster = fit))

hc1 <- hclust((dist(CoD, method="euclidean")), method="average")
hc2 <- hclust((dist(CoD, method="euclidean")), method="ward.D2")

fit1 <- cutree(hc1, k=11)
CoD_clust$clusters <- cutree(hc1, k = 11)
fviz_cluster(list(data = CoD, cluster = fit1))

dend1 <- as.dendrogram((hc1))
dend2 <- as.dendrogram((hc2))

dend_list <- dendlist(dend1, dend2)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

gradient.color <- list(low = "steelblue",  high = "white")
CoD %>%    get_clust_tendency(n = 50, gradient = gradient.color)

Cod<-as.matrix(CoD)

nb <- NbClust(CoD, distance = "euclidean", diss = NULL, min.nc = 2, max.nc = 13, method = "kmeans")
nb

install.packages("apcluster")

library("apcluster")
c1 <- cbind(rnorm(30,.3,.5),rnorm(30.7,.4))
c2 <- cbind(rnorm(30,.7,.4),rnorm(30.4,.5))
x1 <- rbind(c1,c2)

plot(x1, xlab="", ylab="", pch=19, cex=.8)

apresia <- apcluster(negDistMat(r=2),CoD)

s1 <- negDistMat(CoD,r=2)
apres1b <- apcluster(s1)

plot(apresia,CoD)

hc_d <- diana(CoD)
hc_d$dc

plot.diana(hc_d)

pltree(hc_d, cex = 0.6, hang = -1, main = "Dendrogram of diana")

cutree(as.hclust(hc_d), k=11)->clusters


heatmap(Cod)
fviz_cluster(list(data = Cod, cluster = clusters))
CoD$clusters<- cutree(as.hclust(hc_d), k=11)
#return to plotting the SD using cluster groupings
CoD$ucd_group<-row.names(CoD)
clust<-as.data.frame(CoD[,15:16])
clust$original<-substr(clust$ucd_group,1,1)

originalgroups<-unique(clust$original)
originalgroups2<-c("A00-B99","A00-B99","C00-D48","C00-D48","E00-E88","F01-F99","G00-G98",
                   "I00-I99","J00-J98","K00-K92","N00-N98","R00-R99","V01-Y89","V01-Y89",
                   "V01-Y89","V01-Y89")
df <- data.frame(matrix(ncol = 2, nrow = 16))
df$original<-originalgroups
df$original2<-originalgroups2

clust<-merge(clust,df[,c(3:4)],by="original")
data_clust<-merge(data_agg,clust,by="ucd_group")

mort_wide <- melt(data_clust[,-c(4:6,8)], id=c("gender","age","ucd_group","clusters","original2"))
mort_long<-dcast(mort_wide, variable + age + ucd_group + clusters + original2 ~ gender)
mort_long[is.na(mort_long)] = -8.5
mort_long$diff<-mort_long$Male-mort_long$Female


#sex difference in mortality by cluster
mort_long %>%
  na.omit() %>%
  ggplot(aes(x = age, y = diff,group = interaction(clusters,ucd_group),color = original2,fill = diff)) +
  geom_line() +
  geom_point(size=3,color="black",shape=21,alpha=.3) +
  geom_hline(yintercept=0,color="gray",alpha=0.9,linetype=2,size=1)+
  #geom_errorbar(aes(ymin=crude_rate-crude_rate_standard_error, ymax=crude_rate+crude_rate_standard_error), width=.1)
  scale_fill_gradient2(low = "orange", mid = "white", high = "purple", midpoint = 0, limits = c(-2, 2))+
  #scale_color_viridis(breaks = c(1999,2019),labels = c("1999","2019"))+
  #stat_summary(fun = sum, geom = "line")
  #geom_smooth(method='lm', formula= y~x)+
  xlab("Age Group") + 
  ylab("Sex Difference in Mortality Rate \nlog (Male/Female)") +
  ggtitle("attempt at clustering")+
  #facet_grid(frailty ~ type) +
  facet_wrap(~clusters)+
  #ylim(0,1)+
  #facet_grid_sc(rows = vars(group), scales = list(y=scales_y))+
  #scale_y_log10()+
  #theme(legend.position = "none")+ 
  #theme(legend.position = c(0.9, 0.7))+
  theme(legend.key=element_blank())+ 
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = .5), 
        strip.background = element_rect(color = "black", size = 1))+  theme(plot.title = element_text(size = 20, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.5))+
  #theme(axis.text.x = element_blank())+
  #theme(axis.title.x=element_blank())+
  #theme(axis.text.y = element_blank())+
  guides(fill=F)+
  #scale_x_discrete(labels=c("45-54" = "Pre-frail \n(45-54)", "85+" = "Frail \n(85+)"))+
  theme(text=element_text(size=16,  family="Bookman"))+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank())



