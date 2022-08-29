library(tidyverse)
library(dplyr)
library(janitor)

# Load data
MCD45 <- read_tsv("45-54 top50 causes by gender + MCD.txt")
MCD45<-MCD45[-c(11925:12113),]
MCD45$age<-"45-55"
MCD85 <- read_tsv("85+ top50 causes by gender + MCD.txt")
MCD85<-MCD85[-c(11925:12113),]
MCD85$age<-"85+"

MCD<-rbind(MCD45,MCD85)
MCD<-clean_names(MCD)
MCD.1<-MCD[is.na(MCD$notes),]


################################
## Create node and edge lists ##
################################

### Node list ###
sources <- MCD.1 %>%
  distinct(mcd_icd_chapter) %>%
  rename(label = mcd_icd_chapter)

destinations <- MCD.1 %>%
  distinct(underlying_cause_of_death) %>%
  rename(label = underlying_cause_of_death)

nodes <- full_join(sources, destinations, by = "label")

# Create id column and reorder columns
nodes <- add_column(nodes, id = 1:nrow(nodes)) %>% 
  select(id, everything())

### Edge list ###
per_route <- MCD.1 %>%  
  group_by(gender, age, mcd_icd_chapter, underlying_cause_of_death) %>%
  summarise(weight = sum(deaths)) %>% 
  ungroup()

##### asking about external falls in old age by gender
MCD85<-clean_names(MCD85)
MCD85$ucdc <- substr(MCD85$underlying_cause_of_death_code, 0, 3)


by_external <- MCD85 %>%  
  filter(mcd_icd_chapter == "External causes of morbidity and mortality") %>%
  group_by(gender, ucdc) %>%
  summarise(deaths = sum(deaths)) 

by_external<-merge(by_external,MCD85[,c(2,9)],by=c("gender","ucdc"),all=F)
by_external$MR<-by_external$deaths/by_external$population

library(gmodels)
CrossTable(by_external$deaths, by_external$gender, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

sum(MCD85$deaths)#75 million deaths over past 20 years  
hist(MCD85$underlying_cause_of_death)

by_external<-b

#library(MASS)
fit<-glm(formula = MR ~ gender + ucdc, family = "poisson", data = by_external)
summary(fit)

sum(MCD$deaths)#85 million datapoints

# Join with node ids and reorder columns
edges <- per_route %>% 
  left_join(nodes, by = c("mcd_icd_chapter" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("underlying_cause_of_death" = "label")) %>% 
  rename(to = id)

edgesF45<- edges %>% filter(gender == "Female" & age == "45-55")
edgesM45<- edges %>% filter(gender == "Male" & age == "45-55")
edgesF85<- edges %>% filter(gender == "Female" & age == "85+")
edgesM85<- edges %>% filter(gender == "Male" & age == "85+")

edgesF45AHS<- edges %>% filter(gender == "Female" & age == "45-55" & 
                                 mcd_icd_chapter == "External causes of morbidity and mortality")

edgesF45 <- select(edgesF45, from, to, weight)
edgesM45 <- select(edgesM45, from, to, weight)
edgesF85 <- select(edgesF85, from, to, weight)
edgesM85 <- select(edgesM85, from, to, weight)

edgesF45AHS <- select(edgesF45AHS, from, to, weight)
edgesF45AHS <- filter(edgesF45AHS, weight > 400)

#####################
## network package ##
#####################

library(network)

# network object
routes_network <- network(edgesF45AHS, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

# Print network object
routes_network

# network plot
plot(routes_network, vertex.cex = 3)

# network circle plot
plot(routes_network, vertex.cex = 3, mode = "circle")

####################
## igraph package ##
####################

# Clean environment
detach(package:network)
rm(routes_network)
library(igraph)
?igraph::layout
# igraph object
nodes2<-unique(c(edgesF45AHS$from,edgesF45AHS$to))
routes_igraph <- graph_from_data_frame(d = edgesF45AHS, vertices = nodes2, directed = TRUE)

# Print igraph object
routes_igraph

# igraph plot
plot(routes_igraph, edge.arrow.size = 0.2, labels = )

# igraph-graphopt-plot
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)


