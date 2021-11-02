remove(list = ls())

setwd("~/Desktop/Nepal Flood Report")

library(dplyr)
library(ggplot2)
library(ggrepel)
library(rgdal) # For Nepal Map 


df <- read.csv("bipad.csv",stringsAsFactors = FALSE)
dim(df)
head(df)

### Total Male Death and Female Death almost similar ### 5795 Male vs 6272 Female, 8% more Female ###
### Total Loss Estimated is mostly NA ###

sapply(df, function(x) sum(is.na(x))) ### Only total estimated loss in NA ### Rest all cool ###


tail(df$Incident.on) # Just to see the furthest date behind which is 2011 

colnames(df)

table(df$Hazard) # To see the frequency of all Hazard types # Maximum is Fire, for example 


df_selected <- filter(df, Hazard %in% c("Flood")) # Keep Landslide?
print(dim(df)); print(dim(df_selected))
head(df_selected)

sum(is.na(df_selected$Total.estimated.loss..NPR.))
length(df_selected$Total.estimated.loss..NPR.)

df_selected$Total.estimated.loss..NPR.

### Districts when only Landslide is selected is less than the total Districts of Nepal ###
### Fill them with 0 cases ###

print(length(unique(df$District))); print(length(unique(df_selected$District)))

#non_landslide_districts <- setdiff(df$District, df_selected$District)

### Let's try to Cluster the Districts now on the basis on Incidents and Deaths ### 
### By grouping the observations by Districts and collecting the total sum across the same timeline ###

df_selected$Count <- 1

df_cluster <- as.data.frame(group_by(df_selected, District) %>% summarise(sum(Count), sum(Total...People.Death)))
colnames(df_cluster) <- c("District","Total_Incident","Total_Death")
arrange(df_cluster, desc(Total_Incident)) %>% head(10) # Top 10 Districts with maximum Total_Incident 
arrange(df_cluster, desc(Total_Death)) %>% head(10) # Top 10 Districts with maximum Total_Death 
sapply(df_cluster, class)

ggplot(df_cluster, aes(x=Total_Incident, y=Total_Death, label=District)) + geom_point() + geom_text_repel()

df_cluster_scaled <- scale(df_cluster[,c(2,3)])


k.max <- 10
total_wss <- c()

for (k in 1:k.max) {
  total_wss <- c(total_wss, kmeans(df_cluster_scaled, k, iter.max = 10 )$tot.withinss)
}

plot(1:k.max, total_wss,
     type="b", 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

### So, going with the Cluster Size of 3 ### 

cluster_model <- kmeans(df_cluster_scaled, centers=3, iter.max = 10)

cluster_model$tot.withinss
cluster_model$centers
cluster_model$cluster
table(cluster_model$cluster)

df_cluster$Cluster_Number <- cluster_model$cluster # Adding cluster number in the dataframe 

### Only for Landlside only subset ### 
#df_to_bind <- data.frame(non_landslide_districts, 0,0,0)
#colnames(df_to_bind) <- c("District","Total_Incident","Total_Death","Cluster_Number")
#df_cluster <- rbind(df_cluster,df_to_bind)




df_cluster$Cluster_Label <- ifelse(df_cluster$Cluster_Number == 1, "Least Affected",
                              ifelse(df_cluster$Cluster_Number == 2, "Moderately Affected", "Most Affected"
                                     ))

#df_cluster <- arrange(df_cluster, District)




ggplot(df_cluster, aes(x=Total_Incident, y=Total_Death,color=Cluster_Label,label=District)) + labs(fill='NEW LEGEND TITLE') + 
  geom_point() + geom_text_repel() 


### Now adding Nepal Map to visualize the clusters better ### 


nepal.adm3.shp <- readOGR(dsn="NPL_adm", layer="NPL_adm3", stringsAsFactors = FALSE)
nepal.adm3.shp.df <- fortify(nepal.adm3.shp, region = "NAME_3")



map <- ggplot(data = nepal.adm3.shp.df, aes(x = long, y = lat, group = group))

map + geom_path()

map + 
  geom_polygon(aes(fill = id)) +
  coord_fixed(1.3) +
  guides(fill = FALSE)



df_cluster$District <- ifelse(grepl("Chitwan", df_cluster$District),"Chitawan",
                              
                              ifelse(grepl("Nawalparasi", df_cluster$District), "Nawalparasi",
                              
                              ifelse(grepl("Rukum", df_cluster$District), "Rukum", df_cluster$District)))


unique(df_cluster$District)

nepal.adm3.shp.df <- merge(nepal.adm3.shp.df, df_cluster, 
                           by.x ="id", by.y = "District", all=TRUE)




map <- ggplot(data = nepal.adm3.shp.df, aes(x = long, y = lat, group = group))

map + 
  geom_polygon(aes(fill = as.factor(Cluster_Label)), color = 'gray', size = 0.1) +
  coord_fixed(1.3)



centroids <- setNames(do.call("rbind.data.frame", by(nepal.adm3.shp.df, nepal.adm3.shp.df$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
centroids$label <- unique(df_cluster$District)

centroids

map + 
  geom_polygon(aes(fill = as.factor(Cluster_Label)), color = 'gray', size = 0.1) +
  coord_fixed(1.3) +
  theme(legend.justification=c(0,0), legend.position=c(0,0)) +
  with(centroids, annotate(geom="text", x = long, y = lat, label=label, size=2))+
  guides(fill=guide_legend(title="Flood Report"))

