Un-used code: working with shapefile in R and ggplot, would be easier to use sf. 


#Some resources:
#https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html
#https://www.worldfullofdata.com/r-tutorial-plot-maps-shapefiles/

#Maptools

#### import as sp object
feow_shp<-readShapeSpatial("C:Inputs//feow_hydrosheds.shp")
feow_shp@data
class(feow_shp)


feow_shp_crop <- feow_shp[feow_shp$FEOW_ID==c(101:120, 142), ]
feow_shp_crop_2 <- feow_shp_crop[, -2]

# plot(feow_shp_crop_2)

feow_shp_crop_2@data<-merge(feow_shp_crop_2@data, scores_combined_cumulative[,1:2], by.x=c("FEOW_ID"), by.y=c("region"))
cmist_data <- feow_shp_crop_2@data$CMIST_Score
colors <- (cmist_data - min(cmist_data)) / (max(cmist_data) - min(cmist_data))*254+1
feow_shp_crop_2@data$color =  colorRampPalette(c('#9eceff', '#004081'))(255)[colors]
plot(feow_shp_crop_2, col = feow_shp_crop_2@data$color, border=NA)

####Trying it in ggplot2

#  scale_fill_gradient(low = '#9eceff', high = '#004081', name = "CMIST_Score")+
#  scale_fill_continuous_sequential(name = "CMIST_Score", palette="Reds")+


library(colorspace)

#########
feow_shp_crop_2 <- gBuffer(feow_shp_crop_2, byid=TRUE, width=0)

# Plot map and legend with colors
feow_shp_crop_2_data <- fortify(feow_shp_crop_2, region = "FEOW_ID")
feow_shp_crop_2_data <- merge(feow_shp_crop_2_data, feow_shp_crop_2@data[, c('FEOW_ID', 'CMIST_Score')], by.x='id', by.y='FEOW_ID', all.x=TRUE)

ggplot(feow_shp_crop_2_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = CMIST_Score)) +
  geom_path(colour = "black") + 
  scale_fill_viridis(name="CMIST_Score")+
  borders(database="world", regions="canada", colour="black", linetype="dashed")+
  xlim(-170, -50) + ylim(40, 85)



##### Now for each species and all the regions
#World regions
feow_shp@data
class(feow_shp)

#crop it to only the Canadian regions
feow_shp_crop <- feow_shp[feow_shp$FEOW_ID==c(101:120, 142), ]
feow_shp_crop_2 <- feow_shp_crop[, -2]

#Add in the CMIST data
feow_shp_crop_2@data<-merge(feow_shp_crop_2@data, scores_combined[,c("species", "region", "CMIST_Score")], by.x=c("FEOW_ID"), by.y=c("region"))
feow_shp_crop_2@data

# cmist_data <- feow_shp_crop_2@data$CMIST_Score
# colors <- (cmist_data - min(cmist_data)) / (max(cmist_data) - min(cmist_data))*254+1
# feow_shp_crop_2@data$color =  colorRampPalette(c('#9eceff', '#004081'))(255)[colors]
# # plot(feow_shp_crop_2, col = feow_shp_crop_2@data$color, border=NA)

####Trying it in ggplot2

#  scale_fill_gradient(low = '#9eceff', high = '#004081', name = "CMIST_Score")+
#  scale_fill_continuous_sequential(name = "CMIST_Score", palette="Reds")+


library(colorspace)
# 
feow_plot<-ggplot(feow_shp_crop_2, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white") +
  geom_path(color = "black") +
  coord_equal()



#########
feow_shp_crop_2 <- gBuffer(feow_shp_crop_2, byid=TRUE, width=0)

feow_shp_crop_2_marble_subset<- subset(feow_shp_crop_2, species=="marble")

# Fortify to add colours: 
feow_shp_crop_2_data <- fortify(feow_shp_crop_2, region = "FEOW_ID")
feow_shp_crop_2_data <- merge(feow_shp_crop_2_data, feow_shp_crop_2@data, by.x='id', by.y='FEOW_ID', all.x=TRUE)

ggplot(feow_shp_crop_2 %>% filter(species=="marble"), aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = feow_shp_crop_2_data$color)) +
  geom_path(colour = "black") + 
  borders(database="world", regions="canada", colour="black", linetype="dashed")+
  xlim(-170, -50) + ylim(40, 85)





feow_shp_marble<-feow_shp_crop_2
feow_shp_redswamp<-feow_shp_crop_2
feow_shp_whiteriver<-feow_shp_crop_2
feow_shp_rusty<-feow_shp_crop_2
feow_shp_spinycheek<-feow_shp_crop_2
feow_shp_virile<-feow_shp_crop_2
feow_shp_signal<-feow_shp_crop_2
feow_shp_allegheny<-feow_shp_crop_2

##Marble
scores_combined_marble<-scores_combined %>% filter(species=="marble")

feow_shp_marble@data<-merge(feow_shp_marble@data, scores_combined_marble[,c("species", "region")], by.x=c("FEOW_ID"), by.y=c("region"))
feow_shp_marble <- gBuffer(feow_shp_marble, byid=TRUE, width=0)
feow_shp_marble_data <- fortify(feow_shp_marble, region = "FEOW_ID")
feow_shp_marble_data <- merge(feow_shp_marble_data, feow_shp_marble@data[, c('FEOW_ID', 'CMIST_Score')], by.x='id', by.y='FEOW_ID', all.x=TRUE)

marble_score_plot<- ggplot(feow_shp_marble_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = CMIST_Score)) +
  geom_path(colour = "black") + 
  borders(database="world", regions="canada", colour="black", linetype="dashed")+
  xlim(-170, -50) + ylim(40, 85) + 
  ggtitle("marble") + theme(plot.title = element_text(vjust = - 10, hjust=0.1, size=20))

marble_score_plot 

##redswamp
scores_combined_redswamp<-scores_combined %>% filter(species=="redswamp")

feow_shp_redswamp@data<-merge(feow_shp_redswamp@data, scores_combined_redswamp[,c("species", "region")], by.x=c("FEOW_ID"), by.y=c("region"))
feow_shp_redswamp <- gBuffer(feow_shp_redswamp, byid=TRUE, width=0)
feow_shp_redswamp_data <- fortify(feow_shp_redswamp, region = "FEOW_ID")
feow_shp_redswamp_data <- merge(feow_shp_redswamp_data, feow_shp_redswamp@data[, c('FEOW_ID', 'CMIST_Score')], by.x='id', by.y='FEOW_ID', all.x=TRUE)

redswamp_score_plot<- ggplot(feow_shp_redswamp_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = CMIST_Score)) +
  geom_path(colour = "black") + 
  borders(database="world", regions="canada", colour="black", linetype="dashed")+
  xlim(-170, -50) + ylim(40, 85) + 
  ggtitle("redswamp") + theme(plot.title = element_text(vjust = - 10, hjust=0.1, size=20))

library(patchwork)


marble_score_plot + redswamp_score_plot  + scale_fill_viridis(name="CMIST_Score") + plot_layout(ncol = 2) 

##whiteriver
scores_combined_whiteriver<-scores_combined %>% filter(species=="whiteriver")

feow_shp_whiteriver@data<-merge(feow_shp_whiteriver@data, scores_combined_whiteriver[,c("species", "region")], by.x=c("FEOW_ID"), by.y=c("region"))
feow_shp_whiteriver <- gBuffer(feow_shp_whiteriver, byid=TRUE, width=0)
feow_shp_whiteriver_data <- fortify(feow_shp_whiteriver, region = "FEOW_ID")
feow_shp_whiteriver_data <- merge(feow_shp_whiteriver_data, feow_shp_whiteriver@data[, c('FEOW_ID', 'CMIST_Score')], by.x='id', by.y='FEOW_ID', all.x=TRUE)

whiteriver_score_plot<- ggplot(feow_shp_whiteriver_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = CMIST_Score)) +
  geom_path(colour = "black") + 
  scale_fill_viridis(name="CMIST_Score")+
  borders(database="world", regions="canada", colour="black", linetype="dashed")+
  xlim(-170, -50) + ylim(40, 85) + 
  ggtitle("whiteriver") + theme(plot.title = element_text(vjust = - 10, hjust=0.1, size=20))

whiteriver_score_plot 



