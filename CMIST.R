devtools::install_github("https://github.com/remi-daigle/CMISTR")
library(CMISTR)
set.seed(11)
risks <- sample(x = c(1:3),size = 17,replace = TRUE)
mat_risks<- matrix(risks, nrow=2, ncol=17, byrow=TRUE)
uncertainties <- sample(x = c(1:3),size = 17,replace = TRUE)
mat_uncertainties<- matrix(uncertainties , nrow=2, ncol=17, byrow=TRUE)



score <- CMISTScore(risks,uncertainties)
score
colnames(df <- data.frame(matrix(ncol = 3, nrow = 0)))
str(score)
#runs

risks
length(risks)
length()

score_mat <- CMISTScore(mat_risks,mat_uncertainties)
score_mat
#does not work with a matrix. Only 17 values. 


# Create the loop with r and c to iterate over the matrix and add scores 
    
for (r in 1:nrow(mat_risks))   
    new<-CMISTScore(mat_risks[r,],mat_uncertainties[r,])
    score[nrow(score)+1, ]<-new


colnames(score)

library(ggplot2)
theme_set(theme_bw())

ggplot(score, aes(y=CMIST_Score, x=rownames(score))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


### marble

marble_risk<-read.csv("C:Inputs//marble_risk.csv")
marble_risk
rownames(marble_risk)<-marble_risk$region
marble_risk_simple<-marble_risk[,-c(1:3)]
marble_risk_simple
marble_risk_simple_1_3<-marble_risk_simple+1
marble_risk_simple_1_3
colnames(marble_risk_simple_1_3)<-NULL
rownames(marble_risk_simple_1_3)<-NULL
marble_risk_simple_1_3
dim(marble_risk_simple_1_3)

marble_uncertainties<-read.csv("C:Inputs//marble_certainty_321.csv")
rownames(marble_uncertainties)<-marble_uncertainties$ID
marble_uncertainties_simple<-marble_uncertainties[,-c(1:3)]
marble_uncertainties_simple_1_3<-marble_uncertainties_simple
colnames(marble_uncertainties_simple_1_3)<-NULL
rownames(marble_uncertainties_simple_1_3)<-NULL
marble_uncertainties_simple_1_3


score_marble <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
    new_marble<-CMISTScore(as.numeric(marble_risk_simple_1_3[r,]),as.numeric(marble_uncertainties_simple_1_3[r,]))
    score_marble[nrow(score_marble)+1, ]<-new_marble
    }

    
score_marble    
rownames(score_marble)<-rownames(marble_risk_simple)
colnames(score_marble)<-colnames(score)

score_marble_id<-score_marble
score_marble_id$region<-rownames(score_marble_id)
score_marble_id$species<-"marble"
score_marble_id

ggplot(score_marble, aes(y=CMIST_Score, x=rownames(score_marble))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


############### redswamp
redswamp_risk<-read.csv("C:Inputs//redswamp_risk.csv")
redswamp_risk
rownames(redswamp_risk)<-redswamp_risk$region
redswamp_risk_simple<-redswamp_risk[,-c(1:3)]
redswamp_risk_simple
redswamp_risk_simple_1_3<-redswamp_risk_simple+1
redswamp_risk_simple_1_3
colnames(redswamp_risk_simple_1_3)<-NULL
rownames(redswamp_risk_simple_1_3)<-NULL
redswamp_risk_simple_1_3
dim(redswamp_risk_simple_1_3)

redswamp_uncertainties<-read.csv("C:Inputs//redswamp_certainty_321.csv")
rownames(redswamp_uncertainties)<-redswamp_uncertainties$ID
redswamp_uncertainties_simple<-redswamp_uncertainties[,-c(1:3)]
redswamp_uncertainties_simple_1_3<-redswamp_uncertainties_simple
colnames(redswamp_uncertainties_simple_1_3)<-NULL
rownames(redswamp_uncertainties_simple_1_3)<-NULL
redswamp_uncertainties_simple_1_3


score_redswamp <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_redswamp<-CMISTScore(as.numeric(redswamp_risk_simple_1_3[r,]),as.numeric(redswamp_uncertainties_simple_1_3[r,]))
  score_redswamp[nrow(score_redswamp)+1, ]<-new_redswamp
}


score_redswamp    
rownames(score_redswamp)<-rownames(redswamp_risk_simple)
colnames(score_redswamp)<-colnames(score)

score_redswamp_id<-score_redswamp
score_redswamp_id$region<-rownames(score_redswamp_id)
score_redswamp_id$species<-"redswamp"
score_redswamp_id

ggplot(score_redswamp, aes(y=CMIST_Score, x=rownames(score_redswamp))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

######## whiteriver
whiteriver_risk<-read.csv("C:Inputs//whiteriver_risk.csv")
whiteriver_risk
rownames(whiteriver_risk)<-whiteriver_risk$region
whiteriver_risk_simple<-whiteriver_risk[,-c(1:3)]
whiteriver_risk_simple
whiteriver_risk_simple_1_3<-whiteriver_risk_simple+1
whiteriver_risk_simple_1_3
colnames(whiteriver_risk_simple_1_3)<-NULL
rownames(whiteriver_risk_simple_1_3)<-NULL
whiteriver_risk_simple_1_3
dim(whiteriver_risk_simple_1_3)

whiteriver_uncertainties<-read.csv("C:Inputs//whiteriver_certainty_321.csv")
rownames(whiteriver_uncertainties)<-whiteriver_uncertainties$ID
whiteriver_uncertainties_simple<-whiteriver_uncertainties[,-c(1:3)]
whiteriver_uncertainties_simple_1_3<-whiteriver_uncertainties_simple
colnames(whiteriver_uncertainties_simple_1_3)<-NULL
rownames(whiteriver_uncertainties_simple_1_3)<-NULL
whiteriver_uncertainties_simple_1_3


score_whiteriver <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_whiteriver<-CMISTScore(as.numeric(whiteriver_risk_simple_1_3[r,]),as.numeric(whiteriver_uncertainties_simple_1_3[r,]))
  score_whiteriver[nrow(score_whiteriver)+1, ]<-new_whiteriver
}


score_whiteriver    
rownames(score_whiteriver)<-rownames(whiteriver_risk_simple)
colnames(score_whiteriver)<-colnames(score)

score_whiteriver_id<-score_whiteriver
score_whiteriver_id$region<-rownames(score_whiteriver_id)
score_whiteriver_id$species<-"whiteriver"
score_whiteriver_id

ggplot(score_whiteriver, aes(y=CMIST_Score, x=rownames(score_whiteriver))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


############### rusty
rusty_risk<-read.csv("C:Inputs//rusty_risk.csv")
rusty_risk
rownames(rusty_risk)<-rusty_risk$region
rusty_risk_simple<-rusty_risk[,-c(1:3)]
rusty_risk_simple
rusty_risk_simple_1_3<-rusty_risk_simple+1
rusty_risk_simple_1_3
colnames(rusty_risk_simple_1_3)<-NULL
rownames(rusty_risk_simple_1_3)<-NULL
rusty_risk_simple_1_3
dim(rusty_risk_simple_1_3)

rusty_uncertainties<-read.csv("C:Inputs//rusty_certainty_321.csv")
rownames(rusty_uncertainties)<-rusty_uncertainties$ID
rusty_uncertainties_simple<-rusty_uncertainties[,-c(1:3)]
rusty_uncertainties_simple_1_3<-rusty_uncertainties_simple
colnames(rusty_uncertainties_simple_1_3)<-NULL
rownames(rusty_uncertainties_simple_1_3)<-NULL
rusty_uncertainties_simple_1_3


score_rusty <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_rusty<-CMISTScore(as.numeric(rusty_risk_simple_1_3[r,]),as.numeric(rusty_uncertainties_simple_1_3[r,]))
  score_rusty[nrow(score_rusty)+1, ]<-new_rusty
}


score_rusty    
rownames(score_rusty)<-rownames(rusty_risk_simple)
colnames(score_rusty)<-colnames(score)

score_rusty_id<-score_rusty
score_rusty_id$region<-rownames(score_rusty_id)
score_rusty_id$species<-"rusty"
score_rusty_id

ggplot(score_rusty, aes(y=CMIST_Score, x=rownames(score_rusty))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()



############### spinycheek
spinycheek_risk<-read.csv("C:Inputs//spinycheek_risk.csv")
spinycheek_risk
rownames(spinycheek_risk)<-spinycheek_risk$region
spinycheek_risk_simple<-spinycheek_risk[,-c(1:3)]
spinycheek_risk_simple
spinycheek_risk_simple_1_3<-spinycheek_risk_simple+1
spinycheek_risk_simple_1_3
colnames(spinycheek_risk_simple_1_3)<-NULL
rownames(spinycheek_risk_simple_1_3)<-NULL
spinycheek_risk_simple_1_3
dim(spinycheek_risk_simple_1_3)

spinycheek_uncertainties<-read.csv("C:Inputs//spinycheek_certainty_321.csv")
rownames(spinycheek_uncertainties)<-spinycheek_uncertainties$ID
spinycheek_uncertainties_simple<-spinycheek_uncertainties[,-c(1:3)]
spinycheek_uncertainties_simple_1_3<-spinycheek_uncertainties_simple
colnames(spinycheek_uncertainties_simple_1_3)<-NULL
rownames(spinycheek_uncertainties_simple_1_3)<-NULL
spinycheek_uncertainties_simple_1_3


score_spinycheek <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_spinycheek<-CMISTScore(as.numeric(spinycheek_risk_simple_1_3[r,]),as.numeric(spinycheek_uncertainties_simple_1_3[r,]))
  score_spinycheek[nrow(score_spinycheek)+1, ]<-new_spinycheek
}


score_spinycheek    
rownames(score_spinycheek)<-rownames(spinycheek_risk_simple)
colnames(score_spinycheek)<-colnames(score)

score_spinycheek_id<-score_spinycheek
score_spinycheek_id$region<-rownames(score_spinycheek_id)
score_spinycheek_id$species<-"spinycheek"
score_spinycheek_id

ggplot(score_spinycheek, aes(y=CMIST_Score, x=rownames(score_spinycheek))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

############### virile
virile_risk<-read.csv("C:Inputs//virile_risk.csv")
virile_risk
rownames(virile_risk)<-virile_risk$region
virile_risk_simple<-virile_risk[,-c(1:3)]
virile_risk_simple
virile_risk_simple_1_3<-virile_risk_simple+1
virile_risk_simple_1_3
colnames(virile_risk_simple_1_3)<-NULL
rownames(virile_risk_simple_1_3)<-NULL
virile_risk_simple_1_3
dim(virile_risk_simple_1_3)

virile_uncertainties<-read.csv("C:Inputs//virile_certainty_321.csv")
rownames(virile_uncertainties)<-virile_uncertainties$ID
virile_uncertainties_simple<-virile_uncertainties[,-c(1:3)]
virile_uncertainties_simple_1_3<-virile_uncertainties_simple
colnames(virile_uncertainties_simple_1_3)<-NULL
rownames(virile_uncertainties_simple_1_3)<-NULL
virile_uncertainties_simple_1_3


score_virile <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:16) {
  new_virile<-CMISTScore(as.numeric(virile_risk_simple_1_3[r,]),as.numeric(virile_uncertainties_simple_1_3[r,]))
  score_virile[nrow(score_virile)+1, ]<-new_virile
}


score_virile    
rownames(score_virile)<-rownames(virile_risk_simple)
colnames(score_virile)<-colnames(score)

score_virile_id<-score_virile
score_virile_id$region<-rownames(score_virile_id)
score_virile_id$species<-"virile"
score_virile_id

ggplot(score_virile, aes(y=CMIST_Score, x=rownames(score_virile))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

############### signal
signal_risk<-read.csv("C:Inputs//signal_risk.csv")
signal_risk
rownames(signal_risk)<-signal_risk$region
signal_risk_simple<-signal_risk[,-c(1:3)]
signal_risk_simple
signal_risk_simple_1_3<-signal_risk_simple+1
signal_risk_simple_1_3
colnames(signal_risk_simple_1_3)<-NULL
rownames(signal_risk_simple_1_3)<-NULL
signal_risk_simple_1_3
dim(signal_risk_simple_1_3)

signal_uncertainties<-read.csv("C:Inputs//signal_certainty_321.csv")
rownames(signal_uncertainties)<-signal_uncertainties$ID
signal_uncertainties_simple<-signal_uncertainties[,-c(1:3)]
signal_uncertainties_simple_1_3<-signal_uncertainties_simple
colnames(signal_uncertainties_simple_1_3)<-NULL
rownames(signal_uncertainties_simple_1_3)<-NULL
signal_uncertainties_simple_1_3


score_signal <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:20) {
  new_signal<-CMISTScore(as.numeric(signal_risk_simple_1_3[r,]),as.numeric(signal_uncertainties_simple_1_3[r,]))
  score_signal[nrow(score_signal)+1, ]<-new_signal
}


score_signal    
rownames(score_signal)<-rownames(signal_risk_simple)
colnames(score_signal)<-colnames(score)

score_signal_id<-score_signal
score_signal_id$region<-rownames(score_signal_id)
score_signal_id$species<-"signal"
score_signal_id

ggplot(score_signal, aes(y=CMIST_Score, x=rownames(score_signal))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()

############### allegheny
allegheny_risk<-read.csv("C:Inputs//allegheny_risk.csv")
allegheny_risk
rownames(allegheny_risk)<-allegheny_risk$region
allegheny_risk_simple<-allegheny_risk[,-c(1:3)]
allegheny_risk_simple
allegheny_risk_simple_1_3<-allegheny_risk_simple+1
allegheny_risk_simple_1_3
colnames(allegheny_risk_simple_1_3)<-NULL
rownames(allegheny_risk_simple_1_3)<-NULL
allegheny_risk_simple_1_3
dim(allegheny_risk_simple_1_3)

allegheny_uncertainties<-read.csv("C:Inputs//allegheny_certainty_321.csv")
rownames(allegheny_uncertainties)<-allegheny_uncertainties$ID
allegheny_uncertainties_simple<-allegheny_uncertainties[,-c(1:3)]
allegheny_uncertainties_simple_1_3<-allegheny_uncertainties_simple
colnames(allegheny_uncertainties_simple_1_3)<-NULL
rownames(allegheny_uncertainties_simple_1_3)<-NULL
allegheny_uncertainties_simple_1_3


score_allegheny <- data.frame(matrix(ncol = 9, nrow = 0))

set.seed(11)
for (r in 1:21) {
  new_allegheny<-CMISTScore(as.numeric(allegheny_risk_simple_1_3[r,]),as.numeric(allegheny_uncertainties_simple_1_3[r,]))
  score_allegheny[nrow(score_allegheny)+1, ]<-new_allegheny
}


score_allegheny    
rownames(score_allegheny)<-rownames(allegheny_risk_simple)
colnames(score_allegheny)<-colnames(score)

score_allegheny_id<-score_allegheny
score_allegheny_id$region<-rownames(score_allegheny_id)
score_allegheny_id$species<-"allegheny"
score_allegheny_id

ggplot(score_allegheny, aes(y=CMIST_Score, x=rownames(score_allegheny))) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1) +
  geom_point()


#### combining

scores_combined<-rbind(score_marble_id, score_redswamp_id, score_whiteriver_id, score_rusty_id, score_spinycheek_id, score_virile_id, score_signal_id, score_allegheny_id)
head(scores_combined)

ggplot(scores_combined, aes(y=CMIST_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))


#Likelihood
ggplot(scores_combined, aes(y=Likelihood_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))
#Impact
ggplot(scores_combined, aes(y=Impact_Score, x=region, colour=species)) + 
  geom_errorbar(aes(ymin=Impact_Lower, ymax=Impact_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))


#' scores <- simScore(risk=2,certainty=1)
simScore <- function(risk,certainty,n=1000){
  p <- probs[probs$Risk==risk&probs$Certainty==certainty,]
  sample(x=p$Score, size=n, prob=p$Probability,replace=TRUE)
}


simScore(risk=2,certainty=1)

####
head(scores_combined)
scores_combined_116<-scores_combined[scores_combined$region==116, ]
head(scores_combined_116)

ggplot(scores_combined_116, aes(y=Likelihood_Score, x=Impact_Score, colour=species)) + 
  geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper,xmin=Impact_Lower, xmax=Impact_Upper ), width=.1) +
  geom_point(size=4)+ylim(0,3)+xlim(0,3)

###### Main regions:
scores_combined_ontario<-scores_combined[scores_combined$region==116:118, ]
head(scores_combined_ontario)

ggplot(scores_combined_ontario, aes(y=Likelihood_Score, x=Impact_Score, colour=species, shape=region)) + 
  geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper,xmin=Impact_Lower, xmax=Impact_Upper ), width=.1) +
  geom_point(size=4)+ylim(0,3)+xlim(0,3)


##### BC
scores_combined_103<-scores_combined[scores_combined$region==103, ]
head(scores_combined_103)

ggplot(scores_combined_103, aes(y=Likelihood_Score, x=Impact_Score, colour=species)) + 
  geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper,xmin=Impact_Lower, xmax=Impact_Upper ), width=.1) +
  geom_point(size=4)+ylim(0,3)+xlim(0,3)

#Okanagan
scores_combined_120<-scores_combined[scores_combined$region==120, ]
head(scores_combined_120)

ggplot(scores_combined_120, aes(y=Likelihood_Score, x=Impact_Score, colour=species)) + 
  geom_errorbar(aes(ymin=Likelihood_Lower, ymax=Likelihood_Upper,xmin=Impact_Lower, xmax=Impact_Upper ), width=.1) +
  geom_point(size=4)+ylim(0,3)+xlim(0,3)



#### Mean risk across regions: can't do cumulative since some regions don't 
library(tidyverse)
scores_combined_cumulative<-scores_combined %>% group_by(region) %>% summarise_if(is.numeric, mean, na.rm=TRUE) 

View(scores_combined)

ggplot(scores_combined_cumulative, aes(y=CMIST_Score, x=region, colour=region)) + 
  geom_errorbar(aes(ymin=CMIST_Lower, ymax=CMIST_Upper), width=.1, position=position_dodge(width=0.5)) +
  geom_point(size=4, position=position_dodge(width=0.5))

###### Mapping
library(ncdf4)
library(reshape2)
library(dplyr)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(maptools)


library(here)
library(sf)
library(raster)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)
library(rgdal) # spatial/shp reading
library(viridis) # nice color palette
library(ggmap) # ggplot functionality for maps ---> dplyr, purr is dependency
library(ggsn) # for scale bars/north arrows in ggplots
library(maps)
library(mapdata)
library(sp)
library(rgeos)

# once way to import the shape file
#feow_shp<-sf::st_read("C:Inputs//feow_hydrosheds.shp")

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

plot(feow_shp_crop_2)

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



