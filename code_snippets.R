
#' #' scores <- simScore(risk=2,certainty=1)
#' simScore <- function(risk,certainty,n=1000){
#'   p <- probs[probs$Risk==risk&probs$Certainty==certainty,]
#'   sample(x=p$Score, size=n, prob=p$Probability,replace=TRUE)
#' }


# simScore(risk=2,certainty=1)

mat_risks<- matrix(risks, nrow=2, ncol=17, byrow=TRUE)
mat_uncertainties<- matrix(uncertainties , nrow=2, ncol=17, byrow=TRUE)
for (r in 1:nrow(mat_risks))   
  new<-CMISTScore(mat_risks[r,],mat_uncertainties[r,])
score[nrow(score)+1, ]<-new


colnames(score)


# pattern_fill<-scale_pattern_manual(values = c("NA" = "stripe"))
# pattern.list2<-c("stripe","none")

# + theme(plot.title = element_text(vjust = - 10, hjust=0.1, size=16))



# beypal <- c("allegheny"="#D42D24", "marble"= "#211DF0","redswamp" = "#34DD9C","rusty"= "#80CCDF", 
"signal"="#FCEA1B","virile" = "#5D36B1","spinycheek"="#02A0A2","whiteriver"="#E74690")

# cbPalette<-c("allegheny"="#999999", "marble"="#E69F00", "redswamp"="#56B4E9", "rusty"="#009E73","signal"= "#F0E442", 
"virile"="#0072B2","spinycheek" = "#D55E00", "whiteriver"="#CC79A7")



##### identification map of which region 
# This doesn't really work all that well so leave out... 

region_id_plots = list()
feow_sf_regions=list()

for(region_ in regions) {
  feow_sf_regions[[region_]]<-feow_sf_crop_2 %>% filter(FEOW_ID==region_)
  region_id_plots[[region_]] = ggplot(feow_sf_crop_2) + geom_sf(data= feow_sf_crop_2, fill = "grey")+
    geom_sf(data= feow_sf_regions[[region_]], fill="red")+
    borders(database="world", regions="canada", colour="black", linetype="dashed")+
    xlim(-170, -50) + ylim(40, 85)+ 
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
}

region_id_plots[["116"]] 

library(patchwork)
biplot_map_plots<-list()

for(region_ in regions) {
  biplot_map_plots[[region_]] = region_plots[[region_]] + inset_element(region_id_plots[[region_]], 0, 0, .8, .8, align_to='panel')
} 

allregions_biplot_map <-  wrap_plots(biplot_map_plots, ncol = 7) + plot_layout(guides = 'collect') 
allregions_biplot_map
ggsave(allregions_biplot_map, file="Plots/allregions_biplot_map.png", dpi=300)

#trying to find the right size
region_plots[["116"]] + inset_element(region_id_plots[["116"]], 0, 0, .8, .8, align_to='panel')











