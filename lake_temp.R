library(ncdf4)
library(reshape2)
library(dplyr)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(maptools)
# install.packages("remotes")
# remotes::install_github("RS-eco/processNC")
library("RS-eco/processNC")
nc<-nc_open("20190715120000-C3S-L3S-LSWT-v4.0-fv01.0.nc")

print(nc)

attributes(nc$var)$names
attributes(nc$dim)$names

lake_temp <- ncvar_get(nc, attributes(nc$var)$names[1])
nc_lat <- ncvar_get( nc, attributes(nc$dim)$names[1])
nc_lon <- ncvar_get( nc, attributes(nc$dim)$names[2])
time<- ncvar_get( nc, attributes(nc$dim)$names[3])

#dimnames(lake_temp) <- list(lon=nc_lon, lat=nc_lat)
dim(lake_temp )
#only one time dimension
fillvalue <- ncatt_get(nc, "lake_surface_water_temperature", "_FillValue")
fillvalue

nc_close(nc)

lake_temp[lake_temp== fillvalue$value] <- NA

lake_temp_slice<-lake_temp[,]


r <- raster(t(lake_temp_slice), xmn=min(nc_lon), xmx=max(nc_lon), ymn=min(nc_lat), ymx=max(nc_lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y') 

# xmn=-140, xmx=-50, ymn=40, ymx=70
plot(r)

plot(r, xlim=c(-140,-50), ylim=c(40,70))

#### feow
feow_shp<-sf::st_read("feow_hydrosheds.shp")
plot(feow_shp, add=T)

lakes_rivs_shp<-sf::st_read("ghy_000c11a_e.shp")
plot(lakes_rivs_shp)


plot(r, xlim=c(-130,-110), ylim=c(45,50))



#### trying to do a whole list of files
process_nc <- function(files){
  # iterate through the nc
  for (i in 1:length(files)){
    # open a conneciton to the ith nc file
    nc_tmp <- nc_open(files[i])
    # store values from variables and atributes
    lake_temp[i] <- ncvar_get(nc, attributes(nc$var)$names[1])
    nc_lat[i] <- ncvar_get( nc, attributes(nc$dim)$names[1])
    nc_lon[i] <- ncvar_get( nc, attributes(nc$dim)$names[2])
    time[i]<- ncvar_get( nc, attributes(nc$dim)$names[3])
    # close the connection sice were finished
    
    nc_close(nc_tmp)
}}

flist= list.files('2019 lake temperature data/',pattern='*.nc',full.names=TRUE)

data <- process_nc(flist)


temp <- tempfile(fileext=".nc")
mergeNC(files=flist, outfile=temp)
raster::stack(temp) 

##### federal water
fed_water<-read.csv("federal_water_quality.csv")
head(fed_water)
View(fed_water)

library(plyr)
####"SITE_NAME_NOM",

cdata <- ddply(fed_water, c( "VARIABLE_NAME", "DRAINAGE_REGION"), summarise,
               N    = length(VALUE_VALEUR),
               mean = mean(VALUE_VALEUR),
               sd   = sd(VALUE_VALEUR),
               se   = sd / sqrt(N),
               max=max(VALUE_VALEUR),
               min=min(VALUE_VALEUR)
)
View(cdata)

cdata_temp <- ddply(fed_water, c( "DRAINAGE_REGION"), summarise,
               N    = length(TEMPERATURE_USED_TEMPERATURE_UTILISEE),
               mean = mean(TEMPERATURE_USED_TEMPERATURE_UTILISEE, na.rm=TRUE),
               sd   = sd(TEMPERATURE_USED_TEMPERATURE_UTILISEE, na.rm=TRUE),
               se   = sd / sqrt(N),
               max=max(TEMPERATURE_USED_TEMPERATURE_UTILISEE, na.rm=TRUE),
               min=min(TEMPERATURE_USED_TEMPERATURE_UTILISEE, na.rm=TRUE)
)
cdata_temp

cdata_pH <- ddply(fed_water, c( "DRAINAGE_REGION"), summarise,
                    N    = length(PH_USED_PH_UTILISE),
                    mean = mean(PH_USED_PH_UTILISE , na.rm=TRUE),
                    sd   = sd(PH_USED_PH_UTILISE, na.rm=TRUE),
                    se   = sd / sqrt(N),
                    max=max(PH_USED_PH_UTILISE, na.rm=TRUE ),
                    min=min(PH_USED_PH_UTILISE, na.rm=TRUE)
)
cdata_pH

