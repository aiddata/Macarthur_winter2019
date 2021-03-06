
######################
# GIE of Chinese Infrastructure Investments in Cambodia 2.0
# Work originally done for MacArthur Foundation, redoing with geocoding 2.0 data
# Outcome: Forest Loss using 30m Hansen cells aggregated to 5km LTDR, cumulative share of Hansen cells experiencing forest loss
#######################

library(sf); library(raster); library(sp); library(spatialEco); library(geosphere); library(foreign)



## Set working directory
setwd("/Users/rbtrichler/Box Sync/Macarthur_winter2019")
# setwd("~/Box Sync/Macarthur_winter2019")

##---------
## Read in Cell Level Data
#  all of these extracts are from the original analysis
# copied relevant extract files into "Macarthur_winter2019" Box Sync but can also be found in "Macarthur" Box Sync
##---------

## Load the cell dataframe
# provides IDs and coordinates of grid cells for all Southeast Asia (SEA) countries in original analysis
# use this if need to output location information
cells<- "grids/sea_grid.shp"
cells<-st_read(cells)

## Load the cell extracts and subset to Cambodia
# for all SEA countries in original analysis
extract<-read.csv("extracts/sea.csv")
# subset extract to Cambodia
extract2<-extract[extract$NAME_0=="Cambodia",]
# drop INCORRECT ntl vars, to be replaced later
extract3<-extract2[,-grep("ncc4_", colnames(extract2))]

camb_cells <- extract3

#Reorder var names in camb_cells so can rename to something obvious
#for temperature
for (i in 2:length(camb_cells)) {
  
  if (substr(colnames(camb_cells)[i], 1, 4) == "at41"){
    
    name = substr(colnames(camb_cells)[i],1, 4)
    year = substr(colnames(camb_cells)[i], 6, 9)
    letter = substr(colnames(camb_cells)[i], 10,10)
    dt = paste(letter,name,"_",year,sep="")
    colnames(camb_cells)[i] <- dt
  }
}
#for precip
for (i in 2:length(camb_cells)) {
  
  if (substr(colnames(camb_cells)[i], 1, 4) == "pc41"){
    
    name = substr(colnames(camb_cells)[i],1, 4)
    year = substr(colnames(camb_cells)[i], 6, 9)
    letter = substr(colnames(camb_cells)[i], 10,10)
    dt = paste(letter,name,"_",year,sep="")
    colnames(camb_cells)[i] <- dt
  }
}
#for ndvi
for (i in 2:length(camb_cells)) {
  
  if (substr(colnames(camb_cells)[i], 1, 4) == "lnyx"){
    
    name = substr(colnames(camb_cells)[i],1, 4)
    year = substr(colnames(camb_cells)[i], 6, 9)
    letter = substr(colnames(camb_cells)[i], 10,10)
    dt = paste(letter,name,"_",year,sep="")
    colnames(camb_cells)[i] <- dt
  }
}

#Rename variables in camb_cells so make sense
names(camb_cells)[names(camb_cells)=="ID_0"] = "ID_ADM0"
names(camb_cells)[names(camb_cells)=="ID_1"] = "ID_ADM1"
names(camb_cells)[names(camb_cells)=="ID_2"] = "ID_ADM2"
names(camb_cells)[names(camb_cells)=="selv_e"] = "elevation"
names(camb_cells)[names(camb_cells)=="sslp_e"] = "slope"
names(camb_cells)[names(camb_cells)=="dari_e"] = "rivdist"
names(camb_cells)[names(camb_cells)=="droa_e"] = "roaddist"
names(camb_cells)[names(camb_cells)=="am50_e"] = "urbtravtime"

colnames(camb_cells)<-gsub("elnyx","ndvi",colnames(camb_cells))
colnames(camb_cells)<-gsub("mat41","mintemp",colnames(camb_cells))
colnames(camb_cells)<-gsub("xat41","maxtemp",colnames(camb_cells))
colnames(camb_cells)<-gsub("eat41","meantemp",colnames(camb_cells))
colnames(camb_cells)<-gsub("mpc41","minprecip",colnames(camb_cells))
colnames(camb_cells)<-gsub("xpc41","maxprecip",colnames(camb_cells))
colnames(camb_cells)<-gsub("epc41","meanprecip",colnames(camb_cells))

## Create NDVI pre-trend
# years 1990-2000
#subset to ID and NDVI for years 1990 to 2000
ndvi_9000<-camb_cells[c("ID", grep(paste0("ndvi_", 1990:2000, collapse="|"), names(camb_cells), value=T))]
ndvi_9000<-ndvi_9000[,order(names(ndvi_9000))]
ndvi<-grep("ndvi",names(ndvi_9000))

ndvi_reshape<-c(ndvi)
ndvi_9000panel<-reshape(ndvi_9000,varying=ndvi_reshape, direction="long",idvar="ID",sep="_",timevar="year")

#create pre-trends for 1990 to 2000

obj_ndvi <- ndvi_9000panel %>% split(.$ID) %>% lapply (lm, formula=formula(ndvi~year))
#extract one trend value for each cell ID
obj_coefficients_ndvi <- as.data.frame(t(lapply(obj_ndvi, function(x) as.numeric(x[1]$coefficients[2]))))
obj_coeff_ndvi<-as.data.frame(t(obj_coefficients_ndvi))
obj_coeff_ndvi$rownumber <- as.numeric(rownames(obj_coeff_ndvi))

#rename columns to trend and cell id
names(obj_coeff_ndvi)[names(obj_coeff_ndvi)=="V1"]="ndvi_pretrend"
names(obj_coeff_ndvi)[names(obj_coeff_ndvi)=="rownumber"]="ID"
obj_coeff_ndvi$ndvi_pretrend<-as.numeric(obj_coeff_ndvi$ndvi_pretrend)

#merge
camb_cells<-merge(camb_cells, obj_coeff_ndvi)

# ----------------
## Merge in NTL and create pre-trends
# ----------------

## Merge in ntl and create time range trends for pre-trends and to impute 2014 value
# Read in NTL
ntl<-read.csv("ntl_extracts/merge_sea_grid2.csv")
colnames(ntl)<-gsub("v4composites_calibrated_201709.","ntl_",colnames(ntl))
colnames(ntl)<-gsub(".mean","",colnames(ntl))

# create time range trends for 2009-2013 to impute 2014 ntl data
# first create ntl panel dataset for 2009 to 2013 and then can do time range trends 
ntl_order<-ntl[,order(names(ntl))]
ntl_0913<-ntl_order[c("ID", grep(paste0("ntl_", 2009:2013, collapse="|"), names(ntl_order), value=T))]
ntl<-grep("ntl",names(ntl_0913))

ntl_reshape <- c(ntl)
ntl_0913panel <- reshape(ntl_0913, varying=ntl_reshape, direction="long",idvar="ID",sep="_",timevar="year")

#create 2009-2013 trend, then will use to impute 2014 value
# fine since only using it as a covariate, need it to extend full time range of dataset
# create 2009-2013 trend
obj <- ntl_0913panel %>% split(.$ID) %>% lapply (lm, formula=formula(ntl~year))
#extract one trend value for each cell ID
# will add this coefficient (slope) to 2013 value to impute 2014 ntl value
obj_coefficients <- as.data.frame(t(lapply(obj, function(x) as.numeric(x[1]$coefficients[2]))))
obj_coefficients1<-as.data.frame(t(obj_coefficients))
obj_coefficients1$rownumber <- as.numeric(rownames(obj_coefficients1))
obj_coeff<-obj_coefficients1
#rename columns to trend and cell id
names(obj_coeff)[names(obj_coeff)=="V1"]="ntltrend_0913"
names(obj_coeff)[names(obj_coeff)=="rownumber"]="ID"
obj_coeff$ntltrend_0913<-as.numeric(obj_coeff$ntltrend_0913)

# create ntl_pretrend for years 1992-2000
ntl_9200<-ntl_order[c("ID", grep(paste0("ntl_", 1992:2000, collapse="|"), names(ntl_order), value=T))]
ntl_pre<-grep("ntl",names(ntl_9200))

ntl_reshape_pre <- c(ntl_pre)
ntl_9200panel <- reshape(ntl_9200, varying=ntl_reshape_pre, direction="long",idvar="ID",sep="_",timevar="year")

#create pre-trends for 1992-2000 to interact in analysis models
obj_pre <- ntl_9200panel %>% split(.$ID) %>% lapply (lm, formula=formula(ntl~year))
#extract one trend value for each cell ID
obj_coefficients_pre <- as.data.frame(t(lapply(obj_pre, function(x) as.numeric(x[1]$coefficients[2]))))
obj_coefficients1_pre<-as.data.frame(t(obj_coefficients_pre))
obj_coefficients1_pre$rownumber <- as.numeric(rownames(obj_coefficients1_pre))
obj_coeff_pre<-obj_coefficients1_pre
#rename columns to trend and cell id
names(obj_coeff_pre)[names(obj_coeff_pre)=="V1"]="ntlpretrend_9200"
names(obj_coeff_pre)[names(obj_coeff_pre)=="rownumber"]="ID"
obj_coeff_pre$ntlpretrend_9200<-as.numeric(obj_coeff_pre$ntlpretrend_9200)

#merge 2009-2013 trend values back into ntl cross-section
ntl_all <- merge(ntl_order, obj_coeff, by="ID")
#create ntl_2014 using trend values
ntl_all$ntl_2014_imp<-ntl_all$ntl_2013 + ntl_all$ntltrend_0913
#set min value for ntl_2014 to 0
ntl_all$ntl_2014<-ifelse(ntl_all$ntl_2014_imp<0, 0, ntl_all$ntl_2014_imp)
#summary(ntl_all$ntl_2014_imp)
#summary(ntl_all$ntl_2014)

#merge ntl pre-trends back into ntl cross-section
ntl_all1<-merge(ntl_all, obj_coeff_pre, by="ID")
#summary(ntl_all1$ntlpretrend_9200)
ntl_all<-ntl_all1
#drop ntl_2014_imp and merge ntl back into camb_cells
ntl_all<-ntl_all[,!names(ntl_all) %in% c("ntl_2014_imp")]

#merge 
camb_cells1<-merge(camb_cells, ntl_all, by="ID")
camb_cells<-camb_cells1

#---------------
# Add in GPW4 Data, Protected Areas, concessions, plantation data
#---------------

## Protected Areas
#WDPA
#read in data
pa_2000 <- read.csv("ProtectedAreas_Data/merge_sea_grid_pre2001.csv")
#Create new column with percentage of cell covered by protected area
pa_2000$wdpapct_2000 <- NA
pa_2000$wdpapct_2000 <- pa_2000$wdpa_pre2001_sea.na.sum/pa_2000$wdpa_pre2001_sea.na.count
#drop out sum and count calculations to leave only percent covered by protected area
pa_2000<-pa_2000[grepl("ID|wdpapct_2000", names(pa_2000))]
#merge into camb_cells
camb_covars<-merge(camb_cells, pa_2000, by="ID")

## ODC concessions data
# Open Development Cambodia
# the percentage of cell coverage calculated below is for the full concessions dataset
# it is possible to subset the dataset for concessions that were granted before Chinese investments (pre-2001)
# the "concessions_subset" sum and count included in the dataset below are for concessions pre-2004, not 2001
con <- read.csv("ODCConcessions/merge_sea_grid.csv")
#create percentage of cell covered by all concessions in dataset
con$concessionpct_all<-NA
con$concessionpct_all<- con$concessions.na.sum/con$concessions.na.count
#drop unused vars and merge with camb_covars
con <- con[,-grep("(na)", names(con))]
camb_covars<-merge(camb_covars, con, by="ID")

## Plantations Data
# Global Forest Watch, 2013-2014
gfw <- read.csv("GFWPlantation/merge_sea_grid.csv")
#create percentage of cell covered by all plantations in dataset (121 is the max number of grids in a 5km cell)
gfw$plantation_pct<-NA
gfw$plantation_pct<-gfw$gfw_plantations_sea.na.sum/121
#drop unused vars and merge with camb_covars
gfw <- gfw[,-grep("(sea)",names(gfw))]
camb_covars<-merge(camb_covars, gfw, by="ID")

## GPW Population Data
pop <- read.csv("GPW4_Extracts/merge_sea_grid.csv")
colnames(pop)<-gsub("v4_density.","",colnames(pop))
colnames(pop)<-gsub(".mean","",colnames(pop))
# in original analysis, used values from 2000 for 2000-2004, 2005 for 2005-2009, etc. (did not impute, just copied)
# did not impute, just copied values for 5 years
for (i in 2001:2004)
{
  pop[[paste0("gpw_",i)]]<-pop$gpw_2000
}

for (i in 2006:2009)
{
  pop[[paste0("gpw_",i)]]<-pop$gpw_2005
}

for (i in 2011:2014)
{
  pop[[paste0("gpw_",i)]]<-pop$gpw_2010
}

#reorder and merge into camb_covars
pop<-pop[,order(names(pop))]
camb_covars<-merge(camb_covars, pop)

#Write to file
# write.csv(camb_covars,"processed_data/CambodiaCovars_cross.csv", row.names=F)


### Treatment Data ###

# merge geometry back into grid cell dataset
cells2 <- merge(camb_covars, cells, by="ID", all.x=T)
rownames(cells2) <- c(1:nrow(cells2))
grid <- as_Spatial(cells2$geometry, IDs=as.character(c(1:nrow(cells2))))
grid <- SpatialPolygonsDataFrame(Sr=grid, data = cells2)

# find midpoint coords of each grid cell
grid_df <- as.data.frame(grid)
grid_df$midpoint <- sapply(grid_df$geometry, FUN = function(x) list(centroid(matrix(unlist(x), ncol = 2))))
grid_df$lat <- sapply(grid_df$midpoint, FUN = function(x) x[,"lat"])
grid_df$lon <- sapply(grid_df$midpoint, FUN = function(x) x[,"lon"])

# read in road data
roads <- st_read("geocodeddata_dec2018/MacCambodia_Lines_SubsetAccurate.geojson", stringsAsFactors=F)

# identify grid cells intersecting with road projects
intersection <- as.data.frame(point.in.poly(roads, grid))
intersection <- merge(intersection, cells, by="ID", all.x=T)
names(intersection)[names(intersection)=="id"] <- "road_id"
names(intersection)[names(intersection)=="ID"] <- "cell_id"

# midpoints of grid cells intersecting w/ roads
intersection$midpoint <- sapply(intersection$geometry, FUN = function(x) list(centroid(matrix(unlist(x), ncol = 2))))
intersection$lat <- sapply(intersection$midpoint, FUN = function(x) x[,"lat"])
intersection$lon <- sapply(intersection$midpoint, FUN = function(x) x[,"lon"])

# subset grid to 10+% forested areas
grid_df <- grid_df[which(grid_df$tc00_e>=10),]

# create skeleton for grid cell "distance to road" matrix
dist <- matrix(data = NA, nrow = nrow(grid_df), ncol = length(unique(intersection$road_id))+1)
colnames(dist) <- c("cell", unique(roads$id))
dist[,1] <- sort(grid_df$ID)

# filling the distance matrix with minimum distance from each grid cell to each road project
for(i in colnames(dist)[2:ncol(dist)]) {
  # midpoints for each grid intersecting road project i
  roadCoords <- intersection[which(intersection$road_id==i), c("lat", "lon")]
  
  dist[,i] <- sapply(sort(grid_df$ID),
                     FUN = function(x) {
                       # identify midpoint of grid cell x
                       point <- grid_df[which(grid_df$ID==x), c("lat", "lon")]

                       # identify the minimum distance from midpoint of grid cell x to a midpoint of 
                       # road project i
                       minDist <- min(distHaversine(point[c("lon", "lat")], roadCoords[c("lon", "lat")]))/1000

                       return(minDist)

                     })
}

# subset dataset to grid cells w/in 121km of a road project
# dist <- dist[which(apply(dist[,-1], 1, function(x) {min(x)})<=121),]

###

# load in correlogram data
load(file = "/Users/christianbaehr/GitHub/MacArthur/modelData/cambodia_correl.RData")
correl <- do.call(cbind, correlogram_data[c("mean.of.class", "correlation")])

test.correl <- as.data.frame(correl)
test.mod <- lm(correlation ~ log(mean.of.class), data = test.correl)
plot(test.correl$mean.of.class, test.correl$correlation)
xvec <- seq(min(test.correl$mean.of.class), max(test.correl$mean.of.class), by=0.01)
logPred <- predict(test.mod, newdata=data.frame(mean.of.class=xvec))
lines(xvec, logPred, col="blue", lwd=3)

trimmed.correl <- as.data.frame(correl[correl[,1] <= 121,])
test.mod <- lm(correlation ~ log(mean.of.class), data = trimmed.correl)
xvec <- seq(min(test.correl$mean.of.class), max(test.correl$mean.of.class), by=0.01)
logPred <- predict(test.mod, newdata=data.frame(mean.of.class=xvec))
lines(xvec, logPred, col="red", lwd=3)
abline(v=121)

# remove NAs from roads year variable
years <- na.omit(roads$year)

# build skeleton treatment matrix
treatment <- matrix(data = NA, nrow = nrow(dist), ncol = length(seq(min(years), max(years), 1))+1)
colnames(treatment) <- c("cell_id", seq(min(years), max(years), 1))
# fill first treatment column with cell IDs
treatment[,1] <- dist[,1]

# fill treatment matrix
for(i in treatment[,1]) {
  for(j in seq(min(years),max(years),1)) {
    
    # select IDs of roads completed during or before year j
    tempRoads <- roads$id[which(roads$year<=j)]
    # select the distance from cell i to each road completed by year j
    tempDist <- dist[dist[,1]==i, colnames(dist) %in% tempRoads]
    
    x <- NULL
    for(k in tempDist) {
      
      y <- correl[which.min(abs(k-correl[,1])), ]
      
      if(y[1]>121) {
        x[length(x)+1] <- 0
      } else {
        # find the correlogram value nearest to each tempDist value
        x[length(x)+1] <- y[2]
        
      }
      
    }
    
    # sum the correlogram values for cell i relative to all roads completed before year j
    treatment[treatment[,1]==i, colnames(treatment)==as.character(j)] <- sum(x)
    
  }
}

###

# convert treatment to data frame and assign names before merging with covars
treatment <- as.data.frame(treatment)
names(treatment) <- c("ID", paste0("trt_", 2003:2014))

# merge treatment with covars
pre_panel <- merge(grid_df, treatment, by="ID")

# add new variables with missing values for missing years
pre_panel[paste0("trt_", 1980:2002)] <- NA
pre_panel[paste0("ntl_", 1980:1991)] <- NA
pre_panel[paste0("ndvi_", 1980)] <- NA
pre_panel[paste0("gpw_", 1980:1999)] <- NA
pre_panel[paste0("per_loss_", 1980:2000)] <- NA

# reshape long into panel
panel <- reshape(data = pre_panel, direction = "long", idvar = "ID", sep = "_", timevar = "year",
                 varying = list(paste0("ntl_", 1980:2014),
                                paste0("trt_", 1980:2014),
                                paste0("ndvi_", 1980:2014),
                                paste0("mintemp_", 1980:2014),
                                paste0("minprecip_", 1980:2014),
                                paste0("meantemp_", 1980:2014),
                                paste0("meanprecip_", 1980:2014),
                                paste0("maxtemp_", 1980:2014),
                                paste0("maxprecip_", 1980:2014),
                                paste0("gpw_", 1980:2014),
                                paste0("per_loss_", 1980:2014)))

panel$year <- panel$year + 1979

# only keep necessary variables
panel <- panel[c("ID", "NAME_1", "NAME_2", "year", "tc00_e", "rivdist", "roaddist", "elevation", 
                 "slope", "urbtravtime", "gpw3_1990e", "gpw3_1995e", "gpw3_2000e", "ndvi_pretrend",
                 "ntltrend_0913", "ntlpretrend_9200", "wdpapct_2000", "concessionpct_all",
                 "plantation_pct", "ntl_1980", "trt_1980", "ndvi_1980", "mintemp_1980",
                 "minprecip_1980", "meantemp_1980", "meanprecip_1980", "maxtemp_1980",
                 "maxprecip_1980", "gpw_1980", "per_loss_1980")]

# rename variables
names(panel) <- c("cell_id", "prov_name", "dist_name", "year", "tc00_e", "rivdist", "roaddist",
                  "elevation", "slope", "urbtravtime", "gpw3_1990e", "gpw3_1995e", "gpw3_2000e", 
                  "ndvi_pretrend", "ntltrend_0913", "ntlpretrend_9200", "wdpapct_2000", 
                  "concessionpct_all", "plantation_pct", "ntl", "trt", "ndvi", "mintemp",
                  "minprecip", "meantemp", "meanprecip", "maxtemp", "maxprecip", "gpw", "per_loss")

# write data to MacArthur_Winter2019 Box Sync
# write.csv(panel, file = "processed_data/panel.csv", row.names = F)
# write.dta(panel, file = "processed_data/panel.dta")

# test data to check that treatment values are generally higher near road projects
# test <- pre_panel
# test$geometry <- as_Spatial(test$geometry, IDs = as.character(1:nrow(test)))
# test2 <- SpatialPolygonsDataFrame(Sr=test$geometry, data = test)
# library(rgdal)
# writeOGR(obj=test2[!names(test2) %in% c("geometry", "midpoint")], dsn="/Users/christianbaehr/Desktop/test_vis.GeoJSON", layer = "trt_2014", driver = "GeoJSON")
