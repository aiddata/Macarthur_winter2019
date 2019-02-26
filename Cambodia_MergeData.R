
######################
# GIE of Chinese Infrastructure Investments in Cambodia 2.0
# Work originally done for MacArthur Foundation, redoing with geocoding 2.0 data
# Outcome: Forest Loss using 30m Hansen cells aggregated to 5km LTDR, cumulative share of Hansen cells experiencing forest loss
#######################

library(sf)



## Set working directory
setwd("/Users/rbtrichler/Box Sync/Macarthur_winter2019")

# setwd("~/Box Sync/Macarthur_winter2019")

##---------
## Read in Cell Level Data
#  all of these extracts are from the original analysis
# copied relevant extract files into "Macarthur_winter2019" Box Sync but can also be found in "Macarthur" Box Sync
##---------

## Load the cell dataframe
# provides IDs and coordinates for all SEA countries in original analysis
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

## Merge in additional covars produced in separate extracts
# Merge in ntl
ntl<-read.csv("ntl_extracts/merge_sea_grid2.csv")
colnames(ntl)<-gsub("v4composites_calibrated_201709.","ntl_",colnames(ntl))
colnames(ntl)<-gsub(".mean","",colnames(ntl))
# create time range trends for 2009-2013 to impute 2014 ntl data
# first create ntl panel dataset and then can do time range trends 

ntl_0913 <- ntl[,grepl(paste(c("ID", 2009:2013), collapse = "|"), names(ntl))]
ntl_cols<-grep("ntl",names(ntl_0913))

ntl_reshape <- c(ntl_cols)
ntl_0913panel <- reshape(ntl_0913, varying=ntl_cols, direction="long",idvar="ID",sep="_",timevar="year")

#create 2009-2013 trend, then will use to impute 2014 value

# ## Create pre-trend using panel dataset
# 
# obj <- ndvi_pre_panel %>% split(.$reu_id) %>% lapply (lm, formula=formula(ndvi~qtr))
# 
# obj_coefficients <- as.data.frame(t(lapply(obj, function(x) as.numeric(x[1]$coefficients[2]))))
# obj_coefficients1<-as.data.frame(t(obj_coefficients))
# obj_coefficients1$rownumber <- as.numeric(rownames(obj_coefficients1))
# obj_coeff<-obj_coefficients1
# names(obj_coeff)[names(obj_coeff)=="V1"]="ndvipre_0612"
# names(obj_coeff)[names(obj_coeff)=="rownumber"]="reu_id"
# obj_coeff$ndvipre_0612<-as.numeric(obj_coeff$ndvipre_0612)



#

# merge the main extract with NTL, removing all non-Cambodia NTL observations
geo <- merge(camb_cells, ntl, by="ID", all.x=T)





