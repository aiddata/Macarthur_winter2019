
######################
# GIE of Chinese Infrastructure Investments in Cambodia 2.0
# Work originally done for MacArthur Foundation, redoing with geocoding 2.0 data
# Outcome: Forest Loss using 30m Hansen cells aggregated to 5km LTDR, cumulative share of Hansen cells experiencing forest loss
#######################

library(sf)



## Set working directory
setwd("/Users/rbtrichler/Box Sync/Macarthur_winter2019")


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
ntl_0913<-ntl_order[,c(1,19:23)]
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
ntl_9200<-ntl_order[,c(1:10)]
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





