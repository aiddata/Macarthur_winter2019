


library(sf)

setwd("/Users/rbtrichler/Box Sync")

## Read in New Cambodia Lines Dataset (from Miranda)

# Read in Cambodia lines geo-referenced
maclines <- "MacArthur_Winter2019/geocodeddata_dec2018/inputdata/MacCambodia_lines.geojson"
maclines <- st_read(maclines)

# Convert to dataframe
maclines_geo<-st_geometry(maclines)
st_geometry(maclines)<-NULL
maclines_geo<- st_set_geometry(as.data.frame(maclines[,c(1,9,10,15)]),maclines_geo)

## Read in original MacArthur datasets

#Mac_spdf_Cambodia 
#These are 36 project locations used for original analysis

mac_spdf <- read.csv("MacArthur/Mac_spdf_Cambodia_recreate.csv")

## Compare old and new data 

#compare project id's
table(maclines$project_id)
table(mac_spdf$project_id)

unique(maclines$project_id)
sum(is.na(maclines$project_id))
sum(is.na(mac_spdf$project_id))

#output text
maclines$Geocoding.and.Review.Note[49]
#identify projects where project_id is NA (and most other fields are also NA)
maclines$id[is.na(maclines$project_id)]

#identify projects in maclines that are only coded to ADM0 or ADM1
maclines_adm01 <- maclines[grep("KHM_ADM", maclines$GeoJSON.Link.or.Feature.ID),]
table(maclines_adm01$project_id)

#subset maclines to non-ADM0 and non-ADM1
maclines_prec<- maclines[-grep("KHM_ADM", maclines$GeoJSON.Link.or.Feature.ID),]
# identify projects in maclines_prec where dates are missing
maclines_prec$project_id[is.na(maclines$start_actual)]
sum(is.na(maclines_prec$end_actual))

#identify projects in mac_prec that are ODA-like flow-class
macprec_ODA<-maclines_prec[grep("ODA-like",maclines_prec$flow_class),]
table(macprec_ODA$project_id)
table(mac_spdf$project_id)

#missing end dates
maclines_prec$end_planned[is.na(maclines_prec$end_actual)]
maclines_prec$id[is.na(maclines_prec$end_planned)]

maclines_prec$project_id[is.na(maclines_prec$start_planned)]
maclines_prec$start_actual[is.na(maclines_prec$end_actual)]
maclines_prec$start_actual[is.na(maclines_prec$end_actual)]


write.csv(maclines,"MacArthur_Winter2019/geocodeddata_dec2018/MacCambodia_lines.csv")



