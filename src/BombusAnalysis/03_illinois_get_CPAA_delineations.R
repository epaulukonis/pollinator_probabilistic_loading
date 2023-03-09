### Probabilistic Crop Loading 

### 03 getting CPAA delineations 

# Edited by E. Paulukonis March 2023

#get threshold data
print(list.files(path=paste0(root_data_out,"/all_thresh/Illinois"), pattern='3County', all.files=TRUE, full.names=FALSE))
thresh_ill<- file.path(paste0(root_data_out,"/all_thresh/Illinois"), list.files(path=paste0(root_data_out,"/all_thresh/Illinois"), pattern='3County', all.files=TRUE, full.names=FALSE))
thresh_ill<-read.csv(thresh_ill)
thresh_ill <- thresh_ill [!grepl("NANANANANANANANANANANANANANANANANANANANANANANA", thresh_ill $layer.3),] # remove pixels that have no crops in 23 years


#in this section, we'll convert the dataframe to a gridded dataset to perform a focal window analysis
head(thresh_ill)
df_n<-thresh_ill[,c(2:3,9)]
coordinates(df_n)<-~ x + y
gridded(df_n)<-TRUE
df_n<- raster(df_n)
crs(df_n) <- crs(cdl_data_ill_rec[[1]]) # set the CRS the same as the CDL

#for the CPAA, we'll do a 40 acre general crop mask; the FW analysis checks out all cells with value 1 in 40 acre window
r<-terra::rast(df_n)
fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>%
  terra::mask(mask = r)
fw<-raster(fw) #convert back to raster object



fw_outputs<-list()
for(year in 1:length(nlcd_ill)){
  nlcd<-nlcd_ill[[year]]

ext<-extent(nlcd) 
fw<-setExtent(fw, ext)
fw<-crop(fw, nlcd)
fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
#terra::writeRaster(fw, paste0(root_data_out, "/test_mchenry_1_fw_premasknlcd.tif"), filetype = "GTiff", overwrite = TRUE)
fw<-mask(fw, nlcd) #mask out by NLCD NA here

# here's where we identify and remove clumps denoting isolated pixels 
rc <- clump(fw, directions = 8) #identify clumps based on queen rule (all directions)
f<-freq(rc)
f<-as.data.frame(f)
excludeID <- f$value[which(f$count <= 3)] # create set of IDs for pixel clump of 3 or fewer (this was visual inspection based)
formaskSieve <- rc # this creates the 'sieve' we use to remove those clumps

# assign NA to all clumps whose IDs are found in excludeID, and mask out 
formaskSieve[rc %in% excludeID] <- NA
fw<-mask(fw, formaskSieve)

# convert to a vector
fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw),
                                      as_points = FALSE, merge = TRUE))
# fill holes
area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
fw_fill<- fill_holes(fw_poly, threshold = area_thresh)

# write to list
fw_outputs[[year]]<-fw_fill #this is a list of the output CPAA for all NLCD years available from 1999-2021
}

saveRDS(fw_outputs, file=paste0(root_data_out,"/bombus_fw_outputs.RData"))

#nlcd year adjacent
#1999-2002: 2001
#2003-2006: 2006
#2007-2009: 2008
#2010-2012: 2011
#2013-2014: 2013
#2015-2017: 2016
#2018-2021: 2019

# here, we'll write a function to drop polygon edges and expand/shrink the perimeter to smooth the output
area_thresh <- units::set_units(4046.86, m^2) #drop crumbs below 2 acres
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
  shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE) }

#write to shapefiles
for(layer in 1:length(fw_outputs)){
  fw_analysis<-fw_outputs[[layer]]
  illinois_cpaa<-expand_shrink_clean(fw_analysis)
  writeOGR(illinois_cpaa, paste0(root_data_out, "/all_tif/ILLINOIS/CPAA/bombus"), paste0(names(nlcd_ill[layer]),"_cpaa"), driver = "ESRI Shapefile")
}



