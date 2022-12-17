### Probabilistic Crop Loading 

### 04 Delineation of Fields

# Edited by E. Paulukonis September 2022


## NOTE: All counties must assumed to be alphabetical as they are being read in !!!!!

import_start_time <- Sys.time()
print("stepping into 04_CPAA_delineation.R")
options(scipen = 999) #remove exponent options, throws R off

mi_cpaa_t<-paste0(root_data_out, "/all_tif/Michigan/CPAA/Huron_threshold1.shp")
# mi_cpaa_t<-paste0(root_data_out, "/all_tif/MICHIGAN/CPAA/Huron_threshold1.shp")
# wi_cpaa_t<-paste0(root_data_out, "/all_tif/WISCONSIN/CPAA/Langlade_threshhold1.shp")

#file.exists(mi_cpaa_t) && file.exists(wi_cpaa_t) &&

if(file.exists(mi_cpaa_t)){

  # print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  # ill_cpaa<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/CPAA"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  # ill_cpaa<-setNames(lapply(ill_cpaa, readOGR), tools::file_path_sans_ext(basename(ill_cpaa)))
  # ill_cpaa<-ill_cpaa[order(mixedsort(names(ill_cpaa)))]
  # 
  # cpaa_list_ill_f<-list()
  # cpaa_list_ill_f[[1]]<-ill_cpaa[1:2]
  # cpaa_list_ill_f[[2]]<-ill_cpaa[4:5]
  # cpaa_list_ill_f[[3]]<-ill_cpaa[6:7]
  # 
  # 
  # cpaa_list_ill_f<-list()
  # cpaa_list_ill_f[[1]]<-ill_cpaa[1:14]
  # cpaa_list_ill_f[[2]]<-ill_cpaa[15:28]
  # cpaa_list_ill_f[[3]]<-ill_cpaa[29:42]
  # names(cpaa_list_ill_f)<-c("Champaign","DuPage","McHenry")
  # 

  print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  mi_cpaa<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN/CPAA"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  mi_cpaa<-setNames(lapply(mi_cpaa, readOGR), tools::file_path_sans_ext(basename(mi_cpaa)))
  mi_cpaa<-mi_cpaa[order(mixedsort(names(mi_cpaa)))]

  cpaa_list_mi_f<-list()
  cpaa_list_mi_f[[1]]<-mi_cpaa[1:14]
  cpaa_list_mi_f[[2]]<-mi_cpaa[15:28]
  cpaa_list_mi_f[[3]]<-mi_cpaa[29:42]
  names(cpaa_list_mi_f)<-c("Huron", "Oceana", "VanBuren") 
  

  print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/CPAA"), pattern='.shp', all.files=TRUE, full.names=FALSE))
  wi_cpaa<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN/CPAA"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/CPAA"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  wi_cpaa<-setNames(lapply(wi_cpaa, readOGR), tools::file_path_sans_ext(basename(wi_cpaa)))
  wi_cpaa<-wi_cpaa[order(mixedsort(names(wi_cpaa)))]

  cpaa_list_wi_f<-list()
  cpaa_list_wi_f[[1]]<-wi_cpaa[1:14]
  cpaa_list_wi_f[[2]]<-wi_cpaa[15:28]
  cpaa_list_wi_f[[3]]<-wi_cpaa[29:42]
  names(cpaa_list_wi_f)<-c("Langlade","Rock","Waushara")
  

}else{
  
  
#### Illinois Delineation ----
# fw_sets<-list()
# cpaa_field_set<-list()
# 
# names(thresh_list_ill_f)<-c("Champaign","DuPage","McHenry")
# 
# for (c in 1:length(thresh_list_ill_f)){
#   thresh_list<-thresh_list_ill_f[[c]]
# 
#   for (i in 1:length(thresh_list)){
#     thresh_layers<-thresh_list[[i]]
#     df_n<-thresh_layers[,c(1:2,8)]
# 
#     coordinates(df_n)<-~ x + y
#     gridded(df_n)<-TRUE
#     df_n<- raster(df_n)
#     crs(df_n) <- crs(cdl_data_ill_rec[[1]])
# 
#     #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
#     # mask out NA areas here using NLCD
#     # focal window of 3x3 pixels, or 7x7 (13)
#     r<-terra::rast(df_n)
#     fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>%
#       terra::mask(mask = r)
#     fw<-raster(fw) #convert back to raster object
# 
# 
# 
#   ##this part is somewhat complicated; here we need to match the nlcd with the year and county
#     Ch<-'Champaign'
#     Du<-'DuPage'
#     Mc<-'McHenry'
# 
# 
#     if(i <= 2 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[1]]}
#     if(i >2 && i <=4 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[2]]}
#     if(i >4 && i <=7 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[3]]}
#     if(i >7 && i <=10 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[4]]}
#     if(i >10 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[5]]}
# 
#     if(i <= 2 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[6]]}
#     if(i >2 && i <=4 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[7]]}
#     if(i >4 && i <=7 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[8]]}
#     if(i >7 && i <=10 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[9]]}
#     if(i >10 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[10]]}
# 
#     if(i <= 2 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[11]]}
#     if(i >2 && i <=4 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[12]]}
#     if(i >4 && i <=7 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[13]]}
#     if(i >7 && i <=10 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[14]]}
#     if(i >10 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[15]]}
# 
#     ext<-extent(nlcd)
#     fw<-setExtent(fw, ext)
#     fw<-crop(fw, nlcd)
#     fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
#     fw<-mask(fw, nlcd) #mask out by NLCD NA here
# 
# 
#     # this evaluates and removes clumps of pixels (nearest neighbor =  8)
#     rc <- clump(fw, directions = 8)
#     f<-freq(rc)
#     f<-as.data.frame(f)
#     excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this was visual inspection based)
#     formaskSieve <- rc
#     # assign NA to all clumps whose IDs are found in excludeID
#     formaskSieve[rc %in% excludeID] <- NA
#     fw<-mask(fw, formaskSieve)
# 
#     # convert to a vector
#     fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw),
#                                           as_points = FALSE, merge = TRUE))
#     # fill holes
#     area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
#     fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
#     #plot(fw_fill)
# 
# 
#     fw_sets[[i]]<-fw_fill
#     names(fw_sets)<-names(thresh_list_ill_f[c])
# 
#   }
# 
#   cpaa_field_set[[c]]<-fw_sets
# 
# }
# 
# #names(cpaa_field_set)[1]<-names(thresh_list_ill_f)[1]
# 
#   #add separate function for buffering and cleaning, drop crumbs < 10 acres, do buffer to smooth of 8 acres
#   area_thresh <- units::set_units(40460, m^2) #drop crumbs below 10 acres
#   expand_shrink_clean<-function(x){
#     expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
#     shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
#     drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE) }
# 
# 
# cpaa_final_batch<-list()
# for(layer in 1:length(cpaa_field_set)){
#  fw_analysis<-cpaa_field_set[[layer]]
#  illinois_cpaa<-lapply(fw_analysis, expand_shrink_clean)
#  #cpaa_final_batch[[layer]]<-illinois_cpaa
#  for(cpaa in 1:length(illinois_cpaa)){
#  writeOGR(illinois_cpaa[[cpaa]], paste0(root_data_out, "/all_tif/ILLINOIS/CPAA"), paste0(names(cpaa_field_set[[layer]]),"_threshold",cpaa), driver = "ESRI Shapefile")
# 
#  }
# }
#   


#### Michigan Delineation ----
fw_sets<-list()
cpaa_field_set<-list()

names(thresh_list_mi_f)<-c("Huron", "Oceana", "VanBuren")
for (c in 1:length(thresh_list_mi_f)){
  thresh_list<-thresh_list_mi_f[[c]]

  for (i in 1:length(thresh_list)){
    thresh_layers<-thresh_list[[i]]
    df_n<-thresh_layers[,c(1:2,8)]

    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_mi_rec[[1]])

    #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
    # mask out NA areas here using NLCD
    # focal window of 1x1 (3) 3x3 pixels (7), or 7x7 (13)
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 3, fun = "modal",  na.policy='omit', fillvalue=NA)%>%
      terra::mask(mask = r)
    fw<-raster(fw) #convert back to raster object



    ##this part is somewhat complicated; here we need to match the nlcd with the year and county
    hu<-'Huron'
    oc<-'Oceana'
    vb<-'VanBuren'


    if(i <= 2 && grepl(hu, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[1]]}
    if(i >2 && i <=4 && grepl(hu, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[2]]}
    if(i >4 && i <=7 && grepl(hu, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[3]]}
    if(i >7 && i <=10 && grepl(hu, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[4]]}
    if(i >10 && grepl(hu, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[5]]}

    if(i <= 2 && grepl(oc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[6]]}
    if(i >2 && i <=4 && grepl(oc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[7]]}
    if(i >4 && i <=7 && grepl(oc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[8]]}
    if(i >7 && i <=10 && grepl(oc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[9]]}
    if(i >10 && grepl(oc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[10]]}

    if(i <= 2 && grepl(vb, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[11]]}
    if(i >2 && i <=4 && grepl(vb, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[12]]}
    if(i >4 && i <=7 && grepl(vb, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[13]]}
    if(i >7 && i <=10 && grepl(vb, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[14]]}
    if(i >10 && grepl(vb, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_mi[[15]]}

    fw<-projectRaster(fw,nlcd, method='ngb',crs(nlcd))
    fw<-crop(fw, nlcd)
    fw<-mask(fw, nlcd) #mask out by NLCD NA here


    # this evaluates and removes clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw, directions = 8)
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count <= 3)] #remove pixel clump of 3 or fewer (this was visual inspection based)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw<-mask(fw, formaskSieve)

    # convert to a vector
    fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw),
                                          as_points = FALSE, merge = TRUE))
    # fill holes
    area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
    fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
    #plot(fw_fill)

    fw_sets[[i]]<-fw_fill
    names(fw_sets)<-names(thresh_list_mi_f[c])

  }

  cpaa_field_set[[c]]<-fw_sets

}

names(cpaa_field_set)<-names(thresh_list_mi_f)

#add separate function for buffering and cleaning, drop crumbs < 1 acres, do buffer to smooth of 8 acres
area_thresh <- units::set_units(4046.86, m^2) #drop crumbs below 1 acres
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
  shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE) }


cpaa_final_batch<-list()
for(layer in 1:length(cpaa_field_set)){
  fw_analysis<-cpaa_field_set[[layer]]
  michigan_cpaa<-lapply(fw_analysis, expand_shrink_clean)
  #cpaa_final_batch[[layer]]<-illinois_cpaa
  for(cpaa in 1:length(michigan_cpaa)){
    writeOGR(michigan_cpaa[[cpaa]], paste0(root_data_out, "/all_tif/MICHIGAN/CPAA"), paste0(names(cpaa_field_set[[layer]]),"_threshold",cpaa), driver = "ESRI Shapefile")

  }
}







##### Wisconsin Delineation ----
fw_sets<-list()
cpaa_field_set<-list()

names(thresh_list_wi_f)<-c("Langlade","Rock","Waushara")

for (c in 1:length(thresh_list_wi_f)){
  thresh_list<-thresh_list_wi_f[[c]]
  
  for (i in 1:length(thresh_list)){
    thresh_layers<-thresh_list[[i]]
    df_n<-thresh_layers[,c(1:2,8)]
    
    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_wi_rec[[1]])
    
   #we'll do 3, which is a much smaller window to start
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 3, fun = "modal",  na.policy='omit', fillvalue=NA)%>% 
      terra::mask(mask = r) 
    fw<-raster(fw) #convert back to raster object
    
    
    
    ##this part is somewhat complicated; here we need to match the nlcd with the year and county
    la<-'Langlade'
    rk<-'Rock'
    wa<-'Waushara'
    
    
    if(i <= 2 && grepl(la, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[1]]} 
    if(i >2 && i <=4 && grepl(la, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[2]]} 
    if(i >4 && i <=7 && grepl(la, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[3]]} 
    if(i >7 && i <=10 && grepl(la, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[4]]} 
    if(i >10 && grepl(la, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[5]]}
    
    if(i <= 2 && grepl(rk, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[6]]} 
    if(i >2 && i <=4 && grepl(rk, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[7]]} 
    if(i >4 && i <=7 && grepl(rk, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[8]]} 
    if(i >7 && i <=10 && grepl(rk, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[9]]} 
    if(i >10 && grepl(rk, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[10]]}
    
    if(i <= 2 && grepl(wa, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[11]]} 
    if(i >2 && i <=4 && grepl(wa, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[12]]} 
    if(i >4 && i <=7 && grepl(wa, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[13]]} 
    if(i >7 && i <=10 && grepl(wa, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[14]]} 
    if(i >10 && grepl(wa, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_wi[[15]]}
    
    fw<-projectRaster(fw,nlcd, method='ngb',crs(nlcd))
    fw<-crop(fw, nlcd)
    fw<-mask(fw, nlcd) #mask out by NLCD NA here
    
    
    # this evaluates and removes clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw, directions = 8) 
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count <= 3)] #remove pixel clump of 9 or fewer (this was visual inspection based)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw<-mask(fw, formaskSieve)
    
    # convert to a vector
    fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw), 
                                          as_points = FALSE, merge = TRUE)) 
    # fill holes 
    area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
    fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
    #plot(fw_fill)
    
    
    fw_sets[[i]]<-fw_fill
    names(fw_sets)<-names(thresh_list_wi_f[c])
    
  }
  
  cpaa_field_set[[c]]<-fw_sets
  
}

names(cpaa_field_set)<-names(thresh_list_wi_f)

#add separate function for buffering and cleaning, drop crumbs < 1 acres, do buffer to smooth of 8 acres
area_thresh <- units::set_units(4046.86, m^2) #drop crumbs below 1 acres
expand_shrink_clean<-function(x){
  expand<-gBuffer(x, width=180, byid=T) # 6 pixel expand to smooth (~8 acres)
  shrink<-gBuffer(expand, width=-180, byid=T) #6 pixel shrink to smooth (~8 acres)
  drop_polygons<-drop_crumbs(shrink, area_thresh, drop_empty = TRUE) }


cpaa_final_batch<-list()
for(layer in 1:length(cpaa_field_set)){
  fw_analysis<-cpaa_field_set[[layer]]
  wisconsin_cpaa<-lapply(fw_analysis, expand_shrink_clean)
  #cpaa_final_batch[[layer]]<-illinois_cpaa
  for(cpaa in 1:length(wisconsin_cpaa)){
    writeOGR(wisconsin_cpaa[[cpaa]], paste0(root_data_out, "/all_tif/WISCONSIN/CPAA"), paste0(names(cpaa_field_set[[layer]]),"_threshold",cpaa), driver = "ESRI Shapefile")
    
  }
}

}