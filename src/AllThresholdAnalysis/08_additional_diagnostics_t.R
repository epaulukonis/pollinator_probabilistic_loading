### Probabilistic Crop Loading 

### 08 additional diagnostics

# Edited by E. Paulukonis December 2022


#### Illinois ----
fw_sets<-list()
cpaa_field_set<-list()

names(thresh_list_ill_f)<-c("Champaign","DuPage","McHenry")

for (c in 1:length(thresh_list_ill_f)){
  c=1
  thresh_list<-thresh_list_ill_f[[c]]

  for (i in 1:length(thresh_list)){
    i=1
    thresh_layers<-thresh_list[[i]]
    df_n<-thresh_layers[,c(1:2,8)]

    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_ill_rec[[1]])

    #test 7 and 13, which is the minimum field size from the LACIE paper, and the minimum field size from Yan and Roy 2016
    # mask out NA areas here using NLCD
    # focal window of 3x3 pixels, or 7x7 (13)
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 13, fun = "modal",  na.policy='omit', fillvalue=NA)%>%
      terra::mask(mask = r)
    fw<-raster(fw) #convert back to raster object



  ##this part is somewhat complicated; here we need to match the nlcd with the year and county
    Ch<-'Champaign'
    Du<-'DuPage'
    Mc<-'McHenry'


    if(i <= 2 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[1]]}
    if(i >2 && i <=4 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[2]]}
    if(i >4 && i <=7 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[3]]}
    if(i >7 && i <=10 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[4]]}
    if(i >10 && grepl(Ch, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[5]]}

    if(i <= 2 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[6]]}
    if(i >2 && i <=4 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[7]]}
    if(i >4 && i <=7 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[8]]}
    if(i >7 && i <=10 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[9]]}
    if(i >10 && grepl(Du, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[10]]}

    if(i <= 2 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[11]]}
    if(i >2 && i <=4 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[12]]}
    if(i >4 && i <=7 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[13]]}
    if(i >7 && i <=10 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[14]]}
    if(i >10 && grepl(Mc, names(thresh_list[1]), fixed = TRUE)){nlcd<-nlcd_ill[[15]]}
    
    ext<-extent(nlcd)
    fw<-setExtent(fw, ext)
    fw<-crop(fw, nlcd)
    fw<-projectRaster(fw, nlcd, method='ngb',crs(nlcd))
    fw<-mask(fw, nlcd) #mask out by NLCD NA here

    # this evaluates and removes clumps of pixels (nearest neighbor =  8)
    rc <- clump(fw, directions = 8)
    f<-freq(rc)
    f<-as.data.frame(f)
    excludeID <- f$value[which(f$count <= 9)] #remove pixel clump of 9 or fewer (this was visual inspection based)
    formaskSieve <- rc
    # assign NA to all clumps whose IDs are found in excludeID
    formaskSieve[rc %in% excludeID] <- NA
    fw<-mask(fw, formaskSieve)
    
    
    #writeRaster(fw, paste0(root_data_out,"/all_tif/Illinois/CPAA/Evaluation/fw_examination_new"), format="GTiff", overwrite = TRUE)
    

    # convert to a vector
    fw_poly<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fw),
                                          as_points = FALSE, merge = TRUE))
    # fill holes
    area_thresh <- units::set_units(40460, m^2) #Fill holes, 10 acres (rounded down to nearest 5th decimal )
    fw_fill<- fill_holes(fw_poly, threshold = area_thresh)
    #plot(fw_fill)


    fw_sets[[i]]<-fw_fill
    names(fw_sets)<-names(thresh_list_ill_f[c])

  }

  cpaa_field_set[[c]]<-fw_sets

}

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

#### Michigan ----
fw_sets<-list()
cpaa_field_set<-list()

names(thresh_list_mi_f)<-c("Huron", "Oceana", "VanBuren")

for (c in 1:length(thresh_list_mi_f)){
  c=2
  thresh_list<-thresh_list_mi_f[[c]]

  for (i in 1:length(thresh_list)){
    i=1
    thresh_layers<-thresh_list[[i]]
    df_n<-thresh_layers[,c(1:2,8)]

    coordinates(df_n)<-~ x + y
    gridded(df_n)<-TRUE
    df_n<- raster(df_n)
    crs(df_n) <- crs(cdl_data_mi_rec[[1]])
    
    
    #Do small acreages; 3
    r<-terra::rast(df_n)
    fw<- terra::focal(r, w = 3, fun = "modal",  na.policy='omit', fillvalue=NA)%>%
      terra::mask(mask = r)
    fw<-raster(fw) #convert back to raster object

    #writeRaster(fw, paste0(root_data_out,"/all_tif/MICHIGAN/CPAA/Evaluation/fw_examination_new"), format="GTiff", overwrite = TRUE)
    

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
    #let's also start with clumps of 3 here
    excludeID <- f$value[which(f$count <= 3)] #remove pixel clump of 3 or fewer 
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

#names(cpaa_field_set)<-names(thresh_list_ill_f)

#add separate function for buffering and cleaning, drop crumbs < 10 acres, do buffer to smooth of 8 acres
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

