### Probabilistic Crop Loading 

### 05 vectorizing final fields

# Edited by E. Paulukonis March 2023

#in this script, we process the final field outputs and extract the original CDL to the fields while applying a get of geometric
#functions to match original crop patterns by year. we then output the final field delineation as a vector for each year

final_vector<-paste0(paste0(root_data_out, "/all_tif/ILLINOIS/bombus/modified_final_vectors/CDL_1999_17_fieldoutput_fieldoutput.shp"))

#final_vector<-paste0(paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors/CDL_1999_17_fieldoutput.shp"))
# if(file.exists(final_vector)){
#   
#   print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
#   fv<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
#   fv<-setNames(lapply(fv, readOGR), tools::file_path_sans_ext(basename(fv)))
#   fv<-fv[(mixedsort(as.character(names(fv))))]
# 
# 
#   
# }else{
# 
# print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# ill_field<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/bombus/SUB"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/SUB"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
# ill_field<-setNames(lapply(ill_field, readOGR), tools::file_path_sans_ext(basename(ill_field)))
# ill_fieldf<-ill_field[(mixedsort(as.character(names(ill_field))))]
# 
# #to match appropriate NLCD...
# #sets of CDL years
# #1999-2002: 2001
# #2003-2006: 2006
# #2007-2009: 2008
# #2010-2012: 2011
# #2013-2014: 2013
# #2015-2017: 2016
# #2018-2021: 2019
# 
# 
# temporal_level_vectors<-list()
# for(i in 1:length(ill_fieldf)){
#   fields<-ill_fieldf[[i]]
#   
#   #match cdl sets 
#     if(i ==1){cdl_data<-cdl_data_ill_rec[1:4]}
#     if(i ==2){cdl_data<-cdl_data_ill_rec[5:8]}
#     if(i ==3){cdl_data<-cdl_data_ill_rec[9:11]}
#     if(i ==4){cdl_data<-cdl_data_ill_rec[12:14]}
#     if(i ==5){cdl_data<-cdl_data_ill_rec[15:16]}
#     if(i ==6){cdl_data<-cdl_data_ill_rec[17:19]}
#     if(i ==7){cdl_data<-cdl_data_ill_rec[20:23]}
#   
#   #match names of cdl sets
#     if(i ==1){names(cdl_data)<-names(cdl_data_ill_rec[1:4])}
#     if(i ==2){names(cdl_data)<-names(cdl_data_ill_rec[5:8])}
#     if(i ==3){names(cdl_data)<-names(cdl_data_ill_rec[9:11])}
#     if(i ==4){names(cdl_data)<-names(cdl_data_ill_rec[12:14])}
#     if(i ==5){names(cdl_data)<-names(cdl_data_ill_rec[15:16])}
#     if(i ==6){names(cdl_data)<-names(cdl_data_ill_rec[17:19])}
#     if(i ==7){names(cdl_data)<-names(cdl_data_ill_rec[20:23])}
#   
#   
# field_extractions_cdl<-list()
#   for(f in 1:length(cdl_data)){
#     output<-exact_extract(cdl_data[[f]],fields,"mode")
#     output<-as.data.frame(output)
#     field_extract<-as.data.frame(terra::area(fields), na.rm=T)
#     cdl_to_fields<-as.data.frame(cbind(output, field_extract))
#     names(cdl_to_fields)<-c("Crop","Area")
#     field_extractions_cdl[[f]]<-cdl_to_fields
#   }
# 
#   names(field_extractions_cdl)<-names(cdl_data)
#   temporal_level_vectors[[i]]<-field_extractions_cdl
# 
# }
# 
# #here, we take the extracted fields and finalize them with the CDL
# 
# for(field in 1:length(ill_fieldf)){
# set1<-st_as_sf(ill_fieldf[[field]])
# field_extractions_cdl<-temporal_level_vectors[[field]]
# 
# 
#   for(vector in 1:length(field_extractions_cdl)){
#   combo1<-cbind(set1, field_extractions_cdl[[vector]])
#   
#   colnames(combo1)
#   colnames(combo1)[3]<-"crop"
#   colnames(combo1)[4]<-"area"
#   combo1$id<-as.numeric(row.names(combo1))+1
#   
# 
#   inst_list<-st_intersects(combo1)
#   library(igraph)
#   g = graph.adjlist(inst_list)
#   c = components(g)
# 
#   test<-table(c$membership)
#   test<-as.data.frame(cbind(c$membership, 1:(nrow(combo1)-1)))
#   names(test)<-c("groups","id")
#   combo1<-merge(combo1,test,by="id")
# 
#   test_combo1<-combo1 %>%
#     group_by(groups, crop) %>%
#     summarise(geometry = sf::st_union(geometry)) %>%
#     ungroup()
#   
#   area_thresh <- units::set_units(30, m^2) #Fill holes, just one pixel wide
#   fw_fill<- fill_holes(combo1, threshold = area_thresh)
#   
#   st_write(fw_fill, paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), layer=paste0(names(field_extractions_cdl[vector]),"_","fieldoutput.shp"), driver = "ESRI Shapefile")
# 
#   }
# 
# }
# 
# 
# }


#### Code September 2023 to address issues with single fields ---

if(file.exists(final_vector)){
  print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/modified_final_vectors"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  fv<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/bombus/modified_final_vectors"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/modified_final_vectors"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  fv<-setNames(lapply(fv, st_read), tools::file_path_sans_ext(basename(fv)))
  fv<-fv[(mixedsort(as.character(names(fv))))]
  
}else{
print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
fv<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/bombus/final_vectors"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
fv<-setNames(lapply(fv, st_read), tools::file_path_sans_ext(basename(fv)))
fv<-fv[(mixedsort(as.character(names(fv))))]

#let's choose 2011 as our 'true' layer
fields<-fv[[13]]


for(f in 1:length(cdl_data_ill_rec)){
  output<-exact_extract(cdl_data_ill_rec[[f]],fields,"mode")
  output<-as.data.frame(output)
  field_extract<-as.data.frame(terra::area(fields), na.rm=T)
  cdl_to_fields<-as.data.frame(cbind(output, field_extract))


    fieldsf<-st_as_sf(fields)
    fieldsf<-fieldsf[,!c(1,2)]
    
    cdl_to_fields<-cbind(cdl_to_fields, fieldsf)
    colnames(cdl_to_fields)[1]<-"crop"
    colnames(cdl_to_fields)[2]<-"area"
    cdl_to_fields$id<-as.numeric(row.names(cdl_to_fields))

  
  st_write(cdl_to_fields, paste0(root_data_out, "/all_tif/ILLINOIS/bombus/modified_final_vectors"), layer=paste0(names(fv[f]),"_","fieldoutput.shp"), driver = "ESRI Shapefile")
  
}

}
