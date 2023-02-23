### Probabilistic Crop Loading 

### 10 final vectorization

#Edited by E. Paulukonis October 2022


#Illinois
print(list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_field<- file.path(paste0(root_data_out, "/all_tif/ILLINOIS/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/ILLINOIS/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
ill_field<-setNames(lapply(ill_field, readOGR), tools::file_path_sans_ext(basename(ill_field)))
ill_fieldf<-ill_field[(mixedsort(as.character(names(ill_field))))]

field_list_ill_f<-list()
field_list_ill_f[[1]]<-ill_field[1:14]
field_list_ill_f[[2]]<-ill_field[15:28]
field_list_ill_f[[3]]<-ill_field[29:42]
names(field_list_ill_f)<-c("Champaign","DuPage","McHenry")
rm(ill_field)

foreach(i=1:3) %do% {
  areaf<-sapply(field_list_ill_f[[i]],function(x) sum(terra::area(x)))
  field_list_ill_f[[i]]<-field_list_ill_f[[i]][order(areaf)]
  names(field_list_ill_f[[i]])<-paste0(names(field_list_ill_f[i]),14:1,"fin")
}


#Michigan
print(list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
mi_field<- file.path(paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/MICHIGAN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
mi_field<-setNames(lapply(mi_field, readOGR), tools::file_path_sans_ext(basename(mi_field)))
mi_fieldf<-mi_field[order(mixedsort(names(mi_field)))]

field_list_mi_f<-list()
field_list_mi_f[[1]]<-mi_field[1:14]
field_list_mi_f[[2]]<-mi_field[15:28]
field_list_mi_f[[3]]<-mi_field[29:42]
names(field_list_mi_f)<-c("Huron", "Oceana", "VanBuren")
rm(mi_field)

foreach(i=1:3) %do% {
  areaf<-sapply(field_list_mi_f[[i]],function(x) sum(terra::area(x)))
  field_list_mi_f[[i]]<-field_list_mi_f[[i]][order(areaf)]
  names(field_list_mi_f[[i]])<-paste0(names(field_list_mi_f[i]),14:1,"fin")
}


#Wisconsin
print(list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), pattern='.shp', all.files=TRUE, full.names=FALSE))
wi_field<- file.path(paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), list.files(path=paste0(root_data_out, "/all_tif/WISCONSIN/SUB/smallerfields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
wi_field<-setNames(lapply(wi_field, readOGR), tools::file_path_sans_ext(basename(wi_field)))
wi_field<-wi_field[order(mixedsort(names(wi_field)))]

field_list_wi_f<-list()
field_list_wi_f[[1]]<-wi_field[1:14]
field_list_wi_f[[2]]<-wi_field[15:28]
field_list_wi_f[[3]]<-wi_field[29:42]
names(field_list_wi_f)<-c("Langlade","Rock","Waushara")
rm(wi_field)

foreach(i=1:3) %do% {
  areaf<-sapply(field_list_wi_f[[i]],function(x) sum(terra::area(x)))
  field_list_wi_f[[i]]<-field_list_wi_f[[i]][order(areaf)]
  names(field_list_wi_f[[i]])<-paste0(names(field_list_wi_f[i]),14:1,"fin")
}


acreages_by_countyI<-readRDS(paste0(root_data_out,"/acreages_by_county_sfI.RData"))
cntynames<-c('CHAMPAIGN',"DU PAGE","MCHENRY")
names(acreages_by_countyI)<-cntynames


cntynames<-c("HURON", "OCEANA", "VAN BUREN")
acreages_by_countyM<-readRDS(paste0(root_data_out,"/acreages_by_county_sfM.RData"))
names(acreages_by_countyM)<-cntynames


cntynames<-c("LANGLADE","ROCK","WAUSHARA")
acreages_by_countyW<-readRDS(paste0(root_data_out,"/acreages_by_county_sfW.RData"))
names(acreages_by_countyW)<-cntynames

county_level_vectors<-list()
for(county in 1:length(field_list_ill_f)){
  fields<-field_list_ill_f[[county]][[14]]
  
field_extractions_cdl<-list()
  for(f in 1:length(cdl_data_ill_rec)){
    cdl<-cdl_data_ill_rec[[f]]
    output<-exact_extract(cdl,fields,"mode")
    output<-as.data.frame(output)
    field_extract<-as.data.frame(terra::area(fields), na.rm=T)
    cdl_to_fields<-as.data.frame(cbind(output, field_extract))
    names(cdl_to_fields)<-c("Crop","Area")
    field_extractions_cdl[[f]]<-cdl_to_fields
  }
  
  county_level_vectors[[county]]<-field_extractions_cdl

}


saveRDS(county_level_vectors, file=paste0(root_data_out,"/county_level_field_vectors.RData"))

# 
# champ1<-st_as_sf(field_list_ill_f[[1]][[1]])
# champ1<-cbind(champ1,county_level_vectors[[1]][[1]])
# 
# 
# colnames(du1)
# colnames(du1)[3]<-"crop"
# colnames(du1)[4]<-"area"
# du1$id<-as.numeric(row.names(du1))+1
# 
# 
# inst_list<-st_intersects(du1)
# library(igraph)
# g = graph.adjlist(inst_list)
# c = components(g)
# 
# 
# test<-table(c$membership)
# test<-as.data.frame(cbind(c$membership, 1:(nrow(du1)-1)))
# names(test)<-c("groups","id")
# du1<-merge(du1,test,by="id")
# 
# test_du1<-du1 %>% 
#   group_by(groups, crop) %>%
#   summarise(geometry = sf::st_union(geometry)) %>%
#   ungroup()
# 
# 
# area_thresh <- units::set_units(45, m^2) #Fill holes, just a few meters
# fw_fill<- fill_holes(test_du1, threshold = area_thresh)
# 
# 
# st_write(fw_fill, root_data_out, layer="test_int_fh_du1_2008.shp", driver = "ESRI Shapefile")
# 
# 
# 
# 
# ####create polygon layer with crops ----
# 
# #2008
# champ1<-st_as_sf(county_layers[[1]])
# champ1<-cbind(champ1,cdl_extract_df[[1]])
# colnames(champ1)
# colnames(champ1)[3]<-"crop"
# colnames(champ1)[4]<-"area"
# 
# st_write(champ1, root_data_out, layer="champaign1_2008.shp", driver = "ESRI Shapefile")
# #plot(champ1[3])
# 
# 
# 
# 
# #2021
# champ1<-st_as_sf(field_list_ill_f[[1]][[1]])
# champ1<-cbind(champ1,cdl_extract_data[[1]][[1]])
# colnames(champ1)
# colnames(champ1)[3]<-"crop"
# colnames(champ1)[4]<-"area"
# champ1$id<-as.numeric(row.names(champ1))+1
# 
# st_write(champ1, root_data_out, layer="test_champaign1_2021.shp", driver = "ESRI Shapefile")
# plot(champ1[3])
# 
# 
# # st_precision(champ1) <- 0.1 
# # touching_list<-st_touches(st_make_valid(champ1))
# # library(igraph)
# # g = graph.adjlist(touching_list)
# # c = components(g)
# # #test<-c[[1]]
# 
# inst_list<-st_intersects(champ1)
# library(igraph)
# g = graph.adjlist(inst_list)
# c = components(g)
# 
# 
# test<-table(c$membership)
# test<-as.data.frame(cbind(c$membership, 1:(nrow(champ1)-1)))
# names(test)<-c("groups","id")
# champ1<-merge(champ1,test,by="id")
# 
# testu<-champ1 %>% 
#   group_by(groups, crop) %>%
#   summarise(geometry = sf::st_union(geometry)) %>%
#   ungroup()
# 
# 
# area_thresh <- units::set_units(45, m^2) #Fill holes, just a few meters
# fw_fill<- fill_holes(testu, threshold = area_thresh)
# 
# 
# st_write(fw_fill, root_data_out, layer="test_int_fh_champ1_2021.shp", driver = "ESRI Shapefile")
# 
# 
# 
# 
