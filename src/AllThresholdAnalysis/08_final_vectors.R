### Probabilistic Crop Loading 

### 08 final vectorization

# Edited by E. Paulukonis October 2022

####create polygon layer with crops ----

#2008
champ1<-st_as_sf(county_layers[[1]])
champ1<-cbind(champ1,cdl_extract_df[[1]])
colnames(champ1)
colnames(champ1)[3]<-"crop"
colnames(champ1)[4]<-"area"

st_write(champ1, root_data_out, layer="champaign1_2008.shp", driver = "ESRI Shapefile")
#plot(champ1[3])




#2021
champ1<-st_as_sf(field_list_ill_f[[1]][[1]])
champ1<-cbind(champ1,cdl_extract_data[[1]][[1]])
colnames(champ1)
colnames(champ1)[3]<-"crop"
colnames(champ1)[4]<-"area"
champ1$id<-as.numeric(row.names(champ1))+1

st_write(champ1, root_data_out, layer="test_champaign1_2021.shp", driver = "ESRI Shapefile")
plot(champ1[3])


# st_precision(champ1) <- 0.1 
# touching_list<-st_touches(st_make_valid(champ1))
# library(igraph)
# g = graph.adjlist(touching_list)
# c = components(g)
# #test<-c[[1]]

inst_list<-st_intersects(champ1)
library(igraph)
g = graph.adjlist(inst_list)
c = components(g)


test<-table(c$membership)
test<-as.data.frame(cbind(c$membership, 1:(nrow(champ1)-1)))
names(test)<-c("groups","id")
champ1<-merge(champ1,test,by="id")

testu<-champ1 %>% 
  group_by(groups, crop) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()


area_thresh <- units::set_units(45, m^2) #Fill holes, just a few meters
fw_fill<- fill_holes(testu, threshold = area_thresh)


st_write(fw_fill, root_data_out, layer="test_int_fh_champ1_2021.shp", driver = "ESRI Shapefile")




