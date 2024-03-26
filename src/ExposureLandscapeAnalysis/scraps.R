#### kriging to consider later...----
#problem: many of the tutorials I've seen for kriging involve continuous data
#my continuous data isn't so great; it's more likely that some spots just recorded a single occurence even if there were multiple
# I have only ever looked at SV with residual data, and never done kriging before
#so I found a package/blog that talks about advantages of INLA, which allows for non-normal errors within presence/absence data


#variogram to get distance at which SP autocorrelation becomes negligible
# coordinates(affinis_a)<-~decimalLongitude+decimalLatitude #get coordinates
# coordinates(affinis_b)<-~decimalLongitude+decimalLatitude #get coordinates

# n.vgma <- variogram(log(individualCount) ~ decimalLatitude +decimalLongitude , affinis_a, width=0.1)
# n.fita <- fit.variogram(n.vgma, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
# plot(n.vgma, n.fita) 
# 
# n.vgmb <- variogram(log(individualCount) ~ decimalLatitude +decimalLongitude , affinis_b, width=0.1)
# n.fitb <- fit.variogram(n.vgmb, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
# plot(n.vgmb, n.fitb) 


#2km as distance for now


#now, we need to define the points where we have measurements (counts), and the grid of interest
names(affinis_all)[5]<-"y"
names(affinis_all)[6]<-"x"

#all points that we have
plot_points <- affinis_all %>% as.data.frame %>%
  ggplot(aes(x,y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")
plot_points

#area of interest by state
BR_sp<-st_as_sf(BR)
plot(BR_sp["GEOID"], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)


#let's create a grid across the study area
#first, re-project to the CDL

grid_list<-list()
for(state in 1:length(CDL)){
  state_shp<-BR[state,]
  proj4string(state_shp)=CRS("+init=epsg:4326") # set it to lat-long
  state_shp = spTransform(state_shp,crs(CDL[[1]])) #transform to match NC habitat
  # grid <- makegrid(state_shp, cellsize = 30)
  # grid_list[state]<-grid
  grid_list[state]<-state_shp
}


nc_points<-
  
  
  logi_point_in_pol <- st_intersects(box, points, sparse = FALSE)
point_in_pol <- points[as.vector(logi_point_in_pol), ]

grid <- makegrid(grid_list[[1]], cellsize = 30) # cellsize in map units!
plot(grid)





#convert to spatial
coordinates(affinis_all)=~decimalLongitude+decimalLatitude #get coordinates
proj4string(affinis_all)=CRS("+init=epsg:4326") # set it to lat-long
#affinis_all = spTransform(affinis_all,crs(raw_CDL_all[[1]][[1]])) #transform to match NC habitat


affinis_b<-affinis_all[affinis_all$Year <2000,]
affinis_a<-affinis_all[affinis_all$Year >=2000,]




writeOGR(obj=study_area, dsn=paste0(state_dir,"/studyarea"), layer="StudyArea_bombus", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=suball, dsn=paste0(state_dir,"/studyarea"), layer="Counties_bombus", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=affinis_a, dsn=paste0(state_dir,"/studyarea"), layer="locationsa_bombusespost2000", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=affinis_b, dsn=paste0(state_dir,"/studyarea"), layer="locationsb_bombusespre2000", driver="ESRI Shapefile") # this is in geographical projection


#### GLRI study area
glri<-readOGR(paste0(state_dir,"/greatlakes_subbasins"), layer = "greatlakes_subbasins")
glri= spTransform(glri, crs(all_states)) #transform to match NC habitat

plot(study_area)
plot(glri,add=T, col='blue')

plot(glri)
plot(all_states,add=T)

#RPBB
rpbb<-readOGR(paste0(state_dir), layer = "RPBB_states")

codes<-as.data.frame(cbind(as.numeric(rpbb$STATEFP),(rpbb$STUSPS)))

write.csv(codes ,file='codes.csv', row.names=FALSE)



#this takes a ridiculous amount of time so I scrapped it for now

bombus_zones<-list()
# for(state in 1:length(CDL)){
#   state=1
#   cdl_by_state<-CDL[[state]]
#   cdl_masked = mask(cdl_by_state, BZ)
#   bombus_zones[state]<-cdl_masked
#   print(names(bombus_zones[state]))
#   
# }


# saveRDS(bombus_zones, file=paste0(bombus_dir,"/bombus_CDL_zones.RData"))





#06 ----

#I have to figure out how to partition the field data into weeks, including weeks where no plantings occur


function(week, probs) week[sample(nrow(week), nrow(week)*probs, replace = TRUE),]
sampled_fields<-matrix(data=NA, nrow=nrow(field), ncol=3)
#this works for single probabilities...but not a suite of them 
f.sample <- function(a, probs) a[sample(nrow(a), nrow(a)*probs, replace = TRUE),]
out<-f.sample(field[field$crop==1,], )


soy<-test_df[which(test_df$Year == 1999 & test_df$crop ==5), ]  
ss_s <- sample(1:20,size=nrow(fields),replace=TRUE, prob=corn$prob)
fields$sample<-ss_s

#not going to work because not all of them are sampled 


week_list<-list()
for(w in 1:length(unique(test$Date))){
  
}





#new thought; instead of doing timing partitioning up front, it's probably easier to assign
#fields based on x seed, x foliar, x nothing, and then for each of those three sets, go from there. 

#######----
names(fv)<-1999:2021
list_of_sampled_fields_by_year<-list()
for(year in 1:length(fv)){
  year=6
  field<-fv[[year]]
  field<-as.data.frame(field)
  field$id<-as.numeric(row.names(field))+1
  field$year<-names(fv[year])
  field_cornsoy<-field[field$crop %in% c(1,5),]
  field_other<-field[!field$crop %in% c(1,5),]
  
  crop_probs_corn<-corn[which(corn$Year == names(fv[year])), ] 
  crop_probs_soy<-soy[which(soy$Year == names(fv[year])), ] 
  
  crop_probs_corn<-split(crop_probs_corn, f = crop_probs_corn$Status)
  crop_probs_soy<-split(crop_probs_soy, f = crop_probs_soy$Status)
  
  
  
  sample_fields<-function(x){
    sample(1:3,size=nrow(field_cornsoy[field_cornsoy$crop==x$Crop,]),replace=T, prob=x$prob)
  }
  
  
  
  sample_fields<-function(x){
    sample(1:nrow(x),size=nrow(field_cornsoy[field_cornsoy$crop==x$Crop,]),replace=T, prob=x$prob)
  }
  
  
  #note: it is very important that users be clear that these events are mutually exclusive; IDs assigned weekly values in planted are not linked to IDs in emerged
  corn_weeks<-as.data.frame(sapply(crop_probs_corn,sample_fields))
  soy_weeks<-as.data.frame(sapply(crop_probs_soy,sample_fields))
  # corn_weeks<-gather(corn_weeks, "Status", "Period", 1:3)
  # soy_weeks<-gather(soy_weeks, "Status", "Period", 1:3)
  
  #challenge: I need to figure out how to join the fields by ID...
  
  #corn
  #planted -> emergence 4-28 days
  #emergence to silking: 
  
  #soy
  #planted -> emergence 6-15
  #emergence -> blooming
  
  
  #maybe goal is to only have extra scenario for blooming
  
  #think I am making it too complex, Monday consider dialing down a little
  
  
  field_cornsoy[,5:7]<-ifelse(field_cornsoy$crop ==1, paste0("Week ",corn_weeks), paste0("Week ",soy_weeks))
  #nope
  
  field_cornsoy$SampleP<-ifelse(field_cornsoy$crop ==1, paste0("Week ",corn_weeks), paste0("Week ",soy_weeks))
  field_other$Sample<-paste0("NotAssigned")
  field_f<-rbind(field_cornsoy,field_other)
  field_f <- field_f[order(field_f$id),]
  
  
  
  split_by_week<-split(field_f, f = field_f$Sample)
  
  
  list_of_sampled_fields_by_year[[year]]<-split_by_week
}

names(list_of_sampled_fields_by_year)<-names(fv)[1:6]


#code to make mock probabilities

#set probabilities for each year and treatment; here, we assign the fields based on a set of probabilities
#probabilities for neonics
status<-c("seed","foliar","notreatment")

#probabilties that corn or soy will be assigned seed, foliar, or no treatment
corn_neo_probs<-c(70,20,10)
soy_neo_probs<-c(40,30,30)

#of the ones assigned seed or foliar treatment, these are the probabilities that thsoe fields will also be assigned glyphosate spray
#probabilities for glyphosate  
corn_glyp_probs<-c(0,50,50)
soy_glyp_probs<-c(0,50,50)

treatment_probabilities_corn<-as.data.frame(cbind(status,corn_neo_probs, corn_glyp_probs))
treatment_probabilities_soy<-as.data.frame(cbind(status,soy_neo_probs, soy_glyp_probs))

#test year
treatment_probabilities_soy$year<-2004
treatment_probabilities_corn$year<-2004

treatment_probabilities_soy$t_id<-1:3
treatment_probabilities_corn$t_id<-1:3


CORN<-(c(0.02,0.02,0.02,0.34))
SOYBEANS<-(c(0.017,0.02,0.019,0.34))
unit<-"kg/acre"
maxrates<-as.data.frame(cbind(Compound,CORN,SOYBEANS,unit))
maxrates<-gather(maxrates, "Commodity", "AR", 2:3)


##5. combine data to get rough proportions of treated crops
#for later, if you want row-wise totals
# testy<-state_pest %>% 
#   rowwise() %>% 
#   mutate(SumByIndex = sum(c_across(c(6:15)), na.rm = T))

state_pest<-gather(state_pest, "Commodity", "kg", 6:15)
state_pest$Commodity<-toupper(state_pest$Commodity)
state_pest<-state_pest[state_pest$Commodity %in% maxrates$Commodity, ]
state_pest_df<-left_join(state_pest, maxrates, by=c('Compound',"Commodity"))
state_pest_df$applied_acres<-state_pest_df$kg / as.numeric(state_pest_df$AR)
pest_prop_by_crop<-left_join(state_pest_df, crop_by_state, by = c("Year","Commodity"))
pest_prop_by_crop$percent<-(pest_prop_by_crop$applied_acres/pest_prop_by_crop$sumf)*100

#moral: this ended up being a bit of a bust
#what my intention was to try to link kg applied to application rates and get proportion treated assuming some rate
#but it's really tricky to do!
#I think the better option is to calculate the average application rate by dividing kg/acre as Maggie did...
#question for Tom: if we can't say anything certain about which fields are applied or not
#is it simply best to assume that all fields are applied with an average application rate?

#split by planted, emerged
# crop_probs_corn<-split(crop_probs_corn, f = crop_probs_corn$Status)
# crop_probs_soy<-split(crop_probs_soy, f = crop_probs_soy$Status)

#aggregate by year
county_pest_agg<-county_pest %>% group_by(COMPOUND, YEAR) %>% mutate(avg = mean(EPEST_HIGH_KG))
#assignn categories (glyphosate or neonic)

county_pest_agg<-county_pest_agg %>% group_by(YEAR, type) %>% mutate(totals = sum(unique(avg)))
#this provides use with a way to measure the probability that a field is assigned a particular compound within our 3 county area
county_pest_agg$prop<-county_pest_agg$avg/county_pest_agg$totals







nc <- st_read(system.file("shape/nc.shp", package="sf"))
st_equals(nc[1:5,], nc[1:4,])

ab <- st_join(df1,df2, join = st_equals_exact, suffix = c("_df1", "_df2"), left = F)

df1<-scenarios[[1]]
df2<-scenarios[[2]]
testy<-st_equals(df1,df2)

threshold <- 5
testy<-purrr::map(scenarios, function(x) {
  x[x[["id"]] == threshold & x[["Commdty"]] == "CORN", ]
})

test_df<-testy[[3]]
test_df2<-testy[[8]]

plot(test_df2["geometry"])



##### hab map scraps ----
#### Foliar ----
sub_foloff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "CHLORPYRIFOS",]
sub_folon<-onfield_tox_thresh[onfield_tox_thresh$Compound == "CHLORPYRIFOS",]

sub_foloff<-sub_foloff[!sub_foloff$MediaSub=="Air",]
sub_folon<-sub_folon[!sub_folon$MediaSub=="Air",]

sub_foloff<-sub_foloff[sub_foloff$Commodity == "SOYBEANS",]
sub_folon<-sub_folon[sub_folon$Commodity == "SOYBEANS",]

hab_fol<-buf_hab_sub[buf_hab_sub$id == 1,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(sub_foloff, hab_fol)   
plot(off_field["MediaSub"])
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(sub_folon, hab_fol)
plot(st_geometry(on_field))
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA



# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_fol, 10)
habitat<-mask(crop(nlcd, hab_fol), hab_fol) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )



habitat<-habitat[,-4]

mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = off_field, aes(fill = EEC)) +
  geom_sf(data = on_field, aes(fill = EEC),colour="white") +
  scale_fill_viridis_b(option = "D", 
                       name= "EEC [ug/bee]",
                       
                       # limits=lims,
                       # breaks=sc_fill_breaks
  )+
  facet_grid(.~MediaSub) +
  geom_sf(data = hab_buff, fill = NA) +
  theme_bw() +
  ylab("Foliar Application")+
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    
    axis.title.x=element_blank()
  )

foliar<-plot_grid(
  habplot,
  mediaplot,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(1,4))
foliar





#### Soil ----
sub_soff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "BIFENTHRIN" & offfield_tox_thresh$ApplicationType == "Soil",]
sub_son<-onfield_tox_thresh[onfield_tox_thresh$Compound == "BIFENTHRIN" & onfield_tox_thresh$ApplicationType == "Soil",]

hab_s<-buf_hab_sub[buf_hab_sub$id == 7,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(sub_soff, hab_s)   
plot(off_field["MediaSub"])
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(sub_son, hab_s)
plot(st_geometry(on_field))
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA



# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_s, 10)
habitat<-mask(crop(nlcd, hab_s), hab_s) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  # scale_fill_manual(
  #   values=c(nlcdpalette), name="NLCD",
  #   labels = c('Water',
  #              'Developed, Open Space ',
  #              'Developed, Low Intensity',
  #              'Developed, Medium Intensity',
  #              "Developed, High Intensity",
  #              "Deciduous Forest", 
  #              "Evergreen Forest",
  #              "Mixed Forest",
  #              "Barren",
#              "Shrub",
#              "Grassland", 
#              "Pasture/Hay","Cultivated",
#              "Woody Wetlands",
#              "Herbaceous Wetlands")) +

geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )


habitat<-habitat[,-4]

mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = off_field, aes(fill = EEC)) +
  geom_sf(data = on_field, aes(fill = EEC), colour="white") +
  scale_fill_viridis_b(option = "D", 
                       name= "EEC [ug/bee]"
                       
                       # limits=lims,
                       # breaks=sc_fill_breaks
  )+
  facet_grid(.~MediaSub) +
  geom_sf(data = hab_buff, fill = NA) +
  theme_bw() +
  ylab("Soil Application")+
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank()
    
  )

soil<-plot_grid(
  habplot,
  mediaplot,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(1,4))
soil




####OG fig 4 script
#### Seed ----
sub_seedoff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "IMIDACLOPRID" & offfield_tox_thresh$ApplicationType == "Seed" ,]
sub_seedon<-onfield_tox_thresh[onfield_tox_thresh$Compound == "IMIDACLOPRID" & onfield_tox_thresh$ApplicationType == "Seed" ,]

#st_write(sub_seedon , paste0(root_data_out, "/all_bombus/outputforshiny/imidaclopridref.shp"),overwrite=T, driver = "ESRI Shapefile")

sub_seedoff<-sub_seedoff[!sub_seedoff$MediaSub=="Air",]
sub_seedon<-sub_seedon[!sub_seedon$MediaSub=="Air",]

hab_seed<-buf_hab_sub[buf_hab_sub$id == 1,]
plot(hab_seed)

# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(sub_seedoff, hab_seed)   
plot(st_geometry(off_field))
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(sub_seedon, hab_seed)
plot(st_geometry(on_field))
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA



# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_seed, 10)
habitat<-mask(crop(nlcd, hab_seed), hab_seed) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Mixed Developed Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )



habitat<-habitat[,-4]

mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = off_field, aes(fill = EEC)) +
  geom_sf(data = on_field, aes(fill = EEC),colour="white") +
  scale_fill_viridis_b(option = "D", 
                       name= "EED [ug/bee;ug/g]",
                       
                       # limits=lims,
                       # breaks=sc_fill_breaks
  )+
  facet_grid(.~MediaSub) +
  geom_sf(data = hab_buff, fill = NA) +
  theme_bw() +
  ylab("Seed Application")+
  xlab("")+
  ggtitle("Imidacloprid - Soybeans")+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    
    axis.title.x=element_blank()
  )

seed<-plot_grid(
  habplot,
  mediaplot,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(2,4))
seed


#seed calculations
just_area_on<-on_field[on_field$MediaSub=="Soil",]
just_area_off<-off_field[off_field$MediaSub=="Soil",]

paste0("total area of fields:",sum(st_area(just_area_on))/st_area(hab_seed)*100) 
paste0("total area of fields:",sum(st_area(just_area_off))/st_area(hab_seed)*100 - sum(st_area(just_area_on))/st_area(hab_seed)*100) 


#### Foliar ----
#unique(offfield_tox_thresh$Compound)
sub_foloff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "CARBARYL",]
sub_folon<-onfield_tox_thresh[onfield_tox_thresh$Compound == "CARBARYL",]

#st_write(sub_folon , paste0(root_data_out, "/all_bombus/outputforshiny/carbref.shp"),overwrite=T, driver = "ESRI Shapefile")

sub_foloff<-sub_foloff[!sub_foloff$MediaSub=="Air",]
sub_folon<-sub_folon[!sub_folon$MediaSub=="Air",]

# sub_foloff<-sub_foloff[sub_foloff$Commodity == "SOYBEANS",]
# sub_folon<-sub_folon[sub_folon$Commodity == "SOYBEANS",]

hab_fol<-buf_hab_sub[buf_hab_sub$id == 1,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(sub_foloff, hab_fol)   
plot(st_geometry(off_field))
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(sub_folon, hab_fol)
plot(st_geometry(on_field))
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA



# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_fol, 10)
habitat<-mask(crop(nlcd, hab_fol), hab_fol) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Majority Agriculture Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )



habitat<-habitat[,-4]

mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = off_field, aes(fill = EEC)) +
  geom_sf(data = on_field, aes(fill = EEC),colour="white") +
  scale_fill_viridis_b(option = "D", 
                       name= "EED [ug/bee;ug/g]",
                       
                       # limits=lims,
                       # breaks=sc_fill_breaks
  )+
  facet_grid(.~MediaSub) +
  geom_sf(data = hab_buff, fill = NA) +
  theme_bw() +
  ylab("Foliar Application")+
  ggtitle("Carbaryl - Corn")+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    
    axis.title.x=element_blank()
  )

foliar<-plot_grid(
  habplot,
  mediaplot,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(2,4))
foliar

#foliar calculations
just_area_on<-on_field[on_field$MediaSub=="Soil",]
just_area_off<-off_field[off_field$MediaSub=="Soil",]

paste0("total area of fields: ",sum(st_area(just_area_on))/st_area(hab_fol)*100) 
paste0("total area of fields: ",sum(st_area(just_area_off))/st_area(hab_fol)*100 - sum(st_area(just_area_on))/st_area(hab_fol)*100) 



#### Soil ----
sub_soff<-offfield_tox_thresh[offfield_tox_thresh$Compound == "BIFENTHRIN" & offfield_tox_thresh$ApplicationType == "Soil",]
sub_son<-onfield_tox_thresh[onfield_tox_thresh$Compound == "BIFENTHRIN" & onfield_tox_thresh$ApplicationType == "Soil",]

#st_write(sub_son , paste0(root_data_out, "/all_bombus/outputforshiny/bifref.shp"),overwrite=T, driver = "ESRI Shapefile")

hab_s<-buf_hab_sub[buf_hab_sub$id == 2,]
# bbox_new<-st_bbox(hab_seed)
# 
# xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
# yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# 
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
# bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top
# 
# bbox_new <- bbox_new %>%  # take the bounding box ...
#   st_as_sfc() # ... and make it a sf polygon


off_field<-st_intersection(sub_soff, hab_s)   
plot(st_geometry(off_field))
# off_field[nrow(off_field)+1,]<-off_field[nrow(off_field),]
# off_field[nrow(off_field),1]<-"Habitat"
# off_field[nrow(off_field),16]<-NA
# off_field[nrow(off_field),21]<-NA

on_field<-st_intersection(sub_son, hab_s)
plot(st_geometry(on_field))
# on_field[nrow(on_field)+1,]<-on_field[nrow(on_field),]
# on_field[nrow(on_field),1]<-"Habitat"
# on_field[nrow(on_field),16]<-NA



# sc_fill_breaks<-as.numeric(format(c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(off_field$EEC), min(on_field$EEC), max(on_field$EEC)+max(on_field$EEC)*0.05),digits=2))
# lims<-c(min(off_field$EEC)-min(off_field$EEC)*0.05, max(on_field$EEC)+max(on_field$EEC)*0.05)

#get visual buffer
hab_buff<-st_buffer(hab_s, 10)
habitat<-mask(crop(nlcd, hab_s), hab_s) 
habitat<-as.data.frame(habitat,xy = TRUE)
habitat$Layer_1<-as.factor(habitat$Layer_1)
habitat<-na.omit(habitat)


#### Habitat 
habitat$title<-"Mixed Developed Habitat"
habplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1))+
  colScale+
  # scale_fill_manual(
  #   values=c(nlcdpalette), name="NLCD",
  #   labels = c('Water',
  #              'Developed, Open Space ',
  #              'Developed, Low Intensity',
  #              'Developed, Medium Intensity',
  #              "Developed, High Intensity",
  #              "Deciduous Forest", 
  #              "Evergreen Forest",
  #              "Mixed Forest",
  #              "Barren",
#              "Shrub",
#              "Grassland", 
#              "Pasture/Hay","Cultivated",
#              "Woody Wetlands",
#              "Herbaceous Wetlands")) +

geom_sf(data = hab_buff, fill = NA) +
  facet_grid(.~title) +
  theme_bw() +
  theme(
    legend.title.align=0.5,
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "none",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background = element_rect(fill="lightgrey",
                                     size=0.5, linetype="solid", 
                                     colour ="black")
  )


habitat<-habitat[,-4]

mediaplot<-ggplot()+
  geom_tile(data=habitat, aes(x=x,y=y,fill=Layer_1), alpha=0.4)+
  scale_fill_grey(guide = "none") +
  ggnewscale:::new_scale_fill() +
  geom_sf(data = off_field, aes(fill = EEC)) +
  geom_sf(data = on_field, aes(fill = EEC), colour="white") +
  scale_fill_viridis_b(option = "D", 
                       name= "EED [ug/bee;ug/g]"
                       
                       # limits=lims,
                       # breaks=sc_fill_breaks
  )+
  facet_grid(.~MediaSub) +
  geom_sf(data = hab_buff, fill = NA) +
  theme_bw() +
  ylab("Soil Application")+
  ggtitle("Bifenthrin - Corn")+
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    legend.position = "right",
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    axis.title.x=element_blank()
    
  )

soil<-plot_grid(
  habplot,
  mediaplot,
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(2,4))
soil

#soil calculations
just_area_on<-on_field[on_field$MediaSub=="Soil",]
just_area_off<-off_field[off_field$MediaSub=="Soil",]

paste0("total area of fields: ",sum(st_area(just_area_on))/st_area(hab_s)*100) 
paste0("total area of fields: ",sum(st_area(just_area_off))/st_area(hab_s)*100 - sum(st_area(just_area_on))/st_area(hab_s)*100) 






###OLDER PLOTS
#### Comparison of application type residues ----
# read in residues from DER
res<-read.csv(paste0(pest_dir,"/Residues/DER_ResidueData.csv"))
res<-na.omit(res)
residues<-res
residues$Residues_DER_ugg<-((as.numeric(residues$Mean_ng_g_normalized_1.3mg_seed)*as.numeric(residues$Original_treatment_mg_seed))/residues$Normalized)/1000
residues$Chemical<-toupper(residues$Chemical)
residues$Plant<-toupper(residues$Plant)
residues$Plant<-ifelse(residues$Plant == "SOY","SOYBEANS",residues$Plant)
residues<-residues[,c(4,11,12,14)]
colnames(residues)<-c("Compound","Commodity","MediaSub","Residues_ugg")
residues$ApplicationType<-"Seed"
residues$Source<-"Reported"
residues$Location<-"On-field"


# read in residues from lit review
litres<-read.csv(paste0(pest_dir,"/Residues/Lit_ResidueData_Ethan.csv"))

litres<-litres[,c(1:5)]
litres<-na.omit(litres)
litres$Commodity<-"Other"
litres$Source<-"Reported"

litres<-litres[,c(1,6,2,3,5,7,4)]
colnames(litres)<-colnames(residues)
litres$Compound<-toupper(litres$Compound)

residues<-rbind(residues,litres)
residues<-residues[!residues$Commodity =="COTTON",]

# get the estimated data
combine_all<-na.omit(combine_all)
combine_all$Source<-"Estimated"

names(combine_all)
names(residues)

combine_all<-combine_all[,c(2:4,6:9)]
colnames(combine_all)[4]<-"Residues_ugg"

residues<-residues[,c(1:2,7,4:5,3,6)]
#residues<-residues[residues$Compound == "BIFENTHRIN" |residues$Compound == "IMIDACLOPRID",  ]
names(residues)[3]<-"ID"

#combine all together
combine_all<- rbind(combine_all,residues)

names(combine_all)
names(residues)


forcomparisons<-combine_all[combine_all$MediaSub == "Pollen" | combine_all$MediaSub == "Nectar"| combine_all$MediaSub == "Soil", ]
forcomparisons<-na.omit(forcomparisons)
compare_outputs<-split(forcomparisons, list(forcomparisons$Compound), drop=T)



library(RColorBrewer)
myColors <- brewer.pal(3,"Dark2")
names(myColors) <- unique(combine_all$ApplicationType)
colScale <- scale_colour_manual(name = "ApplicationType",values = myColors)
fillScale <- scale_fill_manual(name = "ApplicationType",values = myColors)

x<-compare_outputs[[7]]

create_plot_of_methodcompare<-function(x){
  df<-x
  
  df$Value<-(as.numeric(df$Residues_ugg))
  df$IDf= factor(df$ID, levels=c('On-field','Off-field'))
  
  
  df<-df[!df$Value < 3e-5,]
  
  out<-ggplot(df, aes(ApplicationType, log(Value),color=ApplicationType, fill=ApplicationType)) +
    geom_boxplot()+
    colScale+
    fillScale+
    #scale_y_continuous(breaks=ticks, labels=format(logticks,3))+
    # facet_grid(rows = vars(MediaSub), cols = vars(IDf), scales="free_y")+
    
    ggh4x::facet_nested(MediaSub ~ IDf + Source, scales="free_y")+
    
    ylab("Log Residues [ug/g]")+
    xlab("Application Type")+
    theme_bw()+
    theme(legend.position="none", axis.text.y = element_text(size=14,face="bold"), axis.text.x = element_text(size=14,face="bold"),  axis.title=element_text(size=14,face="bold"))+
    ggtitle(paste0(x$Compound))
  out
  
}


out_plots<-lapply(compare_outputs, create_plot_of_methodcompare)

compare_app_methods<-plot_grid(
  out_plots[[1]],
  out_plots[[2]],
  hjust=0, vjust=0, align= "h",  label_x = 0.01, ncol= 2,rel_widths = c(3,3))
compare_app_methods


#### Daily Time-step organized by on/off-field ----
combine_all$ID<-factor(combine_all$ID,levels=c("On-field","Off-field"))

#extract air/dust
air_df<-combine_all[combine_all$MediaSub == 'Air', ]
#extract air/dust
dust_df<-combine_all[combine_all$MediaSub == 'Dust', ]
#extract soil
soil_df<-combine_all[combine_all$MediaSub == 'Soil', ]
#extract pollen
pollen_df<-combine_all[combine_all$MediaSub == 'Pollen', ]
#extract nectar
nectar_df<-combine_all[combine_all$MediaSub == 'Nectar', ]

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(7,"Dark2")
names(myColors) <- unique(combine_all$Compound)
colScale <- scale_colour_manual(name = "Compound",values = myColors)


# airn<- ggplot(air_df, aes(day, Value, group=Compound, color=Compound)) +
#   geom_point(aes(shape=ApplicationType), size=3)+
#   #scale_shape_manual(values=c(16,4,8))+
#   colScale+
#   facet_wrap(~Commodity,scales = "free", nrow=1)+
#   ylab(expression(paste("Residues [ug/", m^{2},"]")))+
#   xlab("Days Post-Application")+
#  # geom_text(x=125, y=4000, label="Air", color="black", size=6)+
#   theme_bw() +
# #  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
#   theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
# 
# airn
# 
# 
# dustn<- ggplot(dust_df, aes(day, Value, group=Compound, color=Compound)) +
#   geom_point(aes(shape=ApplicationType), size=3)+
#   scale_shape_manual(values=c(16,4,8))+
#   facet_wrap(~Commodity,scales = "free", nrow=1)+
#   ylab(expression(paste("Residues [ug/", m^{2},"]")))+
#   xlab("Days Post-Application")+
# # geom_text(x=125, y=5.5, label="Dust", color="black", size=6)+
#   theme_bw() +
#  # theme(plot.margin = unit(c(1,1,1,1), "cm"))+
#   theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
# 
# dustn


soiln<- ggplot(soil_df, aes(day, (Value), color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  
  scale_x_continuous(limits=c(0, 150), breaks=seq(0,150, by=20))+
  facet_wrap(~Commodity + ID,scales = "free", nrow=2)+
  #facet_grid(rows = vars(Commodity), cols = vars(ID), scales="free")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  theme_bw()+
  theme()+
  theme(legend.position="none",axis.text.y = element_text(size=12,face="bold"), 
        axis.text.x = element_text(size=12,face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_rect(color = "black"))
soiln


pollenn<- ggplot(pollen_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  facet_wrap(~Commodity + ID,scales = "free", nrow=2)+
  #facet_grid(rows = vars(Commodity), cols = vars(ID), scales="free")+
  ylab("")+
  xlab("Days After Pollen Emergence")+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), 
        axis.text.x = element_text(size=12,face="bold"),  
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_rect(color = "black"))
pollenn

c<-seq(67,150, by=10)
s<-seq(56,140, by=10)

pollenn<-pollenn + facetted_pos_scales(
  x = list(
    Commodity == "CORN" & ID == "On-field" ~ scale_x_continuous(limits=c(67, 150), breaks=c,labels =c("0","10","20","30","40","50","60","70","80")),
    Commodity == "SOYBEANS" & ID == "On-field"~ scale_x_continuous(limits=c(56, 140),breaks=s,labels =c("0","10","20","30","40","50","60","70","80"))
  )
)


nectarn<- ggplot(nectar_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  facet_wrap(~Commodity + ID,scales = "free", nrow=3)+
  #facet_grid(rows = vars(Commodity), cols = vars(ID), scales="free_y")+
  ylab("")+
  xlab("Days After Nectar Emergence")+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=12,face="bold"),  
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_rect(color = "black"))
nectarn

c<-seq(67,150, by=10)
s<-seq(56,140, by=10)

nectarn<-nectarn + facetted_pos_scales(
  x = list(
    
    Commodity == "CORN" & ID == "On-field" ~ scale_x_continuous(limits=c(67, 150), breaks=c,labels =c("0","10","20","30","40","50","60","70","80")),
    Commodity == "SOYBEANS" & ID == "On-field"~ scale_x_continuous(limits=c(56, 140),breaks=s,labels =c("0","10","20","30","40","50","60","70","80"))
  )
)


compare_between_media<-plot_grid(
  soiln,
  pollenn,
  nectarn, 
  labels = c('Soil','Pollen','Nectar'),
  
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 1,rel_widths = c(4,4,3))
compare_between_media


all_plot_legend <- ggplot(combine_all, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype=ApplicationType), size=1)+
  colScale+
  facet_wrap(~Commodity,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+ 
  theme_bw()+
  theme( legend.key.size = unit(1, 'cm'), #change legend key size
         legend.key.height = unit(1, 'cm'), #change legend key height
         legend.key.width = unit(1, 'cm'), #change legend key width
         legend.title = element_text(size=16), #change legend title font size
         legend.text = element_text(size=14))



legend<-get_only_legend(all_plot_legend)

compare_between_media<-plot_grid(compare_between_media, legend,ncol = 2, rel_widths = c(7,1))
compare_between_media






