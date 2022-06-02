#what we need to do:

#determine crops to which our pesticides of interest are applied (corn, ww, soy), and 'other'
#determine what the area is in a cumulative sense
#and then what the areas are when we ONLY use the N years designation




##### Total CDL area
#this section of the code outputs a total area by CDL pixel for each year, similar to table 2. 
# list_freq<-list()
# for(f in 1:nlayers(county_r)){
# sq<-as.data.frame(freq(county_r[[f]]))
# sq[4,]<-ifelse(nrow(sq)==4,sq[4,],NA) #this ensures that all dataframes by year have the same # of rows
# sq[3,2]<-ifelse(is.na(sq[3,1]),NA,sq[3,2])
# list_freq[[f]]<-sq
# }
# 
# #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
# df_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
# value<-df_crop_areas[,1]
# df_crop_areas = df_crop_areas[,!names(df_crop_areas) == 'value']
# df_crop_areas$value<-value
# df_crop_areas<-df_crop_areas[1:3,c(12,1:11)]
# names(df_crop_areas)<-c("Crop",1999:2009) #make sure names align
# df_crop_areas[,2:12]<-df_crop_areas[,2:12]*900 #get area in meters
# df_crop_areas[is.na(df_crop_areas)] = 0
# df_crop_areas[4,2:12]<-colSums(df_crop_areas[,2:12])
# row.names(df_crop_areas)[4]<-'sum'
# 


#####Options/attempts at clustering
# #kmeans 
# x<-testdf$field
# testy<-kmeans(x, 100, iter.max = 10, nstart = 25)
# testy$cluster
# testdf$cluster<-testy$cluster
# 
# #bins of digits; not so great, large swaths of fields 
# testdf$newcode<-floor(log10(testdf$field)) + 1
# 
# #quantiles; this doesn't make sense to me. I do understand how it's being binned in theory, not sure why the output is restricted in QGIS
# quant<-as.data.frame(quantile(testdf$field, probs = seq(0, 1, 0.01))) 
# 
# #jenks breaks
# getJenksBreaks(var, k, subset = NULL)



#####This removes small singular pixels; at the moment, let's put this here while we evaluate the cumulative area
#first; let's remove isolated islands of corn, soy or grassland pixels that are likely not part of fields 
rc <- clump(testl, directions = 8) #this evaluates clumps of pixels (nearest neighbor =  8)
#plot(rc)
#writeRaster(rc, file.path(cdl_dir, "/rc"), format="GTiff", overwrite = TRUE)
# get frequency table    
f<-freq(rc)
# save frequency table as data frame
f<-as.data.frame(f)

# which rows of the data.frame are only represented by clumps = 1 pixels?
excludeID <- f$value[which(f$count <= 1)]
# make a new raster to be sieved
formaskSieve <- rc
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[rc %in% excludeID] <- NA

testls<-mask(testl, formaskSieve)
plot(testls)



#is this correct? total possible response is n total crop pixels
#prob for 11 years 
success<-0:total_n
plot(success, dbinom(success, size=total_n, prob=.14),type='h')

is_n <- c(0,1,2:4,5,6:23,24,25:256)
becomes_n <- c(NA,1,rep.int(0,3),1,rep.int(0,18),1,rep.int(0,232))
n<-cbind(is_n,becomes_n)

county_list_b<-list()
layer_list_b<-list()
for (county in 1:length(county_set_list)){
  county_r<-county_set_list[[county]]
  for(layer in 1:nlayers(county_r)){
    layer_list_b[[layer]] <- reclassify(county_r[[layer]], n)
  }
  county_list_b[[county]]<-layer_list_b
}


#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to 9
# is_m <- c(0,1,2:4,5,6:23,24,25:256)
# becomes <- c(NA,1,rep.int(9,3),2,rep.int(9,18),3,rep.int(9,232))
# m<-cbind(is_m,becomes)


plot_list<-list()
for(n in 1:length(unique(df$bin))){
  df_n<-df[df$bin >=n,]
  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_ill_rec[[1]])
  plot(df_n)
  
  plot_list[[n]]<-df_n
  
}

##Don't need this for right now
# pal <-c("#a6cee3",
#         "#1f78b4",
#         "#b2df8a",
#         "#33a02c",
#         "#fb9a99",
#         "#e31a1c",
#         "#fdbf6f",
#         "#ff7f00",
#         "#cab2d6",
#         "#6a3d9a",
#         "#fed976")
# 
# scales::show_col(pal) #take a look at color palettes
# 
# #this corrects the colors for N years
# plot_maps<-function(raster){
#   n<-length(unique(values(raster)))
#   plot(raster, col=pal[(n-1):1])
# }
# 
# plots<-lapply(plot_list,plot_maps) #plot them with the right colors


#proportions of specific crops

#testy$seq<-as.character(testy$n_years)
testy$corn<- str_count(testy$f, "1")/testy$bin
testy$soy<- str_count(testy$f, "2")/testy$bin
testy$ww<- str_count(testy$f, "3")/testy$bin
df_c<-testy[,c(1:2,8)] #corn
df_s<-testy[,c(1:2,9)] #soy
df_w<-testy[,c(1:2,10)] #ww

df_n<-testy[,c(1:2,9)] #get raw corn
df_n<-testy[,c(1:2,6)] #sequence to compare



####Various spatial tools/techniques!


#classification
rc<- cut(df_n, breaks=breaks) #IDK what breaks to use
library(classInt)
zClass <- classIntervals(values(df_n), n=6,style="jenks")
plot(df_n, breaks=zClass$brks, col=colorRampPalette(c("red", "yellow"))(5) )


#expand/shrink (takes a while)
out_exp<-buffer(df_n,width=30) #expand
out_shrk<-buffer(out_exp,width=-30) #shrink

#clump
testy_patch <- clump(df_n, directions=4) #clumping looks for connected cells, using rook or queen rule

#focal stats
width=3
r2 <- focal(df_n, w = matrix(1,width,width), fun = modal) #focal stats overestimates? this for some reason doesn't work as well as terra focal

#aggregate
m <- aggregate(df_n, fact = 2, fun = modal, na.rm = TRUE)
plot(m)

#disaggregate
y <- disaggregate(df_n, 5, method='bilinear') #bilinear resamples 
plot(y)
y <- focal(y, w=matrix(1, 3, 3), modal) 





##### CONVERT BACK TO RASTER DATA ####
df<-thresh_layers
df<-df[,c(1:2,8)] #8 if you'd like it to be binary
plot_list<-list()
for(n in unique(df$bin)){
  df_n<-df[df$bin ==n,]
  coordinates(df_n)<-~ x + y
  gridded(df_n)<-TRUE
  df_n<- raster(df_n)
  crs(df_n) <- crs(cdl_data_ill_rec[[1]])
  plot(df_n)
  plot_list[[n]]<-df_n
}
plot_list<-plot_list[lengths(plot_list) != 0] #remove empty list elements


##### VECTORIZE ####
#first, let's remove small individual 'islands' of pixels
cleaned_plots<-list()
for (layer in 1:length(plot_list)){
  rc <- clump(plot_list[[layer]], directions = 8) #this evaluates clumps of pixels (nearest neighbor =  8)
  f<-freq(rc)
  # save frequency table as data frame
  f<-as.data.frame(f)
  # which rows of the data.frame are only represented by clumps = 1 pixels?
  excludeID <- f$value[which(f$count <= 1)]
  # make a new raster to be sieved
  formaskSieve <- rc
  # assign NA to all clumps whose IDs are found in excludeID
  formaskSieve[rc %in% excludeID] <- NA
  plot_l<-plot_list[[layer]]
  plot_l<-mask(plot_l, formaskSieve)
  cleaned_plots[[layer]]<-plot_l
}

#check that little islands are gone
plot(plot_list[[1]], col='blue')
plot(cleaned_plots[[1]], col="blue")


###We can clean holes/edges one of two ways:

##1: with the sf package and the fill_holes function
#let's try vectorizing with the sf package, which is quite fast
r1<-cleaned_plots[[1]] #this is either the whole binary layer, or individual binary layers
vectorized_bins<- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(r1), 
                                              as_points = FALSE, merge = TRUE)) 
#first, we'll fill in some of the holes; I used a larger buffer of 500 km2 here; this might be too big
area_thresh <- units::set_units(50, m^2)
p_dropped <- fill_holes(vectorized_bins, threshold = area_thresh)
plot(p_dropped)
p_dropped<-aggregate(p_dropped)


##2: using the expand/shrink tool in ArcGIS Pro or buffer, which takes a while in R




s0 = brick(fw)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
df_fw = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
df_fw <- data.frame(coords, df_fw)
testy<-layer.3
testy<-na.omit(testy)
binned_prop<-split(testy, f=thresh_layers$layer.3) #split if needed





##write rasters and/or shapefiles
writeRaster(r1, file.path(cdl_dir, "/r1_CAA"), format="GTiff", overwrite = TRUE)
writeOGR(p_dropped, cdl_dir,  "/r_polyall_fin_50m", driver = "ESRI Shapefile")

#get NLCD as urban layers

nlcd<-raster(paste0(nlcd_dir,"/NLCD_2019_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))
nlcd<-setExtent(nlcd, ext)
nlcd<-crop(nlcd, cpaa)
nlcd<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
nlcd<-mask(nlcd, cpaa)
plot(nlcd)


# #use 2008 NLCD to mask out non-crop
# nlcd<-raster(paste0(nlcd_dir,"/NLCD_2008_Land_Cover_L48_20210604_8Jzq7uEvh4Wq2TtvZWFJ.tiff"))
# nlcd<-setExtent(nlcd, ext)
# nlcd<-crop(nlcd, cpaa)
# nlcd<-projectRaster(nlcd, cpaa, method='ngb',crs(cpaa))
# nlcd<-mask(nlcd, cpaa)
# nlcd[nlcd==11]<-NA
# nlcd[nlcd==22]<-NA
# nlcd[nlcd==23]<-NA
# nlcd[nlcd==24]<-NA
# plot(nlcd)
# cpaa<-mask(cpaa, nlcd)
# plot(cpaa)



#first, fix the cpaa_mask to the same extent (low er columns/rows)
cpaa_mask<-setExtent(cpaa_mask, ext)
cpaa_mask<-crop(cpaa_mask, cpaa)
cpaa_mask<-projectRaster(cpaa_mask, cpaa, method='ngb',crs(cpaa))
cpaa<-mask(cpaa, cpaa_mask) #re-mask with the original CDL crop/non-crop so that any NAs filled in get removed



crop_list<-list()
for(layer in 1:length(y)){
  output<-extract(y[[layer]],fw_smooth)
  crop_list[[layer]]<-output
}


#function for mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#1 = corn, 2 = soy, 3= winter wheat, 0 = other, 100=NA
#get mode of the extracted fields for each year of the CDL
test_year_fix<-list()
test_year_mode<-list()
for(year in 1:length(crop_list)){
  test_year_fix[[year]]<-lapply(test_year, function(d) { d[is.na(d)] <- 100; d }) #assign NA to 100 in order to run mode function
  test_year_mode[[year]]<-lapply(test_year_fix[[year]], function(x) Mode(x)) 
}


#fw_smooth <- smooth(fw_dropped, method = "ksmooth")

#additional 'smoothing'
# agg_fw <- terra::aggregate(fw, fact = 3, fun = modal, na.rm = TRUE)
# fw_f<- disaggregate(agg_fw, 3) #bilinear resamples 

# ifelse(nrow(sq)<5,sq[5,]==filler,sq[5,]==sq[5,])



filler<-cbind(3, NA)
outliers<-Filter(function(x) nrow(x) <5, list_freq)
main<-Filter(function(x) nrow(x) >=5, list_freq)
cdl1<-do.call(cbind, lapply(outliers, as.data.frame))
cdl2<-do.call(cbind, lapply(main, as.data.frame))
cdl1[5,]<-filler
cdl1<-cdl1[c(1:3,5,4),]

value<-cdl1[,1]
cdl_crop_areas<-cbind(cdl1,cdl2)
cdl_crop_areas = cdl_crop_areas[,!names(cdl_crop_areas) == 'value']
cdl_crop_areas[6,]<-colSums(cdl_crop_areas[1:4,1:11])
cdl_crop_areas$value<-c(value,"sum")
cdl_crop_areas<-cdl_crop_areas[,c(12,3:4,1:2,5:11)]
names(cdl_crop_areas)<-c("Crop",1999:2009) #make sure names align




crop_list<-list()
for(layer in 1:length(y)){
  layer=1
  extr<-exact_extract(y[[layer]],output, "mode")
  extr<-as.data.frame(extr)
  names(extr)<-"crops"
  extr$area<-field_areas$area
  extr<- extr %>% group_by(crops) %>%   summarise(area = sum(area)*0.000247105)
  crop_list[[layer]]<-extr
}

names(crop_list)<-1999:2009

outliers<-Filter(function(x) nrow(x) <5, crop_list)
main<-Filter(function(x) nrow(x) >=5, crop_list)
field1<-do.call(cbind, lapply(outliers, as.data.frame))
field2<-do.call(cbind, lapply(main, as.data.frame))
field1[5,]<-filler
field1<-field1[c(1:3,5,4),]

value<-field1[,1]
field_crop_areas<-cbind(field1,field2)
field_crop_areas =  field_crop_areas %>% select(-contains(".crops"))
field_crop_areas$value<-value
field_crop_areas<-field_crop_areas[ ,order(names(field_crop_areas))]

field_crop_areas<-field_crop_areas[,c(12,1:11)]
names(field_crop_areas)<-c("crop",1999:2009) #make sure names align

error<-((field_crop_areas[,c(2:12)] - cdl_crop_areas[,c(2:12)])/cdl_crop_areas[,c(2:12)])*100
error$crop<-c("other","corn","soy","ww","NA")
print(error)

colSums(field_crop_areas[,c(2:12)])
colSums(cdl_crop_areas[,c(2:12)])

error_f<-tidyr::gather(error, key="year", value="residual", 1:11)
ggplot(error_f, aes(x=year, y=residual, fill=crop) )+ 
  geom_boxplot()+
  facet_wrap(~crop)
error_nona<-error_f[error_f$crop !="NA",]
ggplot(error_nona, aes(x=year, y=residual, fill=crop) )+ 
  geom_boxplot()+
  facet_wrap(~crop)



##in this section, we need to buffer and get the larger CPAA
# cpaa <- raster(paste0(cdl_dir, "/cpaa_mask.tif")) #use expanded CPAA
# cpaa[cpaa == 0] <- NA
# ext<-extent(cpaa)


#### Reclassify ----
#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to 0 (or 9 if needed to count)
# is_m <- c(0,1,2:4,5,6:23,24,25:256)
# becomes <- c(NA,1,rep.int(0,3),2,rep.int(0,18),3,rep.int(0,232))
# m<-cbind(is_m,becomes)
# 
# county_list<-list()
# layer_list<-list()
# for (county in 1:length(county_set_list)){
#   county_r<-county_set_list[[county]]
#   for(layer in 1:nlayers(county_r)){
#     layer_list[[layer]] <- reclassify(county_r[[layer]], m)
#   }
#   county_list[[county]]<-layer_list
# }


# is_n <- c(0,1,2:4,5,6:23,24,25:256)
# becomes_n <- c(NA,1,rep.int(0,3),1,rep.int(0,18),1,rep.int(0,232))
# n<-cbind(is_n,becomes_n)
# 
# #this reclassifys the layers from county_set_list to be binary (crop and non-crop)
# county_list_binary<-list()
# layer_list<-list()
# for (county in 1:length(county_set_list)){
#   county_r<-county_set_list[[county]]
#   for(layer in 1:nlayers(county_r)){
#     layer_list[[layer]] <- reclassify(county_r[[layer]], n)
#   }
#   county_list_binary[[county]]<-layer_list
# }
# 
# county_list<-county_list_binary

####Dataframe compilation ----
##in this section: we take the county_list and turn it into a dataframe, and compile the sequence columns
# dataframe_list<-list()
# for(county in 1:length(county_list)){
# 
# #convert to dataframe
# y<-county_list[[i]] ##first county
# s0 = brick(y) #brick the raster
# coords = coordinates(s0) #get the coordinates
# s1 = as.data.frame(getValues(s0)) #get the dataframe
# rec_layer = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = '')) #make it nicer looking
# rec_layer <- data.frame(coords, rec_layer) #finish as final organized frame
# 
# #let's format the sequence data
# rec_f_layer<-rec_layer
# any_layer <- rec_f_layer[!grepl("NANANANANANANANANANANA", rec_f_layer$rec_layer),] # remove pixels that have no crops in 11 years
# any_layer$f<-gsub("NA", "", any_layer$rec_layer) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
# any_layer$field<-as.numeric(any_layer$f) #turn f into fields as numeric integer
# any_layer$n_years<-as.numeric(gsub(0, "", any_layer$field)) #sub 0s with empty slots
# any_layer$bin<-floor(log10(any_layer$n_years)) + 1  #create column to count the number of years
# 
# 
# thresh_layers<-any_layer[any_layer$bin >= 6,] #for now
# #thresh_layers<-any_layer[tany_layer$bin >= (as.numeric(thresh$Var1) - 1),] 
# 
# thresh_layers<-na.omit(thresh_layers)
# dataframe_list[[county]]<-thresh_layers
# #binned_prop<-split(thresh_layers, f=thresh_layers$bin) #split if needed
# 
# }