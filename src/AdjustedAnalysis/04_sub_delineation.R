### Probabilistic Crop Loading 

### 04 Sub-delineation of fields

# Edited by E. Paulukonis March 2022

import_start_time <- Sys.time()
print("stepping into 04_sub_delineation.R")

options(scipen = 999) #remove exponent options, throws R off

#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to NA (or 9 if needed to count)
is_m <- c(0,1,2:4,5,6:23,24,25:256)
becomes <- c(NA,1,rep.int(0,3),2,rep.int(0,18),3,rep.int(0,232))
m<-cbind(is_m,becomes)

county_list<-list()
layer_list<-list()
for (county in 1:length(county_set_list)){
  county_r<-county_set_list[[county]]
  for(layer in 1:nlayers(county_r)){
    layer_list[[layer]] <- reclassify(county_r[[layer]], m)
  }
  county_list[[county]]<-layer_list
}



i=1
test_county<-county_list[[i]]
y<-test_county
s0 = brick(y)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer.3 <- data.frame(coords, layer.3)

testdf<-layer.3
testdf <- testdf[!grepl("NANANANANANANANANANANA", testdf$layer.3),] # remove pixels that have no crops in 11 years
#unique(testdf$layer.3)
testdf$f<-gsub("NA", "", testdf$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
testdf$field<-as.numeric(testdf$f) #turn f into fields as numeric integer
testdf$n_years<-as.numeric(gsub(0, "", testdf$field))




testdf$bin<-floor(log10(testdf$n_years)) + 1  #create column to count the number of years
#testdf$prop<-testdf$bin/11

thresh_layers<-testdf[testdf$bin >= (as.numeric(thresh$Var1) - 1),] 
thresh_layers<-na.omit(thresh_layers)


binned_prop<-split(thresh_layers, f=thresh_layers$bin)
testy<-binned_prop[[1]]

#testy$seq<-as.character(testy$n_years)




df_n<-thresh_layers[,c(1:2,7)] #if looking at unique bins 
df_n<-thresh_layers[,c(1:2,8)] #if looking at proportions of all

coordinates(df_n)<-~ x + y
gridded(df_n)<-TRUE
df_n<- raster(df_n)
crs(df_n) <- crs(cdl_data_ill_rec[[1]])
plot(df_n)

writeRaster(df_n, file.path(cdl_dir, "/r_prop"), format="GTiff", overwrite = TRUE)
