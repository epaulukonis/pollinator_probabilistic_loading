### Probabilistic Crop Loading 

###01 Formatting CDL data

#Edited by E. Paulukonis Sept 2021
import_start_time <- Sys.time()
print("stepping into 02_formatting_cdl.R")

#### Import and modify files ####
print(list.files(path=cdl_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(cdl_dir)
cdl_data <- file.path(cdl_dir, list.files(path=cdl_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_data<-lapply(cdl_data, raster) #create list of cdl rasters 

#there are 3 different extents; 2020-2008, 2007-2006, and 2005-1999
# first expand extent 
ext<-extent(cdl_data[[22]])
cdl_data<-lapply(cdl_data, function(x) setExtent(x, ext))

# now we'll resample to fix the number of rows and columns in several of the later years
#get 1999-2005
set_1<-cdl_data[c(1:7)]
#get 2006-2007
set_2<-cdl_data[c(8:9)]
#get rest
cdl_base<-cdl_data[c(10:22)]

# r1<-cdl_data[[22]]
# out_1<-lapply(set_1, function(file){
#   projectRaster(file, r1, method='ngb',crs(r1))
# })
# for (i in 1:length(out_1)){
#   writeRaster(out_1[[i]], filename = file.path(cdl_dir, "cdl_final1.tif"), by_layer=T, format="GTiff")
# }
# out_2<-lapply(set_2, function(file){
#   projectRaster(file, r1, method='ngb',crs(r1))
# })
# for (i in 1:length(out_2)){
#   writeRaster(out_2[[i]], filename = file.path(cdl_dir, "cdl_final2.tif"), by_layer=T, format="GTiff")
# }

print(list.files(path=cdl_dir_fin, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_f<-file.path(cdl_dir_fin,
                  list.files(path=cdl_dir_fin, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_f<-lapply(cdl_f, raster) #list new projected/fixed rasters


#assign original year names 
list_names<-vector()
for (n in 1:22){
  list_names[n]<-names(cdl_data[[n]])
}
cdl_fin<-c(cdl_f, cdl_base)
names(cdl_fin)<-list_names

#make function mask and crop to study area
# co <- "PEORIA"
# study<-state[state$COUNTY_NAM == co,]
mask_crop_cdl<-function(cdl){
  cdl_list<-crop(cdl, study)
  mask(cdl_list, study)
}
cdl_fin_co<-lapply(cdl_fin, mask_crop_cdl)


# when you get to the point where we'll do it for all counties, follow this:
# county_names<-state$COUNTY_NAM
# cdl_fin_co<-list()
# mask_crop_cdl<-function(cdl){
#   for (c in 1:length(county_names)){
#   county<-state[state$COUNTY_NAM == county_names[c],]
#   county_c<-crop(cdl, county)
#   cdl_fin_co[c]<-mask(county_c, county)
# }}
# you'll have a nested list of stacked rasters by county

 
#### Get accuracy and error data ####
#https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section3_22.0

#note: users will need to format and organize CDL accuracy and error as specified in the demo data provided
#if using code below
cdl_acc<-read.csv(paste0(cdl_acc_dir,"/CDL_Acc_il.csv"))
cdl_err<-read.csv(paste0(cdl_acc_dir,"/CDL_Err_il.csv"))
nnames<-2008:2020
names(cdl_acc)[3:15]<-nnames
names(cdl_err)[3:15]<-nnames

#instead of setting all NAs to 0.5, as in Budreski et al., we'll use the average of years to backfill missing values
correct_backfill<-function(data){
o<-data[,1:2]
m<-as.matrix(data[,3:15])
k <- which(is.na(m), arr.ind=TRUE)
m[k] <- rowMeans(m, na.rm=TRUE)[k[,1]]
m<-as.data.frame(m)
m<-round(m, 2)
out<-cbind(o,m)
}

cdl_acc<-correct_backfill(cdl_acc)
cdl_err<-correct_backfill(cdl_err)
print(head(cdl_acc))
print(head(cdl_err))

#First, get list of actual crop codes from all attribute layers
cdl_fin_co_y<-cdl_fin_co[10:22] #match number of years for accuracy, for now
out<-list()
for (i in 1:length(cdl_fin_co_y)){
  out[[i]]<-sort(unique(values(cdl_fin_co_y[[i]])))
}
crop_list<- sort(unique(unlist(out, use.names=FALSE)))
names(crop_list)<-'crop_code'
crop_list_fin<-cdl_acc[cdl_acc$Attribute_Code %in% crop_list,1:2] #pull out the crops actually in our layer
codes<-crop_list_fin$Attribute_Code
print(codes)

#let's look at the percent of each crop's contribution to the study area for each year

calculate_percent<-function(x){
  x[!x == crop_list_fin$Attribute_Code]<-NA
  x[x == 0]<-NA
  df <- as.data.frame(table(as.matrix(x)))
  df$Percent <- round(df$Freq / sum(df$Freq) * 100,3)
  df
}

out<-lapply(cdl_fin_co_y, calculate_percent)

for (y in 1:length(out)){
  out[[y]]$Crop<-crop_list_fin[crop_list_fin$Attribute_Code %in% out[[y]]$Var1,1] 
}

#create function to make simple plot
plot_data_column = function (data) {
  ggplot(data, aes(x = Crop, y = Percent, fill=Crop)) + 
    geom_bar(stat = "identity")+
    coord_flip()+
    scale_x_discrete(limits=rev)+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
          legend.position = "none")
}

plot_list<-list()
for(i in 1:length(out)){
  df<-out[[i]]
  plot_list[i]<-plot_data_column(df)
}
finished_plots<-lapply(plot_list, plot_data_column)
years<-2008:2020
for (i in 1:13){
  plot_list[[i]]$Year <- years[i]
}


final_list<-do.call("rbind", plot_list)
final_rem<-final_list[!final_list$Percent < 1,]

final_rem$Year<-as.factor(final_rem$Year)


  ggplot(final_rem, aes(Year,Percent, group=factor(Crop), colour =  factor(Crop))) + 
    geom_line()+
    geom_point()+
    # coord_flip()+
    # scale_x_discrete(limits=rev)+
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"))


#create more indices on x axis

#when it's time to loop this over counties, try this:
# get_regional_crops<-function(county){
# out<-list()
# for (i in 1:length(county)){
#   out[[i]]<-sort(unique(values(county[[i]])))
# }}
# 
# values_by_county<-lapply(cdl_fin_co_y, get_regional_crops)
# crop_list <-sort(unique(unlist(values_by_county, use.names=FALSE)))
# names(crop_list)<-'crop_code'
# crop_list_fin<-cdl_acc[cdl_acc$Attribute_Code %in% crop_list,1:2] #pull out the crops actually in our layer
# codes<-crop_list_fin$Attribute_Code
numCores <- detectCores()
print(numCores)
# 
# reclassify_cdl<-function(cdl_data){
#     for(y in 2008:2020){
#       for(c in codes){
#    cdl <- cdl_data #get the CDL raster by year
#     values(cdl)[values(cdl)!=c]<-0 #set any values that are not crop to 0
#     acc<-as.matrix(cdl_acc%>%select("Attribute_Code",paste0(y))) #get the accuracy data for year y
#     err<-as.matrix(cdl_err%>%select("Attribute_Code",paste0(y))) #get the error data for year y
#     file_a<-reclassify(cdl, acc, right=NA) #reclassify the crop raster, so the cells are set to the crop's accuracy value
#     file_e<-reclassify(cdl, err, right=NA) #reclassify the crop raster, so the cells are set to the crop's error value
#     acc_stack<-stack(cdl, file_a, file_e) #make a stack
#     fl<-paste0("cdl", "_",y,"_",c,"_stack.tif") #set up the new file name, based on y and c
#     writeRaster(acc_stack,  paste(cdl_dir_rec,"/",fl, sep=""), format="GTiff", overwrite=T) #save the raster stack
#       }}
# }
# 
# 
# mclapply(cdl_fin_co_y, reclassify_cdl, mc.cores=numCores)

#when time comes to test over counties, try this out:
# foreach(county = length(cdl_fin_co_y)) %do% {
#   lapply(cdl_fin_co_y[[county]], reclassify_cdl, mc.cores=numCores)
# }



# #### Reclassify the adjusted CDL crops ####
#We need to combine double crops and adjust the codes for crops as needed
#note that this may be dependent on location; look to the compiled Accuracy data-set to learn more

#the new attribute codes for the CDL crops will depend on the region and county; therefore, we create code to semi-automatically reclassify
#old_crops<-codes
# new_code<-1:length(old_crops)
# old_non_dbl <- crop_list_fin[!grepl("Dbl", crop_list_fin$Cover_Type), ]
# dbl <- crop_list_fin[grepl("Dbl", crop_list_fin$Cover_Type), ]
# 



# for(y in 2008:2020){
#   for(c in codes){
#     rast<-stack(paste0(cdl_dir_rec, "/CDL", co, "_",y,"_",c,"_stack.tif"))
#     val<-df$new[df$old==c]
#     fl<-paste0(co,"_",2008,"_",val,"_stack.tif")
#     writeRaster(rast, paste(cdl_dir_adj, fl, sep="/"), format="GTiff", overwrite=T)
#   }
# }


test_1<-stack(paste0(cdl_dir_rec, "/cdl", "_",2008,"_",1,"_stack.tif"))
plot(test_1)

test_30<-stack(paste0(cdl_dir_rec, "/cdl", "_",2017,"_",30,"_stack.tif"))
plot(test_30)


#Re-classify the rasters which needed recombination
#Dbl crop rasters are NOT mutually exclusive (e.g. dbl crop lettuce/durum wht is included in both lettuce and durum wheat)

#Corn: New=1, Old=1, 225, 226, 228, 237, 241
# for(k in 2013:2017){
#   layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_1_stack.tif"))
#   layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_225_stack.tif"))
#   layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_226_stack.tif"))
#   layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_228_stack.tif"))
#   layer5<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_237_stack.tif"))
#   layer6<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_241_stack.tif"))
#   pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], layer5[[1]], layer6[[1]], na.rm=T)
#   acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], layer5[[2]], layer6[[2]], na.rm=T)
#   err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], layer5[[3]], layer6[[3]], na.rm=T)
#   rast<-stack(pres, acc, err)
#   fl<-paste0(co,"_",k,"_1_stack.tif")
#   writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
# }




print("CDL formatted, reconstructed, and corrected for accuracy/error")

#test locally here: for Tom
#test out on a list of 2
# cdl_fin_co_t<-cdl_fin_co[10:11]
# reclassify_cdl<-function(cdl_data){
#   for(c in codes){
#     for(y in 2008:2009){
#       cdl <- cdl_data #get the CDL raster by year
#       values(cdl)[values(cdl)!=c]<-0 #set any values that are not crop to 0
#       acc<-as.matrix(cdl_acc%>%select("Attribute_Code",paste0(y))) #get the accuracy data for year y
#       err<-as.matrix(cdl_err%>%select("Attribute_Code",paste0(y))) #get the error data for year y
#       file_a<-reclassify(cdl, acc, right=NA) #reclassify the crop raster, so the cells are set to the crop's accuracy value
#       file_e<-reclassify(cdl, err, right=NA) #reclassify the crop raster, so the cells are set to the crop's error value
#       acc_stack<-stack(cdl, file_a, file_e) #make a stack
#       fl<-paste0(co, "_",y,"_",c,"_stack.tif") #set up the new file name, based on y and c
#       writeRaster(acc_stack,  paste(cdl_dir_rec,"/",fl, sep=""), format="GTiff", overwrite=T) #save the raster stack
#     }}}
# 
# 
# mclapply(cdl_fin_co_t, reclassify_cdl, mc.cores=numCores)


