### Probabilistic Crop Loading 

### 02 Formatting CDL data

# Edited by E. Paulukonis Sept 2021
import_start_time <- Sys.time()
print("stepping into 02_formatting_cdl.R")

## Multistate-Specific



### Illinois-Specific 
cdl_rec_filename<-paste0(cdl_dir_rec, "some file")
# if(file.exists(cdl_rec_filename)){
  
  print(list.files(path=cdl_dir_rec, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_rec <- file.path(cdl_dir_rec, list.files(path=cdl_dir_rec, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  cdl_data_rec<-lapply(cdl_data_rec, stack) #create list of reclassed and stacked cdl rasters 
  
# }else{

#### Import and modify files ####
print(list.files(path=cdl_ill_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
print(cdl_ill_dir)
cdl_data <- file.path(cdl_ill_dir, list.files(path=cdl_ill_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
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

#this section is for writing the new rasters out with a new projection and extent; it is commented out because we've already done this section

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

#first 7 are 1999-2005
#last 2 are 2006-2007

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
# plot(study)
mask_crop<-function(x){
  r_list<-crop(x, study)
  mask(r_list, study)
}
cdl_fin_co<-lapply(cdl_fin, mask_crop)
 

cdl_fin[[1]]



#First, get list of actual crop codes from all attribute layers
#cdl_fin_co_y<-cdl_fin_co[10:22] #if you need a specific subset of years
cdl_fin_co_y<-cdl_fin_co
out<-list()
for (i in 1:length(cdl_fin_co_y)){
  out[[i]]<-sort(unique(values(cdl_fin_co_y[[i]])))
}

#create list of crops, sort
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

#create function to make simple plot of crop percents
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

#create list of individual plots of crops for each year
plot_list<-list()
for(i in 1:length(out)){
  df<-out[[i]]
  plot_list[i]<-plot_data_column(df)
}

finished_plots<-lapply(plot_list, plot_data_column)
years<-1999:2020
for (i in 1:22){
plot_list[[i]]$Year <- years[i]
}

print(plot_list)
final_list<-do.call("rbind", plot_list)
final_rem<-final_list[!final_list$Percent < 1,]
final_rem$Year<-as.factor(final_rem$Year)
final_rem$Year<-as.factor(final_rem$Year)

#save plot of crop proportion by year, combined into one
  ggplot(final_rem, aes(Year,Percent, group=factor(Crop), colour =  factor(Crop))) +
    geom_line()+
    geom_point()+
    # coord_flip()+
    # scale_x_discrete(limits=rev)+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"))
  ggsave(paste0("/work/HONEYBEE/eap/pollinator_probabilistic_loading/figures/crop_plot_by_year.png"))

#run reclassification function over our area codes within all years
numCores <- detectCores()
print(numCores)

crop_final<-unique(final_rem$Crop) #pull out set of final crops we're interested in
write.csv(crop_final, paste0(cdl_dir_rec,"/cdl_crop_list_final.csv"))

reclassify_cdl<-function(cdl_data){
    for(y in 1999:2020){
      for(c in crop_final){
   cdl <- cdl_data #get the CDL raster by year
    values(cdl)[values(cdl)!=c]<-0 #set any values that are not crop to 0
    acc<-as.matrix(cdl_acc%>%select("Attribute_Code",paste0(y))) #get the accuracy data for year y
    err<-as.matrix(cdl_err%>%select("Attribute_Code",paste0(y))) #get the error data for year y
    file_a<-reclassify(cdl, acc, right=NA) #reclassify the crop raster, so the cells are set to the crop's accuracy value
    file_e<-reclassify(cdl, err, right=NA) #reclassify the crop raster, so the cells are set to the crop's error value
    acc_stack<-stack(cdl, file_a, file_e) #make a stack
    fl<-paste0("cdl", "_",y,"_",c,"_stack.tif") #set up the new file name, based on y and c
    writeRaster(acc_stack,  paste(cdl_dir_rec,"/",fl, sep=""), format="GTiff", overwrite=T) #save the raster stack
      }}
}

#apply over all available cores
mclapply(cdl_fin_co_y, reclassify_cdl, mc.cores=numCores)

# }

print("each raster stack contains 3 layers, presence, CDL user's accuracy, and CDL commission error")
print("we created rasters for all crops covering > 1 of the total crop area")
print("CDL formatted, reconstructed, and corrected for accuracy/error")



