### Probabilistic Crop Loading 

###02 Formatting CDL data

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
#plot(study)
mask_crop_cdl<-function(cdl){
  cdl_list<-crop(cdl, study)
  mask(cdl_list, study)
}
cdl_fin_co<-lapply(cdl_fin, mask_crop_cdl)

 
#### Get accuracy and error data ####
#https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section3_22.0

#note: users will need to format and organize CDL accuracy and error as specified in the demo data provided
#if using code below
cdl_acc<-read.csv(paste0(cdl_acc_dir,"/CDL_Acc_il.csv"))
cdl_err<-read.csv(paste0(cdl_acc_dir,"/CDL_Err_il.csv"))
nnames<-c(names(cdl_acc)[1:2], 2007:2020)
print(nnames)
print(names(cdl_acc))
print(names(cdl_err))
names(cdl_acc)<-nnames
names(cdl_err)<-nnames

# round 
cdl_acc[,3:16]<-round(cdl_acc[,3:16], 3)
cdl_err[,3:16]<-round(cdl_err[,3:16], 3)

#we also need to average the LANDSAT AD tiles for 1999-2006
ad_data<-read.csv(paste0(cdl_acc_dir,"/cdl_19992006_IL.csv"))
# group CE by CDL class, year, and average the output value across AD tiles
ad_CE<-ad_data %>%
  group_by(CDL, Year) %>% summarize(CE_mean=mean(CE))
# spead the dataframe to match
ad_CE <- ad_CE %>% tidyr::spread(key = Year, value = CE_mean)
ad_CE[,2:9]<-round((ad_CE[,2:9]/100),3)

# do the same for UA
ad_UA<-ad_data %>%
  group_by(CDL, Year) %>% summarize(UA_mean=mean(UA))
ad_UA <- ad_UA %>% tidyr::spread(key = Year, value = UA_mean)
ad_UA[,2:9]<-round((ad_UA[,2:9]/100),3)


#rearrange in order of years
cdl_acc<-merge(cdl_acc, ad_UA, by.x = "Attribute_Code", by.y = "CDL")
cdl_acc<-cdl_acc[,c(1:2,17:24,3:16)]
cdl_err<-merge(cdl_err, ad_CE, by.x = "Attribute_Code", by.y = "CDL")
cdl_err<-cdl_err[,c(1:2,17:24,3:16)]


#instead of setting all NAs to 0.5, as in Budreski et al., we'll use the average of years to backfill missing values
correct_backfill<-function(data){
o<-data[,1:2]
m<-as.matrix(data[,3:24])
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
#cdl_fin_co_y<-cdl_fin_co[10:22] #match number of years for accuracy, for now
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
years<-1999:2020
for (i in 1:13){
plot_list[[i]]$Year <- years[i]
}

final_list<-do.call("rbind", plot_list)
final_rem<-final_list[!final_list$Percent < 1,]
final_rem$Year<-as.factor(final_rem$Year)


#jpeg("/work/HONEYBEE/eap/pollinator_probabilistic_loading/crop_graph.jpg", width = 350, height = "350")
#png(paste0("/work/HONEYBEE/eap/pollinator_probabilistic_loading/figures/crop_plot_by_year.png"))
  ggplot(final_rem, aes(Year,Percent, group=Crop, colour =  Crop)) +
    geom_line()+
    geom_point()+
    # coord_flip()+
    # scale_x_discrete(limits=rev)+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
          axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"))
  ggsave(paste0("/work/HONEYBEE/eap/pollinator_probabilistic_loading/figures/crop_plot_by_year.png"))


crop_list <-sort(unique(unlist(values_by_county, use.names=FALSE)))
names(crop_list)<-'crop_code'
crop_list_fin<-cdl_acc[cdl_acc$Attribute_Code %in% crop_list,1:2] #pull out the crops actually in our layer
codes<-crop_list_fin$Attribute_Code
numCores <- detectCores()
print(numCores)

reclassify_cdl<-function(cdl_data){
    for(y in 1999:2020){
      for(c in codes){
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


mclapply(cdl_fin_co_y, reclassify_cdl, mc.cores=numCores)




# #### Pull out the crop layers we're interested in ####



print("CDL formatted, reconstructed, and corrected for accuracy/error")



