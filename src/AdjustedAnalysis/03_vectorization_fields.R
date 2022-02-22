### Probabilistic Crop Loading 

### 03 Vectorization of Fields

# Edited by E. Paulukonis Feb 2022
import_start_time <- Sys.time()
print("stepping into 03_vectorization_fields.R")



set1<-stack(cdl_data_ill_rec[c(1:2)])
set2<-stack(cdl_data_ill_rec[c(12:22)])


#Stack the rasters and convert to data frame
rast_df<- as.data.frame(set1)
colnames(rast_df)<-c("y1","y2")

#Concatenate using the layer stack (layer.1 for rast1, layer.2 for rast2)
rast_con<- transform(rast_df, s1=paste(y1, y2, sep = ""))

#Inspect layer.3 values
head(rast_con)

#convert to matrix
rast_mat<- as.matrix(as.numeric(as.character(rast_con$y1)))
#convert back to raster
rast_final<- raster(rast_mat, xmn=406065, xmx=727725, ymn=1688025, ymx=2194845)

plot(rast_final)
head(rast_mat)
