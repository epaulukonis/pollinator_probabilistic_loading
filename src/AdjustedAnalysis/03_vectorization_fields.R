### Probabilistic Crop Loading 

### 03 Vectorization of Fields

# Edited by E. Paulukonis Feb 2022
import_start_time <- Sys.time()
print("stepping into 03_vectorization_fields.R")


plot(cdl_data_ill_rec[[22]])



#Create two rasters
xmn = 1; xmx = 5; ymn = 10; ymx = 20 
rast1<- raster(matrix(1,3,4), xmx=xmx, xmn=xmn, ymx=ymx, ymn=ymn)
rast2<- raster(matrix(2,3,4), xmx=xmx, xmn=xmn, ymx=ymx, ymn=ymn)

plot(rast1)
plot(rast2)


#Stack the rasters and convert to data frame
rast_df<- as.data.frame(stack(rast1, rast2))

#Concatenate using the layer stack (layer.1 for rast1, layer.2 for rast2)
rast_con<- transform(rast_df, layer.3=paste(layer.1, layer.2, sep = ""))

#Inspect layer.3 values
head(rast_con)

#convert to matrix
rast_mat<- as.matrix(as.numeric(as.character(rast_con$layer.3)))
#convert back to raster
rast_final<- raster(rast_elf, xmx=xmx, xmn=xmn, ymx=ymx, ymn=ymn)