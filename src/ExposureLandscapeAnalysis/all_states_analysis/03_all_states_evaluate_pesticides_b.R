### Probabilistic Crop Loading 

### 03 read in pesticide data

# Edited by E. Paulukonis Jan 2023

date <- '20220325' # date for aggregate estimates (bee toxic load)
cmp_date <- '20220325' # date for the compound-specific table 



all_states<-st_read(paste0('C:/Users/EPAULUKO/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/non-apis-diversity-exposure/data/cb_2018_us_state_500k.shp')) #read in states


#all_states@data
states<-all_states$STUSPS


#states<-range$STUSPS #get all states associated with RPBB
rec <- reclasstables(filepath = paste0(pest_dir, "/beecosP/beetox_I_cdl_reclass.", date, ".csv"),
                      state = "WI",
                      year = 2008:2021,
                      write_reclass = T,
                      outfolder = paste0(pest_dir, "/beecosP/output/reclasskeys/insecticidestest"))
# specify path to CDL raster file

test_imi <- reclasstables(filepath = paste0(pest_dir,"/beecosP/beetox_cmpd_cdl_reclass_", cmp_date, "/IMIDACLOPRID.csv"),
                          state = "WI",
                          year = 2008:2021,
                          write_reclass = T,
                          outfolder = paste0(pest_dir, "/beecosP/output/reclasskeys/imidaclopridtest"))


test_cloth <- reclasstables(filepath = paste0(pest_dir,"/beecosP/beetox_cmpd_cdl_reclass_", cmp_date, "/CLOTHIANIDIN.csv"),
                            state = "WI",
                            year = 2008:2021,
                          write_reclass = T,
                          outfolder = paste0(pest_dir, "/beecosP/output/reclasskeys/clothtest"))


#from our own reclassed datasets
cdl_path <- paste0(cdl_dir, "/BombusAnalysis/WI_2008.tif")

# contact toxicity for insecticides
CDL_reclass(rasterpath = cdl_path,
            reclasstable = rec$WI_2008,
            from = "value",
            to = "ld50_ct_ha_bil",
            writerast = TRUE,
            outpath = paste0(pest_dir, "/beecosP/output/rasters"),
            meanreclass = FALSE)

# imidacloprid
CDL_reclass(rasterpath = cdl_path,
            reclasstable = test_imi$WI_2008,
            from = "value",
            to = "kg_ha",
            writerast = TRUE,
            outpath = paste0(pest_dir, "/beecosP/output/rasters"),
            meanreclass = FALSE)



