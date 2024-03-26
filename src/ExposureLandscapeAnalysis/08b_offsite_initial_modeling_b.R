### Probabilistic Crop Loading 

### 08b Initial Offsite Model Outputs

# Edited by E. Paulukonis August 2023
library(stringdist)
library(cowplot)
library(tidyverse)
library(ggh4x)
##### Read in data for models ----
## It's important to remember that offsite, we are basically assuming wildflower/foraging habitat
#### App rates and timing:
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)


## Because we're dealing with off-field plants that have both nectar and pollen, here, we'll impute 'pollen and nectar' for each combo of compounds
library(tidyr)
apprates<-crossing(apprates, type=c("Pollen","Nectar"))
apprate_table <- apprates %>%
  gt()
apprate_table
apprate_table_filename_html <- file.path(root_figures_parameters,"bombus_apprate_table.html")
apprate_table_html <- as.character(htmltools::save_html(htmltools::as.tags(apprate_table), apprate_table_filename_html))

#saveWidget(as_widget(apprate_table), file = apprate_table_filename_html, selfcontained = F)
#jpeg(apprate_table_filename, width = 8, height = 11, units = "in", res=300)
#  as_widget(apprate_table)
#dev.off()



#### Set up and read in variables for models:
## Li
Li<-read.csv(paste0(pest_dir,"/Models/LiParam.csv"))
Li<-Li %>% spread(Parameter, Value)
Li$Compound<-toupper(Li$Compound)
li_table <- Li %>%
  gt()
li_table
li_table_filename_html <- file.path(root_figures_parameters,"bombus_li_table.html")
li_table_html <- as.character(htmltools::save_html(htmltools::as.tags(li_table), li_table_filename_html))

## Briggs
#defaults
theta<-0.2
foc<-0.01
bulkdensity<-1.5
## Purucker and Paulukonis
#elimination rate via growth
kel_grow<-0.035
#relative growth rate
rgr_corn<-0.066
rgr_soy<-0.06
#mass of seed (g)
m0_corn<-0.25
m0_soy<-0.15


#partition coeff
Fpl<-c(0.35,0.69, 0.69)
Fnl<-c(0.017,0.05, 0.05)
partcoeffn<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM"), (Fnl),paste0("Nectar")))
names(partcoeffn)<-c("Compound","Fr","Type")
partcoeffp<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM"), (Fpl),paste0("Pollen")))
names(partcoeffp)<-c("Compound","Fr","Type")
part_coeff<-rbind(partcoeffp, partcoeffn)
part_coeff
li_table <- Li %>%
  gt()
li_table
li_table_filename_html <- file.path(root_figures_parameters,"bombus_li_table.html")
li_table_html <- as.character(htmltools::save_html(htmltools::as.tags(li_table), li_table_filename_html))


#### Set up empty matrix to hold outputs; these will auto-populate as you go through each application type
#first, we will create our empty df to populate the on-field concentrations by day
off_field_residues<-as.data.frame(matrix(NA,nrow = 151, ncol = 0))
off_field_residues[,1]<-0:150
colnames(off_field_residues)[1]<-"day"

#put all combos into a list where the name represents the scenario
off_field_residue_list<-rep(list(off_field_residues), 18L)
unique_names<-apprates %>% distinct(Compound,ApplicationType, Commodity)
names(off_field_residue_list)<-with(unique_names, paste0(Compound,"_",ApplicationType,"_",Commodity))


######## Analysis ###########
#### Seed Applications ----
## Dust ----
#these are the only residues we'll measure as ug/m2 
#First, we will derive the fraction of dust moving across and off the field 
d<-0:299*0.3048 # first, convert the distance to meters
x<-exp(1.68-0.0905*log(d))-1 # estimate the fraction 
orig_conc_at_distance<-(x/1875)*1000 #ng/mm2 to ug/m2 is a 1000 unit conversion


#original conc in field, from Krupke
conc_in_field<-((2.06+11.92)/2)*1000#(ug/m2) # average application rate

#to translate this relationship to other in-field concentrations, 
#I assume that the fraction of off-site deposition based on the original concentration is reflective of all seed treatments
deposition<-(orig_conc_at_distance/conc_in_field) #fraction of off
air_krupke<-as.data.frame(cbind(d,orig_conc_at_distance, deposition))
#avg_frac<-(0.001849+0.000319)/2
avg_frac<-3.81/conc_in_field

#add in the initial estimate of the concentration at 0 (in-field)
air_krupke<-rbind(c(0,3.81,avg_frac), air_krupke)

#air krupke represents a dataframe with information about the on and off-field estimate of deposition from seeds, based on the relationship
#between the concentration in field in ug/m2 and the amount reported offsite. we then use these deposition fractions to estimate the same relationship for other compounds

#visualize the original concentration, if desired
p <- ggplot(air_krupke, aes(d, deposition*100)) +
  geom_point()+
  geom_line()+
  ylab("Deposition (% of In-Field Concentration)")+
  xlab("Distance[m]")+
  theme_bw()
p

#get the seed treatment data and estimate concentration in dust for each seed type
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]

#for dust, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
seed_treatments<-seed_treatments[!seed_treatments$type == "Nectar",]
seed_treatments<-seed_treatments[ , !(names(seed_treatments) %in% "type")]

seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_dust_conc<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_dust_conc<-seed_dust_conc[with(seed_dust_conc, order(Compound,Commodity)), ] # order by compound, commodity
seed_dust_conc$dust_drift_conc<-seed_dust_conc$ug_m2*seed_dust_conc$deposition #multiply application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
seed_dust_datasets<-split(seed_dust_conc, list(seed_dust_conc$Compound, seed_dust_conc$ApplicationType, seed_dust_conc$Commodity), drop=T) 

#here, we will estimate the concentration in ug per g

daily_conc_off_field<-list()
for(compound_and_crop in 1:length(seed_dust_datasets)){
  seed_dust_data_off_field<-seed_dust_datasets[[compound_and_crop]]
  seed_dust_data_off_field<- seed_dust_data_off_field[!seed_dust_data_off_field$d == 0,]
  seed_dust_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_dust_data_off_field$dust_drift_conc #retain as ug/m2
  seed_dust_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_dust_data_off_field$Dust_concentration_ug_m2_from_seed) #get mean
  
  seed_dust_data_off_field<-  seed_dust_data_off_field[1,]
  seed_dust_data_off_field<-seed_dust_data_off_field[rep(seq_len(nrow(seed_dust_data_off_field)), 151),]
  seed_dust_data_off_field[2:151, 18]<-0
  seed_dust_data_off_field$day<-0:150
  daily_conc_off_field[[compound_and_crop]]<-seed_dust_data_off_field[,c(1,5,6,18,19)]
  
}

#first put OG names 
names(daily_conc_off_field)<-names(seed_dust_datasets)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)


## Soil----
#get the seed treatment data and estimate soil concentration in dust for each seed type
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]

#for soil, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
seed_treatments<-seed_treatments[!seed_treatments$type == "Nectar",]
seed_treatments<-seed_treatments[ , !(names(seed_treatments) %in% "type")]

seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_soil_conc<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_soil_conc<-merge(seed_soil_conc, Li[ , c("Compound","KdissSoil")], by = "Compound", all.x=TRUE) # join specifically to get dissipation rate of compound in soil
seed_soil_conc<-seed_soil_conc[with(seed_soil_conc, order(Compound,Commodity)), ] # order by compound, commodity
seed_soil_conc$dust_drift_conc<-seed_soil_conc$ug_m2*seed_soil_conc$deposition #multiply application rate times deposition curve 

#split each set of commodity and compound data into a unique dataframe
seed_soil_datasets<-split(seed_soil_conc, list(seed_soil_conc$Compound, seed_soil_conc$Commodity), drop=T) 


#get PWC set for seed
pwc_set<-PWC_data_list[grepl('seed', names(PWC_data_list))]
names(pwc_set)<-toupper(names(pwc_set))



#here, we will estimate the concentration in ug per g, using both dust and amount contained in soil
daily_conc_off_field<-list()
for(compound_and_crop in 1:length(seed_soil_datasets)){
  seed_soil_data_off_field<-seed_soil_datasets[[compound_and_crop]]
  seed_soil_data_off_field<- seed_soil_data_off_field[!seed_soil_data_off_field$d == 0,]
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed) # get average dust deposition
  
  #we can drop all rows except 1 because we've averaged the deposition
  seed_soil_data_off_field<-seed_soil_data_off_field[1,]
  seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
  seed_soil_data_off_field$day<-0:150
  seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
  
  #first, get the concentration from the average deposition
  conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95)*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(seed_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
   pwc[126:276,]
  } else {
     pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-pwc$soil_concentration_ug_g
  
  #loop to add new daily flux outputs to existing soil concentration
  for(i in 2:nrow(seed_soil_data_off_field)){
      seed_soil_data_off_field[i,7]<-seed_soil_data_off_field[i,7] + seed_soil_data_off_field[i-1,7]*exp(0-seed_soil_data_off_field[i,4] * seed_soil_data_off_field[i,6]) 
    }
    
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed <- conc_from_dust + seed_soil_data_off_field$Soil_concentration_ug_g_from_seed
  daily_conc_off_field[[compound_and_crop]]<-seed_soil_data_off_field[,c(1,2,3,6,7)] #add compound
  
}



#first put OG names 
names(daily_conc_off_field)<-names(seed_soil_datasets)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)


## Pollen and Nectar----
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]
seed_treatments$ug_m2<-seed_treatments$AvgRate*112085 # convert to ug/m2
seed_treatments<-merge(seed_treatments,air_krupke) #merge with the fraction estimated via Krupke
seed_treatments<-seed_treatments[with(seed_treatments, order(Compound,Commodity)), ] # order by compound, commodity
seed_treatments$dust_drift_conc<-seed_treatments$ug_m2*seed_treatments$deposition #multiple application rate times deposition curve 

seed_treatments<-seed_treatments[,!names(seed_treatments) %in% "Type"]
#seed_treatments<-merge(seed_treatments, part_coeff, by="Compound")
seed_nectarpollen_conc<-merge(seed_treatments,Li) #merge with the fraction estimated via Krupke


##because we have both nectar and pollen, we need to split up the sets into two batches
seed_treatments_pollen<-seed_nectarpollen_conc[seed_nectarpollen_conc$type == "Pollen",]
seed_treatments_nectar<-seed_nectarpollen_conc[seed_nectarpollen_conc$type == "Nectar",]

#get PWC set for seed
pwc_set<-PWC_data_list[grepl('seed', names(PWC_data_list))]
names(pwc_set)<-toupper(names(pwc_set))



## Pollen
#split by compound, crop, and type (nectar or pollen)
seed_pollen_datasets<-split(seed_treatments_pollen, list(seed_treatments_pollen$Compound, seed_treatments_pollen$ApplicationType, seed_treatments_pollen$Commodity), drop=T)

#x<-seed_pollen_datasets[[1]]

residues_in_nectar_and_pollen_from_seed<-function(x){

# get the deposition and soil concentrations first
  seed_soil_data_off_field<- x[!x$d == 0,]
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed) # get average dust deposition
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  seed_soil_data_off_field<-seed_soil_data_off_field[1,]
  seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
  seed_soil_data_off_field$day<-0:150
  # seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
  
  #first, get the concentration from the average deposition
  conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95)#*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(seed_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-pwc$soil_concentration_ug_g
 
  #loop to add new daily flux outputs to existing soil concentration; but remove decay component here
  for(i in 2:nrow(seed_soil_data_off_field)){
    seed_soil_data_off_field[i,38]<-seed_soil_data_off_field[i,38] + seed_soil_data_off_field[i-1,38] #*exp(0-seed_soil_data_off_field[i,11] * seed_soil_data_off_field[i,38]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-conc_from_dust + seed_soil_data_off_field$Soil_concentration_ug_g_from_seed
  
  
  output<- matrix(data=0, nrow=151, ncol=2)
  #n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-1:150
  output[2:151,2]<-t
  for(i in 1:nrow(output)){
    # units will be ug per g of pollen
    # first get uptake from soil
    pol<-seed_soil_data_off_field[i,]
    uptakesoil <-( (pol$`Kp-L`* pol$Soil_concentration_ug_g_from_seed) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
 
    #then estimate RUD due to leaf deposition
    uptakeleaf<-( ((pol$`Kp-L`*pol$fsurface*pol$cf1)/(pol$LAI*pol$LMA*(1/(1-pol$Wleaf))*1000)) * (pol$KupSurface/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSurface)) * ( exp(-pol$KdissSurface*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) * (pol$Dust_concentration_ug_m2_from_seed/100000) # sub dust as AR in kg/ha
    output[i,1]<- (uptakesoil + uptakeleaf) 
    
  }
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(pol$type,'_concentration_ug_g_from_seed'),"day")

  output
  
}
 

#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(seed_pollen_datasets,residues_in_nectar_and_pollen_from_seed)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)


## Nectar
#split by compound, crop, and type (nectar or pollen)
seed_nectar_datasets<-split(seed_treatments_nectar, list(seed_treatments_nectar$Compound, seed_treatments_nectar$ApplicationType, seed_treatments_nectar$Commodity), drop=T)

#x<-seed_nectar_datasets[[1]]

residues_in_nectar_and_pollen_from_seed<-function(x){
  # get the deposition and soil concentrations first
  seed_soil_data_off_field<- x[!x$d == 0,]
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-seed_soil_data_off_field$dust_drift_conc #retain as ug/m2
  seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed<-mean(seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed) # get average dust deposition
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  seed_soil_data_off_field<-seed_soil_data_off_field[1,]
  seed_soil_data_off_field<-seed_soil_data_off_field[rep(seq_len(nrow(seed_soil_data_off_field)), 151),]
  seed_soil_data_off_field$day<-0:150
  # seed_soil_data_off_field<-seed_soil_data_off_field[,c(1,5,6,12,19,20)]
  
  #first, get the concentration from the average deposition
  conc_from_dust<-((seed_soil_data_off_field$Dust_concentration_ug_m2_from_seed * (1/10000))/2 * 1/1.95) #*exp(-(seed_soil_data_off_field$k_values*seed_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(seed_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-pwc$soil_concentration_ug_g
  
  #loop to add new daily flux outputs to existing soil concentration; but remove decay component here
  for(i in 2:nrow(seed_soil_data_off_field)){
    seed_soil_data_off_field[i,38]<-seed_soil_data_off_field[i,38] + seed_soil_data_off_field[i-1,38] #*exp(0-seed_soil_data_off_field[i,11] * seed_soil_data_off_field[i,38]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
  seed_soil_data_off_field$Soil_concentration_ug_g_from_seed<-conc_from_dust + seed_soil_data_off_field$Soil_concentration_ug_g_from_seed

  
  output<- matrix(data=0, nrow=151, ncol=2)
  #n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-1:150
  output[2:151,2]<-t
  for(i in 1:nrow(output)){
    # units will be ug per g of pollen
    # first get uptake from soil
    nec<-seed_soil_data_off_field[i,]
    uptakesoil <-( (nec$`Kn-L` * nec$Soil_concentration_ug_g_from_seed) * (nec$KupSoil/(nec$KelAir+nec$KelDeg+nec$KelGrow - nec$KdissSoil)) * (exp(-nec$KdissSurface*(i)) - exp(-(nec$KelAir+nec$KelDeg+nec$KelGrow)*i)) ) 
    
    #then estimate RUD due to leaf deposition
    uptakeleaf<-( ((nec$`Kn-L`*nec$fsurface*nec$cf1)/(nec$LAI*nec$LMA*(1/(1-nec$Wleaf))*1000)) * (nec$KupSurface/(nec$KelAir+nec$KelDeg+nec$KelGrow - nec$KdissSurface)) * ( exp(-nec$KdissSurface*(i)) - exp(-(nec$KelAir+nec$KelDeg+nec$KelGrow)*i)) ) * (nec$Dust_concentration_ug_m2_from_seed/100000) # sub dust as AR in kg/ha
    output[i,1]<- (uptakesoil + uptakeleaf)
    
  }
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(nec$type,'_concentration_ug_g_from_seed'),"day")
  
  output
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(seed_nectar_datasets,residues_in_nectar_and_pollen_from_seed)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)




#### Soil Applications----
##Soil----
#get the seed treatment data and estimate soil concentration in dust for each seed type
soil_treatments<-apprates[apprates$ApplicationType == "Soil",]
soil_treatments<-soil_treatments[,!names(soil_treatments) %in% "Type"]

#for soil, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
soil_treatments<-soil_treatments[!soil_treatments$type == "Nectar",]
soil_treatments<-soil_treatments[ , !(names(soil_treatments) %in% "type")]


soil_treatments$kg_ha<-soil_treatments$AvgRate*1.12085 #convert original application rate to kg_ha for the soil uptake compartment 
soil_soil_datasets<-split(soil_treatments, list(soil_treatments$Compound, soil_treatments$ApplicationType,soil_treatments$Commodity), drop=T)


#get PWC set for seed
pwc_set<-PWC_data_list[grepl('soil', names(PWC_data_list))]
names(pwc_set)<-toupper(names(pwc_set))


#here, we will estimate the concentration in ug per g, using amount contained in soil
daily_conc_off_field<-list()
for(compound_and_crop in 1:length(soil_soil_datasets)){
  soil_soil_data_off_field<-soil_soil_datasets[[compound_and_crop]]
 
  #we can drop all rows except 1 because we've averaged the deposition
  soil_soil_data_off_field<-soil_soil_data_off_field[rep(seq_len(nrow(soil_soil_data_off_field)), 151),]
  soil_soil_data_off_field$day<-0:150
  soil_soil_data_off_field<-soil_soil_data_off_field[,c(1,4,5,11,13)]
  
   
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(soil_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  soil_soil_data_off_field$Soil_concentration_ug_g_from_soil<-pwc$soil_concentration_ug_g
  
  for(i in 2:nrow(soil_soil_data_off_field)){
    soil_soil_data_off_field[i,6]<-soil_soil_data_off_field[i,6] + soil_soil_data_off_field[i-1,6]*exp(0-soil_soil_data_off_field[i,4] * soil_soil_data_off_field[i,5]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff

  daily_conc_off_field[[compound_and_crop]]<-soil_soil_data_off_field[,c(1,2,3,5,6)] #add compound
  
}


#first put OG names 
names(daily_conc_off_field)<-names(soil_soil_datasets)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list, which will automatically repopulate back to the matrix we set up. 
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)
#testy<-off_field_residue_list[[1]]

## Pollen and Nectar----
#get the seed treatment data and estimate soil concentration in dust for each seed type
soil_treatments<-apprates[apprates$ApplicationType == "Soil",]
soil_treatments<-soil_treatments[,!names(soil_treatments) %in% "Type"]
soil_treatments<-merge(soil_treatments,Li) 
soil_treatments$kg_ha<-soil_treatments$AvgRate*1.12085 #convert original application rate to kg_ha for the soil uptake compartment 

#split into df with pollen or nectar
soil_treatments_pollen<-soil_treatments[soil_treatments$type == "Pollen",]
soil_treatments_nectar<-soil_treatments[soil_treatments$type == "Nectar",]

#split by nectar/pollen
soil_pollen_datasets<-split(soil_treatments_pollen, list(soil_treatments_pollen$Compound, soil_treatments_pollen$ApplicationType, soil_treatments_pollen$Commodity), drop=T)
soil_nectar_datasets<-split(soil_treatments_nectar, list(soil_treatments_nectar$Compound, soil_treatments_nectar$ApplicationType, soil_treatments_nectar$Commodity), drop=T)

#get PWC set for seed
pwc_set<-PWC_data_list[grepl('soil', names(PWC_data_list))]
names(pwc_set)<-toupper(names(pwc_set))

#x<-soil_pollen_datasets[[1]]
## Pollen
 residues_in_pollen_from_soil<-function(x){

  #we can drop all rows except 1 because we've averaged the deposition
  soil_pollen_data_off_field<-x[rep(seq_len(nrow(x)), 151),]
  soil_pollen_data_off_field$day<-0:150

  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(soil_pollen_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  soil_pollen_data_off_field$Soil_concentration_ug_g_from_soil<-pwc$soil_concentration_ug_g
  
#loop to add new daily flux outputs to existing soil concentration; but remove decay component here
  for(i in 2:nrow(soil_pollen_data_off_field)){
    soil_pollen_data_off_field[i,33]<-soil_pollen_data_off_field[i,33] + soil_pollen_data_off_field[i-1,33] #*exp(0-seed_soil_data_off_field[i,11] * seed_soil_data_off_field[i,38]) 
  }
  
  
  output<- matrix(data=0, nrow=151, ncol=2)
  t<-1:150
  output[2:151,2]<-t
  for(i in 1:nrow(output)){
    # units will be ug per g of pollen
    # get uptake from soil
    pol<-soil_pollen_data_off_field[i,]
    uptakesoil <-( (pol$`Kp-L`* pol$Soil_concentration_ug_g_from_soil) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
    
    output[i,1]<- uptakesoil 
    
  }
  output<-as.data.frame(output)
  output[,3]<-x$Compound # add compound
  output[,4]<-x$Commodity
  output<-output[,c(2,3,4,1)]
  names(output)<-c("day","Compound","Commodity",'Pollen_concentration_ug_g_from_soil')
  output
  
  
 }
 
 daily_conc_off_field<-lapply(soil_pollen_datasets, residues_in_pollen_from_soil)
 
 #first put OG names 
 names(daily_conc_off_field)<-names(soil_pollen_datasets)
 
 #then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
 names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]
 
 #where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
 keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
 off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)
 
 #function to set the names in the merged dataframe
 set_names_in_list<-function(x){
   x<-do.call(data.frame, c(x, check.names = FALSE))
   x<-setNames(x, (names(x)))
   x<-(x[, !duplicated(colnames(x)), drop=F])
   
 }
 
 #apply over list, which will automatically repopulate back to the matrix we set up. 
 off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)

 
 #x<-soil_nectar_datasets[[1]]
 ## Nectar
 residues_in_nectar_from_soil<-function(x){
   
   #we can drop all rows except 1 because we've averaged the deposition
   soil_nectar_data_off_field<-x[rep(seq_len(nrow(x)), 151),]
   soil_nectar_data_off_field$day<-0:150
   
   #now, extract the daily PWC estimate using matching PWC run names
   pwc<-as.data.frame(pwc_set[amatch(names(soil_nectar_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
   
   #subset PWC for now (other efforts will need to include 365 days of exposure)
   pwc<-if(grepl('SOY',colnames(pwc)[1])){
     pwc[126:276,]
   } else {
     pwc[114:264,]} # earlier for corn
   
   pwc<-pwc[,c(6,7,10)]
   names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
   pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
   
   #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
   soil_weight<-18000*1.95
   pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
   pwc$day<-0:150
   
   soil_nectar_data_off_field$Soil_concentration_ug_g_from_soil<-pwc$soil_concentration_ug_g
   
   #loop to add new daily flux outputs to existing soil concentration; but remove decay component here
   for(i in 2:nrow(soil_nectar_data_off_field)){
     soil_nectar_data_off_field[i,33]<-soil_nectar_data_off_field[i,33] + soil_nectar_data_off_field[i-1,33] #*exp(0-seed_soil_data_off_field[i,11] * seed_soil_data_off_field[i,38]) 
   }
   
   
   output<- matrix(data=0, nrow=151, ncol=2)
   t<-1:150
   output[2:151,2]<-t
   for(i in 1:nrow(output)){
     # units will be ug per g of pollen
     # get uptake from soil
     pol<-soil_nectar_data_off_field[i,]
     uptakesoil <-( (pol$`Kn-L`* pol$Soil_concentration_ug_g_from_soil) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
     
     output[i,1]<- uptakesoil 
     
   }
   output<-as.data.frame(output)
   output[,3]<-x$Compound # add compound
   output[,4]<-x$Commodity
   output<-output[,c(2,3,4,1)]
   names(output)<-c("day","Compound","Commodity",'Nectar_concentration_ug_g_from_soil')
   output
   
   
 }
 
 daily_conc_off_field<-lapply(soil_nectar_datasets, residues_in_nectar_from_soil)
 
 #first put OG names 
 names(daily_conc_off_field)<-names(soil_nectar_datasets)
 
 #then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
 names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]
 
 #where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
 keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
 off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)
 
 #function to set the names in the merged dataframe
 set_names_in_list<-function(x){
   x<-do.call(data.frame, c(x, check.names = FALSE))
   x<-setNames(x, (names(x)))
   x<-(x[, !duplicated(colnames(x)), drop=F])
   
 }
 
 #apply over list, which will automatically repopulate back to the matrix we set up. 
 off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)
 
 
#### Foliar Applications----
## Air ----
 #these are the only residues we'll measure as ug/m2
 ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))
 #convert to meters
 ag_drift$distance<-ag_drift$distance*0.3048
 agdrift_data<-ag_drift[,c(1,5)] #use ground spray, very fine to fine, ground boom low
 
 
 #get the seed treatment data and estimate concentration in dust for each seed type
 foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
 
 #for drift, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
 foliar_treatments<-foliar_treatments[!foliar_treatments$type == "Nectar",]
 foliar_treatments<-foliar_treatments[ , !(names(foliar_treatments) %in% "type")]
 
 foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
 foliar_air_conc<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
 foliar_air_conc<-foliar_air_conc[with(foliar_air_conc, order(Compound,Commodity)), ] # order by compound, commodity
 # foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_aerial_vf2f #multiple application rate times deposition curve 
 foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_ground_high_vf2f #multiple application rate times deposition curve 
 
 #split each set of commodity and compound data into a unique dataframe
 foliar_air_datasets<-split(foliar_air_conc, list(foliar_air_conc$Compound,foliar_air_conc$ApplicationType, foliar_air_conc$Commodity), drop=T) 

 daily_conc_off_field<-list()
 for(compound_and_crop in 1:length(foliar_air_datasets)){
   foliar_air_data<-foliar_air_datasets[[compound_and_crop]]
   foliar_air_data$Air_concentration_ug_m2_from_foliar<-foliar_air_data$air_drift_conc #retain as ug/m2
   
   foliar_air_data_off_field<-foliar_air_data[!foliar_air_data$d == 0,]
   foliar_air_data_off_field$Air_concentration_ug_m2_from_seed<-mean(foliar_air_data_off_field$Air_concentration_ug_m2_from_foliar)
   
   #because we are only interested in on-field, we extract the conc at 0 distance
   foliar_air_data_off_field<- foliar_air_data_off_field[1,]
   foliar_air_data_off_field<- foliar_air_data_off_field[rep(seq_len(nrow(foliar_air_data_off_field)), 151),]
   foliar_air_data_off_field[2:151, 18]<-0
   foliar_air_data_off_field$day<-0:150
   daily_conc_off_field[[compound_and_crop]]<-foliar_air_data_off_field[,c(1,5,6,18,19)]
   
 }
 
 #first put OG names 
 names(daily_conc_off_field)<-names(foliar_air_datasets)
 
 #then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
 names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]
 
 #where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
 keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
 off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)
 
 #function to set the names in the merged dataframe
 set_names_in_list<-function(x){
   x<-do.call(data.frame, c(x, check.names = FALSE))
   x<-setNames(x, (names(x)))
   x<-(x[, !duplicated(colnames(x)), drop=F])
   
 }
 
 #apply over list, which will automatically repopulate back to the matrix we set up. 
 off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list) 
 
## Soil ----
 #these are the only residues we'll measure as ug/m2
 ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))
 #convert to meters
 ag_drift$distance<-ag_drift$distance*0.3048
 agdrift_data<-ag_drift[,c(1,5)] #use ground spray, very fine to fine
 
 #get the seed treatment data and estimate concentration in dust for each seed type
 foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
 
 #for soil, we can drop duplicate rows that contain nectar/pollen designations bc we don't care
 foliar_treatments<-foliar_treatments[!foliar_treatments$type == "Nectar",]
 foliar_treatments<-foliar_treatments[ , !(names(foliar_treatments) %in% "type")]
 
 foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
 foliar_air_conc<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
 foliar_air_conc<-foliar_air_conc[with(foliar_air_conc, order(Compound,Commodity)), ] # order by compound, commodity
 #foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_aerial_vf2f 
 foliar_air_conc$air_drift_conc<-foliar_air_conc$ug_m2*foliar_air_conc$pond_ground_high_vf2f #multiple application rate times deposition curve 
 
 #split each set of commodity and compound data into a unique dataframe
 foliar_soil_datasets<-split(foliar_air_conc, list(foliar_air_conc$Compound,foliar_air_conc$ApplicationType, foliar_air_conc$Commodity), drop=T) 
 
 #get PWC set for foliar
 pwc_set<-PWC_data_list[grepl('spray', names(PWC_data_list))]
 names(pwc_set)<-toupper(names(pwc_set))
 
 
 daily_conc_off_field<-list()
 for(compound_and_crop in 1:length(foliar_soil_datasets)){
   foliar_soil_data<-foliar_soil_datasets[[compound_and_crop]]
   foliar_soil_data_off_field<-foliar_soil_data[!foliar_soil_data$d == 0,]
   foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<- foliar_soil_data_off_field$air_drift_conc #retain as ug/m2
   foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-mean(foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar)
   
   #because we are only interested in on-field, we extract the conc at 0 distance
   foliar_soil_data_off_field<- foliar_soil_data_off_field[1,]
   foliar_soil_data_off_field<- foliar_soil_data_off_field[rep(seq_len(nrow(foliar_soil_data_off_field)), 151),]
   #foliar_soil_data_off_field[2:151, 17]<-0
   foliar_soil_data_off_field$day<-0:150
   foliar_soil_data_off_field<-foliar_soil_data_off_field[,c(1,5,6,12, 17,18)]
   
   #first, get the concentration from the average deposition
   conc_from_air<-((foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar * (1/10000))/2 * 1/1.95)*exp(-(foliar_soil_data_off_field$k_values*foliar_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
   
   #now, extract the daily PWC estimate using matching PWC run names
   pwc<-as.data.frame(pwc_set[amatch(names(foliar_soil_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
   
   #subset PWC for now (other efforts will need to include 365 days of exposure)
   pwc<-if(grepl('SOY',colnames(pwc)[1])){
     pwc[126:276,]
   } else {
     pwc[114:264,]} # earlier for corn
   
   pwc<-pwc[,c(6,7,10)]
   names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
   pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
   
   #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
   soil_weight<-18000*1.95
   pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
   pwc$day<-0:150
   
   foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar<-pwc$soil_concentration_ug_g
   
   #loop to add new daily flux outputs to existing soil concentration
   for(i in 2:nrow(foliar_soil_data_off_field)){
     foliar_soil_data_off_field[i,6]<-foliar_soil_data_off_field[i,6] + foliar_soil_data_off_field[i-1,6]*exp(0-foliar_soil_data_off_field[i,4] * foliar_soil_data_off_field[i,5]) 
   }
   
   #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
   foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar <- conc_from_air + foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar
   daily_conc_off_field[[compound_and_crop]]<-foliar_soil_data_off_field[,c(1,2,3,6,7)] #add compound
   
   
 }
 
 #first put OG names 
 names(daily_conc_off_field)<-names(foliar_soil_datasets)
 
 #then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
 names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]
 
 #where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
 keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
 off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)
 
 #function to set the names in the merged dataframe
 set_names_in_list<-function(x){
   x<-do.call(data.frame, c(x, check.names = FALSE))
   x<-setNames(x, (names(x)))
   x<-(x[, !duplicated(colnames(x)), drop=F])
   
 }
 
 #apply over list, which will automatically repopulate back to the matrix we set up. 
 off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)
 
 
 
 
## Pollen and Nectar----
 foliar_treatments<-apprates[apprates$ApplicationType == "FoliarI" | apprates$ApplicationType == "FoliarH",]
 foliar_treatments$ug_m2<-foliar_treatments$AvgRate*112085 # convert to ug/m2
 foliar_treatments<-merge(foliar_treatments,agdrift_data) #merge with the fraction estimated via Krupke
 foliar_treatments<- foliar_treatments[with( foliar_treatments, order(Compound,Commodity)), ] # order by compound, commodity
 #foliar_treatments$air_drift_conc<- foliar_treatments$ug_m2* foliar_treatments$pond_aerial_vf2f 
 foliar_treatments$air_drift_conc<- foliar_treatments$ug_m2* foliar_treatments$pond_ground_high_vf2f #multiple application rate times deposition curve 
 
 foliar_treatments<-foliar_treatments[,!names(foliar_treatments) %in% "Type"]
 foliar_nectarpollen_conc<-merge(foliar_treatments,Li) #merge with the fraction estimated via Krupke
 
 ##because we have both nectar and pollen, we need to split up the sets into two batches
 foliar_treatments_pollen<-foliar_nectarpollen_conc[foliar_nectarpollen_conc$type == "Pollen",]
 foliar_treatments_nectar<-foliar_nectarpollen_conc[foliar_nectarpollen_conc$type == "Nectar",]
 
 
 #get PWC set for foliar
 pwc_set<-PWC_data_list[grepl('spray', names(PWC_data_list))]
 names(pwc_set)<-toupper(names(pwc_set))
 
 
 ## Pollen
 #split by compound, crop, and type (nectar or pollen)
 foliar_pollen_datasets<-split(foliar_treatments_pollen, list(foliar_treatments_pollen$Compound, foliar_treatments_pollen$ApplicationType,foliar_treatments_pollen$Commodity), drop=T)
 
#x<-foliar_pollen_datasets[[1]]
 residues_in_nectar_and_pollen_from_foliar<-function(x){
   
   foliar_soil_data_off_field<-x[!x$d == 0,]
   foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-foliar_soil_data_off_field$air_drift_conc #retain as ug/m2
   foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-mean(foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar)

   
   #because we are only interested in on-field, we extract the conc at 0 distance
   foliar_soil_data_off_field<- foliar_soil_data_off_field[1,]
   foliar_soil_data_off_field<- foliar_soil_data_off_field[rep(seq_len(nrow(foliar_soil_data_off_field)), 151),]
   foliar_soil_data_off_field$day<-0:150
   #foliar_soil_data_off_field<-foliar_soil_data_off_field[,c(1,5,6,12,35,36)]
   
   #first, get the concentration from the average deposition
   conc_from_air<-((foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar * (1/10000))/2 * 1/1.95)#*exp(-(foliar_soil_data_off_field$k_values*foliar_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
   
   #now, extract the daily PWC estimate using matching PWC run names
   pwc<-as.data.frame(pwc_set[amatch(names(foliar_pollen_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
   
   #subset PWC for now (other efforts will need to include 365 days of exposure)
   pwc<-if(grepl('SOY',colnames(pwc)[1])){
     pwc[126:276,]
   } else {
     pwc[114:264,]} # earlier for corn
   
   pwc<-pwc[,c(6,7,10)]
   names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
   pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
   
   #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
   soil_weight<-18000*1.95
   pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
   pwc$day<-0:150
   
   foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar<-pwc$soil_concentration_ug_g
   
   #loop to add new daily flux outputs to existing soil concentration
   for(i in 2:nrow(foliar_soil_data_off_field)){
     foliar_soil_data_off_field[i,37]<-foliar_soil_data_off_field[i,37] + foliar_soil_data_off_field[i-1,37]#*exp(0-foliar_soil_data_off_field[i,12] * foliar_soil_data_off_field[i,36]) 
   }
   
   #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
   foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar <- conc_from_air + foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar
   
   output<- matrix(data=0, nrow=151, ncol=2)
   #n<-ifelse(x$Commodity == "CORN", 67,56)
   t<-1:150
   output[2:151,2]<-t
   for(i in 1:nrow(output)){
     # units will be ug per g of pollen
     # first get uptake from soil
     pol<-foliar_soil_data_off_field[i,]
     uptakesoil <-( (pol$`Kp-L`* pol$Soil_concentration_ug_g_from_foliar) * (pol$KupSoil/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSoil)) * (exp(-pol$KdissSoil*i) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) 
     
     #then estimate RUD due to leaf deposition
     uptakeleaf<-( ((pol$`Kp-L`*pol$fsurface*pol$cf1)/(pol$LAI*pol$LMA*(1/(1-pol$Wleaf))*1000)) * (pol$KupSurface/(pol$KelAir+pol$KelDeg+pol$KelGrow - pol$KdissSurface)) * ( exp(-pol$KdissSurface*(i)) - exp(-(pol$KelAir+pol$KelDeg+pol$KelGrow)*i)) ) * (pol$Air_concentration_ug_m2_from_foliar/100000) # sub air drift as AR in kg/ha
            
      output[i,1]<- (uptakesoil + uptakeleaf) 
     
   }
   output<-as.data.frame(output)
   
   names(output)<-c(paste0(pol$type,'_concentration_ug_g_from_foliar'),"day")
   
   #plot(output$Pollen_concentration_ug_g_from_foliar ~ output$day)
   
   output
   
 }
 
 
 #apply function to estimate nectar/pollen residues in ug per g after x number of days
 daily_conc_off_field<-lapply(foliar_pollen_datasets,residues_in_nectar_and_pollen_from_foliar)
 
 #then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
 names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]
 
 #where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
 keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
 off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)
 
 #function to set the names in the merged dataframe
 set_names_in_list<-function(x){
   x<-do.call(data.frame, c(x, check.names = FALSE))
   x<-setNames(x, (names(x)))
   x<-(x[, !duplicated(colnames(x)), drop=F])
   
 }
 
 #apply over list
 off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)
 
 
 ## Nectar
 #split by compound, crop, and type (nectar or pollen)
foliar_nectar_datasets<-split(foliar_treatments_nectar, list(foliar_treatments_nectar$Compound, foliar_treatments_nectar$ApplicationType, foliar_treatments_nectar$Commodity), drop=T)
 
#x<-foliar_nectar_datasets[[1]]
residues_in_nectar_and_pollen_from_foliar<-function(x){
  
  foliar_soil_data_off_field<-x[!x$d == 0,]
  foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-foliar_soil_data_off_field$air_drift_conc #retain as ug/m2
  foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar<-mean(foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar)
  
  
  #because we are only interested in on-field, we extract the conc at 0 distance
  foliar_soil_data_off_field<- foliar_soil_data_off_field[1,]
  foliar_soil_data_off_field<- foliar_soil_data_off_field[rep(seq_len(nrow(foliar_soil_data_off_field)), 151),]
  foliar_soil_data_off_field$day<-0:150
  #foliar_soil_data_off_field<-foliar_soil_data_off_field[,c(1,5,6,12,35,36)]
  
  #first, get the concentration from the average deposition
  conc_from_air<-((foliar_soil_data_off_field$Air_concentration_ug_m2_from_foliar * (1/10000))/2 * 1/1.95)#*exp(-(foliar_soil_data_off_field$k_values*foliar_soil_data_off_field$day)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
  
  #now, extract the daily PWC estimate using matching PWC run names
  pwc<-as.data.frame(pwc_set[amatch(names(foliar_pollen_datasets[compound_and_crop]), names(pwc_set), maxDist = Inf)])
  
  #subset PWC for now (other efforts will need to include 365 days of exposure)
  pwc<-if(grepl('SOY',colnames(pwc)[1])){
    pwc[126:276,]
  } else {
    pwc[114:264,]} # earlier for corn
  
  pwc<-pwc[,c(6,7,10)]
  names(pwc)<-c("RFLX","EFLX","day") #units for rflx and eflx are g/cm2
  pwc$flux<-(pwc$RFLX+pwc$EFLX) * 2 #distributed at 2cm
  
  #to get an average across the 90m, we take 18000 cm3 and a soil bulk density of 1.95 g/cm3
  soil_weight<-18000*1.95
  pwc$soil_concentration_ug_g<-(pwc$flux/soil_weight)*1000000
  pwc$day<-0:150
  
  foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar<-pwc$soil_concentration_ug_g
  
  #loop to add new daily flux outputs to existing soil concentration
  for(i in 2:nrow(foliar_soil_data_off_field)){
    foliar_soil_data_off_field[i,37]<-foliar_soil_data_off_field[i,37] + foliar_soil_data_off_field[i-1,37]#*exp(0-foliar_soil_data_off_field[i,12] * foliar_soil_data_off_field[i,36]) 
  }
  
  #here we combine the deposition from seed dust with the concentration already in the soil from the seed treatment runoff
  foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar <- conc_from_air + foliar_soil_data_off_field$Soil_concentration_ug_g_from_foliar
  
  output<- matrix(data=0, nrow=151, ncol=2)
  #n<-ifelse(x$Commodity == "CORN", 67,56)
  t<-1:150
  output[2:151,2]<-t
  for(i in 1:nrow(output)){
    # units will be ug per g of pollen
    # first get uptake from soil
    nec<-foliar_soil_data_off_field[i,]
    uptakesoil <-( (nec$`Kn-L`* nec$Soil_concentration_ug_g_from_foliar) * (nec$KupSoil/(nec$KelAir+nec$KelDeg+nec$KelGrow - nec$KdissSoil)) * (exp(-nec$KdissSoil*(i)) - exp(-(nec$KelAir+nec$KelDeg+nec$KelGrow)*i)) ) 
    
    #then estimate RUD due to leaf deposition
    uptakeleaf<-( ((nec$`Kn-L`*nec$fsurface*nec$cf1)/(nec$LAI*nec$LMA*(1/(1-nec$Wleaf))*1000)) * (nec$KupSurface/(nec$KelAir+nec$KelDeg+nec$KelGrow - nec$KdissSurface)) * ( exp(-nec$KdissSurface*(i)) - exp(-(nec$KelAir+nec$KelDeg+nec$KelGrow)*i)) ) * (nec$Air_concentration_ug_m2_from_foliar/100000) # sub air drift as AR in kg/ha
    output[i,1]<- (uptakesoil + uptakeleaf) 
    
  }
  output<-as.data.frame(output)
  
  names(output)<-c(paste0(nec$type,'_concentration_ug_g_from_foliar'),"day")
  
  output
  
}


#apply function to estimate nectar/pollen residues in ug per g after x number of days
daily_conc_off_field<-lapply(foliar_nectar_datasets,residues_in_nectar_and_pollen_from_foliar)

#then get names of off_field_residue list that match the seed dust output, rename daily_conc_off_field
names(daily_conc_off_field)<-names(off_field_residue_list)[amatch(names(daily_conc_off_field), names(off_field_residue_list), maxDist = Inf)]

#where the names in the off_field_residue_list match the daily concentrations, join together (note: not the cleanest way, but the easiest I found)
keys <- unique(c(names(daily_conc_off_field), names(off_field_residue_list)))
off_field_residue_list<-setNames(mapply(c, off_field_residue_list[keys], daily_conc_off_field[keys]), keys)

#function to set the names in the merged dataframe
set_names_in_list<-function(x){
  x<-do.call(data.frame, c(x, check.names = FALSE))
  x<-setNames(x, (names(x)))
  x<-(x[, !duplicated(colnames(x)), drop=F])
  
}

#apply over list
off_field_residue_list<-lapply(off_field_residue_list,set_names_in_list)


#testy<-off_field_residue_list[[1]]




######## Plots ###########
#### Daily outputs by application type----

foliar<-off_field_residue_list[1:10]
soil<-off_field_residue_list[11:12]
seed<-off_field_residue_list[13:18]


#simple function to arrange data
gather_data<-function(x){
  df<-x
  df<-gather(df, "Media", "Value", 5:ncol(df))
  df
  
}

# #Create a custom color scale
# library(RColorBrewer)
# myColors <- brewer.pal(8,"Dark2")
# names(myColors) <- unique(apprates$Compound)
# colScale <- scale_colour_manual(name = "Compound",values = myColors)

##Get foliar output
foliar_data<-lapply(foliar,gather_data)
foliar_data<-do.call(rbind,foliar_data)
foliar_data$Value<-ifelse(foliar_data$Value == 0,NA,foliar_data$Value)

foliar <- ggplot(foliar_data, aes(day, Value, color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  scale_y_continuous(expand = c(0.1,0))+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()
  #theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))

foliar


##Get seed output
seed_data<-lapply(seed,gather_data)
seed_data<-do.call(rbind,seed_data)
seed_data$Value<-ifelse(seed_data$Value == 0,NA,seed_data$Value)

seed <- ggplot(seed_data, aes(day, Value,color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  scale_y_continuous(expand = c(0.1,0))+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))

seed

##Get soil output
soil_data<-lapply(soil,gather_data)
soil_data<-do.call(rbind,soil_data)
soil_data$Value<-ifelse(soil_data$Value == 0,NA,soil_data$Value)

soil <- ggplot(soil_data, aes(day, Value, color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  scale_y_continuous(expand = c(0.1,0))+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))

soil

compare_between_application_type<-plot_grid(foliar, seed, soil,  ncol = 1, hjust=1, vjust=-0.5,rel_widths = c(2,2,2))
compare_between_application_type

#labels = c('Foliar', 'Seed','Soil'),

#function to extract legend 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 


all_data<-lapply(off_field_residue_list,gather_data)
all_data<-do.call(rbind,all_data)
all_data$Value<-ifelse(all_data$Value == 0,NA,all_data$Value)

all_plot_legend <- ggplot(all_data, aes(day, Value, color=Compound)) +
  geom_point(aes(shape=Commodity), size=1.6)+
  colScale+
  facet_wrap(~Media,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+
  theme_bw()


legend<-get_only_legend(all_plot_legend)

compare_between_application_type<-plot_grid(compare_between_application_type, legend,ncol = 2, rel_widths = c(6,1))
compare_between_application_type

#### Daily outputs by media type ----

#First designate type
foliar_data$ApplicationType<-"Foliar"
soil_data$ApplicationType<-"Soil"
seed_data$ApplicationType<-"Seed"

#Add in specific media for grouping
foliar_data$MediaSub<- sub("_.*", "", foliar_data$Media)
soil_data$MediaSub<- sub("_.*", "", soil_data$Media)
seed_data$MediaSub<- sub("_.*", "", seed_data$Media)


#combine all 3 types
combine_all<-rbind(foliar_data,soil_data,seed_data)

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
  #geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
  scale_x_continuous(limits=c(0, 150), breaks=seq(0,150, by=20))+
 # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(MediaSub), cols = vars(Commodity), scales="free_y")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  #geom_text(x=125, y=0.05, label="Soil", color="black", size=6)+
  theme_bw()+
  theme()+
  #theme(plot.margin = unit(c(1,1,1,1), "cm"))
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
soiln


pollenn<- ggplot(pollen_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  scale_x_continuous(limits=c(0, 150), breaks=seq(0,150, by=20))+
  #scale_x_continuous(limits=c(60, 130), breaks=seq(60,130, by=10),labels =c("0","7","17","27","37","47","57","67"))+
  # ifelse(pollen_df$Commodity == "CORN",
  # scale_x_continuous(limits=c(70, 130), breaks=seq(70,130, by=10),labels =c("7","17","27","37","47","57","67")),
  # scale_x_continuous(limits=c(60, 120), breaks=seq(60,120, by=10),labels =c("7","17","27","37","47","57","67"))
  # )+
  # geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
  #facet_wrap(Commodity ~ .,scales = "free", nrow=1)+
  facet_grid(rows = vars(MediaSub), cols = vars(Commodity), scales="free_y")+
  ylab("")+
  xlab("Days Post-Application")+
  # geom_text(x=125, y=7.5, label="Pollen", color="black", size=6)+
  theme_bw()+
  theme()+
  #  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
pollenn

# c<-seq(70,130, by=10)
# s<-seq(60,120, by=10)
# 
# pollenn<-pollenn + facetted_pos_scales(
#   x = list(
#     Commodity == "CORN" ~ scale_x_continuous(limits=c(70, 130), breaks=c,labels =c("0","7","17","27","37","47","57")),
#     Commodity == "SOYBEANS" ~ scale_x_continuous(limits=c(60, 120),breaks=s,labels =c("0","7","17","27","37","47","57"))
#   )
# )
# 


nectarn<- ggplot(nectar_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  scale_x_continuous(limits=c(0, 150), breaks=seq(0,150, by=20))+
  #scale_x_continuous(limits=c(60, 120), breaks=seq(60,120, by=10),labels =c("0","7","17","27","37","47","57"))+
  #geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
 # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(MediaSub), cols = vars(Commodity), scales="free_y")+
  ylab("")+
 xlab("Days Post-Application")+
  #geom_text(x=125, y=0.9, label="Nectar", color="black", size=6)+
  theme_bw()+
  # theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
nectarn

# nectarn<-nectarn + facetted_pos_scales(
#   x = list(
#     Commodity == "SOYBEANS" ~ scale_x_continuous(limits=c(60, 120),breaks=s,labels =c("0","7","17","27","37","47","57"))
#   )
# )
# 

compare_between_media<-plot_grid(
  soiln,
  pollenn,
  nectarn, 
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 1,rel_widths = c(3,3,3))
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

compare_between_mediaoff<-plot_grid(compare_between_media, legend,ncol = 2, rel_widths = c(7,1))
compare_between_mediaoff


### get deposition data for table

air_and_dust<-combine_all[combine_all$MediaSub ==  "Air" | combine_all$MediaSub ==  "Dust",]
air_and_dust<-air_and_dust[air_and_dust$day == 0,]



### the 3rd type is by compound
list_by_compound<-split(combine_all, f= combine_all$Compound)

create_plot_by_compound<-function(x){
  df<-x
  
  out<-ggplot(df, aes(day, Value, color=ApplicationType)) +
    geom_point()+
    geom_line(aes(linetype = Commodity), size=0.8)+
    facet_grid(rows = vars(MediaSub), cols = vars(Commodity), 
               scales="free_y", switch = 'y')+
    ylab("Residues")+
    xlab("Days Post-Planting")+
    ggtitle(paste0(x$Compound)) +
    theme_bw()
  out
  
}

list_of_plots<-lapply(list_by_compound,create_plot_by_compound)


### The 4th is similar to Li et al.2022


list_by_compound<-split(combine_all, f= combine_all$Compound)


create_plot_by_compound<-function(x){
  df<-x
  df<-df[!(df$MediaSub=="Air" | df$MediaSub=="Dust"),]
  
  out<-ggplot(df, aes(day, Value, group=interaction(ApplicationType,MediaSub), color=MediaSub)) +
    # geom_point()+
    geom_line( size=0.8)+
    facet_grid(rows=vars(ApplicationType),cols = vars(Commodity), 
               scales="free",switch = 'y')+
    ylab("Residues")+
    xlab("Days Post-Application")+
    ggtitle(paste0(x$Compound)) +
    theme_bw()
  out
  
}

list_of_plots<-lapply(list_by_compound,create_plot_by_compound)


#### Hazard quotients----
options(scipen = 0)
#first designate 
foliar_data$ApplicationType<-"Foliar"
soil_data$ApplicationType<-"Soil"
seed_data$ApplicationType<-"Seed"

#Add in specific media for grouping
foliar_data$MediaSub<- sub("_.*", "", foliar_data$Media)
soil_data$MediaSub<- sub("_.*", "", soil_data$Media)
seed_data$MediaSub<- sub("_.*", "", seed_data$Media)


#combine all 3 types
combine_all<-rbind(foliar_data,soil_data,seed_data)

#ingestion rate for oral
IR<-0.292 #g/day

#surface area of a bumblebee
SA<-2.216 #cm2


# ###max points
# get_max_eec<-combine_all %>% 
#   group_by(Compound,Commodity,ApplicationType,MediaSub) %>%
#   slice(which.max(Value)) %>%
#   arrange(Compound,Commodity,ApplicationType,MediaSub)
# 
# 
#  get_max_eec$units<-ifelse(get_max_eec$MediaSub == "Dust" | get_max_eec$MediaSub == "Air", "ug/m2","ug/g" )
#  eec_df<-merge(get_max_eec,beetox,by="Compound")
#  split_datasets<-split(eec_df, list(eec_df$ApplicationType, eec_df$Commodity), drop=T) 
#  
#  x<-split_datasets[[5]]
#  create_plot_of_rq<-function(x){
#    df<-x
#    
#    depo<-df[df$MediaSub == "Air" |df$MediaSub == "Dust" , ]
#    micro<-df[df$MediaSub == "Pollen" | df$MediaSub == "Nectar"| df$MediaSub == "Soil", ]
#    
#    #first calculate the estimated RQ based on contact with deposition
#    depo$RQ<-((depo$Value/1000)*SA)/depo$Contact_LD50_ug_bee # convert to cm2, multiple by surface area of bumblebee to get eec in ug/bee, and divide by contact LD50
#    
#    #then estimate RQ based on either ingestion (pollen/nectar) or soil contact
#    micro$RQ<-ifelse(micro$MediaSub == "Soil", (micro$Value)/micro$Contact_LD50_ug_bee, (micro$Value * IR)/micro$Oral_LD50_ug_bee)
#   
#    df<-rbind(micro,depo)
#   
#    out<-ggplot(df, aes(Compound, RQ, color=MediaSub)) +
#      geom_hline(yintercept=0.4)+
#      geom_point( size=3.2, aes(shape=MediaSub))+
#      facet_wrap(~MediaSub, scales = "free")+
#      ylab("Risk Quotient")+
#      xlab("Compound")+
#      theme_bw()+
#      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#      ggtitle(paste0(x$ApplicationType,"-", x$Commodity)) 
#    out
#    
#   }
# 
#  list_of_plots<-lapply(split_datasets,create_plot_of_rq)
#  

###boxplots
#combine all 3 types
combine_all<-rbind(foliar_data,soil_data,seed_data)
combine_all$units<-ifelse(combine_all$MediaSub == "Dust" | combine_all$MediaSub == "Air", "ug/m2","ug/g" )
eec_df<-merge(combine_all,beetox,by="Compound")

# unique(combine_all$Compound)
# unique(eec_df$Compound)
# 
#customize colors
library(RColorBrewer)
myColors <- brewer.pal(5,"Dark2")
names(myColors) <- unique(eec_df$MediaSub)
colScale <- scale_colour_manual(name = "MediaSub",values = myColors)

split_datasets<-split(eec_df, list(eec_df$ApplicationType), drop=T) 


create_plot_of_rq<-function(x){
  df<-x
  depo<-df[df$MediaSub == "Air" |df$MediaSub == "Dust" , ]
  micro<-df[df$MediaSub == "Pollen" | df$MediaSub == "Nectar"| df$MediaSub == "Soil", ]
  
  #first calculate the estimated RQ based on contact with deposition
  depo$RQ<-((depo$Value/1000)*SA)/depo$Contact_LD50_ug_bee # convert to cm2, multiple by surface area of bumblebee to get eec in ug/bee, and divide by contact LD50
  
  #then estimate RQ based on either ingestion (pollen/nectar) or soil contact
  micro$RQ<-ifelse(micro$MediaSub == "Soil", (micro$Value)/micro$Contact_LD50_ug_bee, (micro$Value * IR)/micro$Oral_LD50_ug_bee)
  df<-rbind(micro,depo)
  
  #create plot
  out<-ggplot(df, aes(MediaSub, log(RQ), fill=MediaSub)) +
    geom_hline(yintercept=log(0.4))+
    geom_boxplot() +
    colScale+
    scale_y_continuous(limits=c(-30,5), breaks=seq(-30,5, by=10))+
    facet_wrap(Commodity ~ Compound, 
               scales="free_y",nrow=2)+
    #facet_wrap(~Compound, scales = "free", nrow=1)+
    ylab(ifelse(df$ApplicationType == "Foliar","Hazard Quotient", ""))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0(x$ApplicationType)) +
    theme(legend.position="none",axis.title=element_text(size=14,face="bold"),axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"), axis.title.x=element_blank())
  out
  
}


list_of_plots<-lapply(split_datasets,create_plot_of_rq)

compare_RQ<-plot_grid(
  list_of_plots[[1]],
  list_of_plots[[2]],
  list_of_plots[[3]],
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 1,rel_widths = c(3,2,1))
compare_RQ


legend_plot<-ggplot(eec_df, aes(MediaSub, Value, fill=MediaSub)) +
  geom_hline(yintercept=log(0.4))+
  geom_boxplot() +
  #geom_point( size=3.2, aes(shape=MediaSub))+
  # scale_y_continuous(limits=c(-30,5), breaks=seq(-30,5, by=10))+
  
  facet_wrap(Commodity ~ Compound, 
             scales="free_y",nrow=2)+
  #facet_wrap(~Compound, scales = "free", nrow=1)+
  ylab("Risk Quotient")+
  xlab("Compound")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=14))+
  #ggtitle(paste0(x$ApplicationType)) +
  guides(fill=guide_legend(title="Media Type"))
legend_plot


legend<-get_only_legend(legend_plot)

compare_RQ<-plot_grid(compare_RQ, legend,nrow=1, rel_widths = c(6.5,0.5))
compare_RQ




#### Compare against reported field residues----
options(scipen=999)

## read in residues from DER
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
residues$Source<-"Field"


# read in residues from lit review
litres<-read.csv(paste0(pest_dir,"/Residues/Lit_ResidueData.csv"))
litres<-litres[,c(1:5)]
litres<-litres[,c(1,3,2,4,5)]

litres<-na.omit(litres)
litres$Source<-"Field"

colnames(litres)<-colnames(residues)
litres$Compound<-toupper(litres$Compound)

# get the estimated data
combine_all<-rbind(foliar_data,soil_data,seed_data)
combine_all<-combine_all[,c(2,3,6,7,5)]
colnames(combine_all)[5]<-"Residues_ugg"
combine_all<-na.omit(combine_all)
combine_all$Source<-"Estimated"
# names(combine_all)
# names(residues)

combine_all<-combine_all[,c(1:2,4,5,3,6)]

#combine all together
residues_all<- rbind(combine_all,residues, litres)
residues_all<-residues_all[!residues_all$Commodity =="COTTON",]


#install.packages("ggh4x")

list_by_applciationtype<-split(residues_all, f= residues_all$MediaSub)
# x<-list_by_applciationtype[[3]]

create_plot_of_fieldcompare<-function(x){
  df<-x
  
  df<-df %>% 
    group_by(MediaSub, ApplicationType,Compound)%>%
    filter(Residues_ugg > 0.001) %>%
    filter(n_distinct(Source) > 1) 
  
  
  out<-ggplot(df, aes(Source, log(Residues_ugg),fill=Source)) +
    geom_boxplot() +
    geom_hline(yintercept=log(0.001))+
    colScale+
    # scale_y_continuous(limits=c(-7,2), breaks=seq(-7,2, by=2))+
    ggh4x:: facet_nested(.~ApplicationType + Compound, 
                         scales="free_y")+
    #facet_wrap(~Compound, scales = "free", nrow=1)+
    ylab(ifelse(df$MediaSub == "Pollen","Log Residues [ug/g]", ""))+
    xlab(ifelse(df$MediaSub == "Nectar", "Estimated vs. Field Residues", ""))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0(x$MediaSub)) +
    theme(legend.position="none", axis.text.y = element_text(size=14,face="bold"), axis.text.x = element_text(size=14,face="bold"),  axis.title=element_text(size=14,face="bold"))
  out
}


list_of_estimate_plots<-lapply(list_by_applciationtype,create_plot_of_fieldcompare)

compare_field_est<-plot_grid(
  # list_of_estimate_plots[[1]],
  # list_of_estimate_plots[[2]],
  list_of_estimate_plots[[5]],
  list_of_estimate_plots[[4]],
  list_of_estimate_plots[[3]],
  
  
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 3,rel_widths = c(1.5,2,1.5))

compare_field_est


#### Compare directly between application types for same compound ----
combine_all<-rbind(foliar_data,soil_data,seed_data)
forcomparisons<-combine_all[combine_all$MediaSub == "Pollen" | combine_all$MediaSub == "Nectar"| combine_all$MediaSub == "Soil", ]
forcomparisons<-na.omit(forcomparisons)

paired_data<-forcomparisons %>% 
  group_by(Compound,Commodity)%>%
  filter(n_distinct(ApplicationType) > 1) 

# paired_data<-paired_data[paired_data$day==1 | paired_data$day == 60 | paired_data$day == 70,]
# ind1<-with(paired_data, (MediaSub == "Soil") &
#   ((day==60) | (day==70)))
# 
# ind2<-with(paired_data, (Commodity == "SOYBEANS") &
#              (day==70))
# 
# paired_dataf<-paired_data[!c(ind1 | ind2),]
options("scipen"=2, "digits"=3)

compare_outputs<-split(paired_data, list(paired_data$Compound, paired_data$Commodity), drop=T) 
#x<-compare_outputs[[1]]



library(RColorBrewer)
myColors <- brewer.pal(3,"Dark2")
names(myColors) <- unique(combine_all$ApplicationType)
colScale <- scale_colour_manual(name = "ApplicationType",values = myColors)
fillScale <- scale_fill_manual(name = "ApplicationType",values = myColors)


create_plot_of_methodcompare<-function(x){
  df<-x
  
  ticks<-  ifelse(df$Compound=="BIFENTHRIN",
                  c(-45,-35,-20,-10,-5),
                  c(-20,-15,-10,-5,0.5))
  
  logticks<-  ifelse(x$Compound=="BIFENTHRIN",
                     c(exp(-45),exp(-35),exp(-20),exp(-10),exp(-5)),
                     c(exp(-20),exp(-15),exp(-10),exp(-5), exp(0.5)))
  
  
  ticks<- ticks[1:5]
  logticks<-logticks[1:5]
  
  df$Value<-log(df$Value)
  
  out<-ggplot(df, aes(ApplicationType, (Value),color=ApplicationType, fill=ApplicationType)) +
    geom_boxplot()+
    colScale+
    fillScale+
    scale_y_continuous(breaks=ticks, labels=format(logticks,3))+
    facet_wrap( ~MediaSub)+
    ylab("Residues [ug/g]")+
    xlab(ifelse(df$Compound=="IMIDACLOPRID","Application Type", ""))+
    theme_bw()+
    theme(legend.position="none", axis.text.y = element_text(size=14,face="bold"), axis.text.x = element_text(size=14,face="bold"),  axis.title=element_text(size=14,face="bold"))+
    ggtitle(paste0(x$Compound, "-", x$Commodity)) 
  out
  
}




out_plots<-lapply(compare_outputs, create_plot_of_methodcompare)

compare_app_methods<-plot_grid(
  out_plots[[1]],
  out_plots[[2]],
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 2,rel_widths = c(3,3))
compare_app_methods


#### Compare with onsite

compare_between_media_both<-plot_grid(compare_between_media, compare_between_mediaoff,nrow=2, rel_widths = c(3,3))


