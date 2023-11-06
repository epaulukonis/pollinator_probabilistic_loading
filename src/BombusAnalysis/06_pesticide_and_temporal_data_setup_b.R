### Probabilistic Crop Loading 

### 06 temporal crop plantings

# Edited by E. Paulukonis March 2023


scenario<-paste0(root_data_out, "/all_bombus/modified_sampled_fields/sampled_fields_2021.shp")


if(file.exists(scenario)){
  
  print(list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  scenarios<- file.path(paste0(root_data_out, "/all_bombus/modified_sampled_fields"), list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
  scenarios<-setNames(lapply(scenarios, st_read), tools::file_path_sans_ext(basename(scenarios)))
  scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]
  
  #change column names to fix 'emerged date' as application date
  colnamesog <- colnames(scenarios[[1]])
  colnamesnew<-c("Compond","Commdty","Year","ApplctT", "crop","area",  "id", "plntddt","applddt", "AvgRt_k","geometry")
  
  scenarios<- lapply(scenarios, setNames, colnamesnew)
  
}else{



options(scipen = 999) #remove exponent options, throws R off
#par(mfrow=c(1,1))

#note; much of this data works with years from 1999-2021, but for the purposes of trying to match reported data, we end up with data from 2004-2019

##### Crop and pesticide reported data preparation ----
## 1. get pesticide data
#state data
state_pest<-read.csv(paste0(pest_dir, "/PNSP/state/PNSP_IL.csv"))

#county data
county_pest<-list()
county_pest[1:17]<-file.path(paste0(pest_dir,"/PNSP/County"), list.files(paste0(pest_dir,"/PNSP/County"), pattern='.txt', all.files=TRUE, full.names=FALSE))
county_pest_list<-lapply(county_pest, function(x) read.table(x, header=TRUE, sep ='\t'))

#here, we add in the county codes as well as the specific compounds we'd like to focus on  
codes<-c(7, 111, 201)
Compound<-c(
"ACEPHATE",
"CLOTHIANIDIN",
"CHLORPYRIFOS",
"CARBOFURAN",
"DIMETHOATE",
"MALATHION",
"CHLORETHOXYFOS",
"PHORATE",
"IMIDACLOPRID",
"THIAMETHOXAM",
"BIFENTHRIN",
"PERMETHRIN",
"CARBARYL",
"GLYPHOSATE",
"ATRAZINE",
"ACETOCHLOR"

)

#get state, county, and compounds
get_data<-function(x){
  x<-x[x$STATE_FIPS_CODE == 17,]
  x<-x[x$COUNTY_FIPS_CODE %in% codes,]
  x<-x[x$COMPOUND %in% Compound,] 
}
county_pest<-lapply(county_pest_list, get_data)
county_pest<-do.call(rbind,county_pest)
county_pest<-county_pest %>% group_by(COMPOUND, YEAR) %>% summarise(sumx=sum(EPEST_HIGH_KG))

## some code to take a look
#list<-county_pest[with(county_pest,order(-sumx)),]
# list<-list[list$COMPOUND == "BIFENTHRIN",]
# list<-list[!list$YEAR != 2014 & list$COUNTY_FIPS_CODE==201,]



## 2. get county crop data for comparison
ill_coa<-read.csv(paste0(coa_dir,"/ILL_bombus.csv"))
ill_coa<-ill_coa[order(ill_coa$Year),]
ill_nass<- ill_coa%>%
 group_by(County,Year)%>%
  filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .))) %>%
  filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING|WHEAT, WINTER', .))) #note; I included an extra 'winter wheat' elimination because CoA double counts winter wheat as wheat
ill_nass<-ill_nass[!ill_nass$Value == " (D)",]
ill_nass$Value<-as.numeric(as.numeric(gsub(",", "", ill_nass$Value)))
crop_by_county<- ill_nass %>% group_by(County, Year, Commodity) %>% summarise(sumf = sum(as.numeric(Value)))
crop_by_county<-crop_by_county %>% group_by(Year,Commodity) %>% summarise(acres=sum(sumf)) #represents the total acreage of crops for three counties


## 3. get state crop data for calculating percentage planted
ill_coa<-read.csv(paste0(coa_dir,"/ILL_bombus_statewide.csv"))
ill_coa<-ill_coa[order(ill_coa$Year),]
ill_nass<-filter(ill_coa,Domain == "TOTAL")
ill_nass$Value<-as.numeric(as.numeric(gsub(",", "", ill_nass$Value)))
crop_by_state<- ill_nass %>% group_by(Year, Commodity) %>% summarise(acres = sum(as.numeric(Value))) #total acres harvested
crop_by_state<-crop_by_state[crop_by_state$Year %in% c(1997,2002,2007,2012,2017),] #get CoA years, represents the total acreage of crops for the state

##now read in the application rates from the literature 
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)

## 4. get average applications in loads for the state and county, as was done in Douglas et al. 2022, but we'll convert to per pixel rate
##State
state_pest<-gather(state_pest, "Commodity", "kg", 6:15)
state_pest$Commodity<-toupper(state_pest$Commodity)
state_pest<-state_pest[state_pest$Commodity %in% crop_by_state$Commodity, ]
pest_prop_by_crops<-left_join(state_pest, crop_by_state, by = c("Year","Commodity"))

##County
#merge with apprates
county_pest<-merge(county_pest, apprates, by.x="COMPOUND", by.y="Compound", all=T)
county_pest<-county_pest[!is.na(county_pest$AvgRate),]
names(county_pest)[1:2]<-c("Compound","Year")
pest_prop_by_cropc<-left_join(county_pest, crop_by_county, by = c("Year","Commodity"))

#fill interval generator; this takes the closest available data year and backfills
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

#before we do anything else, we need to account for the lag in reporting for seed treatments post 2014
# county level
pest_prop_by_cropc<-pest_prop_by_cropc %>% 
  group_by(Compound, Commodity) %>%
  mutate(sumx = ifelse(ApplicationType == "Seed" & Year > 2014, sumx[Year==2014] - sumx, sumx ))

#code to test that the replacement is working  
#test<-pest_prop_by_cropc[pest_prop_by_cropc$ApplicationType == "Seed",]
#test<-test[with(test, order(Compound, Commodity, Year)), ] 

# state level
neonics<-c("CLOTHIANIDIN","IMIDACLOPRID","THIAMETHOXAM")
pest_prop_by_crops<-pest_prop_by_crops %>% 
  group_by(Compound, Commodity) %>%
  mutate(kg = ifelse(Compound %in% neonics & Year > 2014, kg[Year==2014], kg ))

#code to test that the replacement is working  
# test<-pest_prop_by_crops[pest_prop_by_crops$Compound == "CLOTHIANIDIN",]
# test<-test[with(test, order(Compound, Commodity, Year)), ] 

##here, we get the proportions of treated acres that we will use to define the sampling
#first, prepare the county datatset
pest_prop_by_cropc<-pest_prop_by_cropc[with(pest_prop_by_cropc, order(Compound,Year)), ] #order
pest_prop_by_cropc<-  pest_prop_by_cropc %>% group_by(Compound,Commodity) %>% mutate(acres_f =f1(acres)) #impute total acres of crops
pest_prop_by_cropc$AvgRate_kgacre<-pest_prop_by_cropc$AvgRate*0.453592 #convert application rate to kg/acre

#then, get the proportion of each compound applied to corn/soy from the state dataset
pest_prop_by_crops<- pest_prop_by_crops[with(pest_prop_by_crops, order(Compound,Year)), ] #order
pest_prop_by_crops<- pest_prop_by_crops %>% group_by(Compound,Commodity) %>% mutate(acres_f =f1(acres)) #impute total acres of crops
pest_prop_by_crops<- pest_prop_by_crops %>% group_by(Compound,Commodity) %>% mutate(kg_f =f1(kg)) #impute kg of compound applied
pest_prop_by_crops<- pest_prop_by_crops %>% group_by(Compound,Year) %>% mutate(sumx=sum(kg_f)) # sum total kg applied to all crops
pest_prop_by_crops$prop_crop<- pest_prop_by_crops$kg_f/pest_prop_by_crops$sumx # proportion of total loading applied to each crop

#now that we have estimates of the proportion of crops by compound, join back to the county
propjoin<-pest_prop_by_crops[,c(3,4,6,12)]
pest_prop_by_cropc<- merge(pest_prop_by_cropc, propjoin, by=c("Compound", "Year", "Commodity"), all.x=T, all.y=F)

#adjust to 97% of crop coverage of soy/corn, based on CropScape visual estimate
pest_prop_by_cropc$sumx<-pest_prop_by_cropc$sumx*0.97

#multiply that adjusted amount by the proportions calculated from the state data
pest_prop_by_cropc$sumx_by_crop<-pest_prop_by_cropc$sumx*pest_prop_by_cropc$prop_crop

#proportion of sumx (kg of x compound by tri-county area)
pest_prop_by_cropc$acres_total<-pest_prop_by_cropc$sumx_by_crop/pest_prop_by_cropc$AvgRate_kgacre # divide the summed kg by kg/acre to get acres of treated fields based on the avg application rate
pest_prop_by_cropc$prop_treated_acres<-pest_prop_by_cropc$acres_total/pest_prop_by_cropc$acres_f # this theoretically represents the proportion of all treated acres for this

#however, because we have multiple application types and crops for singular compounds, we need to address that here:

#because imidacloprid and bifenthrin are available as both seed/soil AND foliar, we use a representative sampling scheme here
pest_prop_by_cropc$prop_treated_acresf<- ifelse(pest_prop_by_cropc$Compound == "IMIDACLOPRID" & pest_prop_by_cropc$ApplicationType == "FoliarI", pest_prop_by_cropc$prop_treated_acres *0.03, pest_prop_by_cropc$prop_treated_acres)
pest_prop_by_cropc$prop_treated_acresf<- ifelse(pest_prop_by_cropc$Compound == "IMIDACLOPRID" & pest_prop_by_cropc$ApplicationType == "Seed", pest_prop_by_cropc$prop_treated_acres *0.95, pest_prop_by_cropc$prop_treated_acres)

pest_prop_by_cropc$prop_treated_acresf<- ifelse(pest_prop_by_cropc$Compound == "BIFENTHRIN" & pest_prop_by_cropc$ApplicationType == "FoliarI", pest_prop_by_cropc$prop_treated_acres *0.50, pest_prop_by_cropc$prop_treated_acres)
pest_prop_by_cropc$prop_treated_acresf<- ifelse(pest_prop_by_cropc$Compound == "BIFENTHRIN" & pest_prop_by_cropc$ApplicationType == "Soil", pest_prop_by_cropc$prop_treated_acres *0.50, pest_prop_by_cropc$prop_treated_acres)


##we also need to add an additional layer of sampling based on which compound will be selected as a specific application type, as we assume only one of each kind EXCEPT glyphosate
pest_prop_by_cropc<-pest_prop_by_cropc %>% 
  group_by(Year, Commodity, ApplicationType) %>% 
  mutate(sum_by_app =sum(sumx_by_crop)) %>% 
  mutate(probs_application=sumx_by_crop/sum_by_app)

#finally, there will be some 'proportion treated' where the proportion is greater than 1, because the application rate may overestimate acreage 
# we also need to account for some rows adding up to more than 0
pest_prop_by_cropc<- pest_prop_by_cropc %>% 
  group_by(Year, Commodity, ApplicationType) %>% 
  mutate(prop_treated_acresf = ifelse(ApplicationType == "Seed" & prop_treated_acres > 1, 1, prop_treated_acresf)) %>%
  mutate(prop_treated_acresf = ifelse(ApplicationType == "Seed" & prop_treated_acres < 0, 0, prop_treated_acresf)) %>%
  mutate(prop_treated_acresf = ifelse(cumsum(prop_treated_acresf)  > 1 & prop_treated_acresf < 1, 0, prop_treated_acresf )) %>% # this code specifically makes sure that the values of groups do not add up to more than 0
  ungroup()

  
# Various code to test the output if needed
# testy<-pest_prop_by_cropc %>%
# group_by(Year, Commodity, ApplicationType) %>%
#   filter(n()>1)%>%
#   mutate(pick= sample(Compound, size = 1, prob = probs_application))


# code to test output; sum of probs_application should = 1
#test<-pest_prop_by_cropc[pest_prop_by_cropc$Year == 2015 & pest_prop_by_cropc$ApplicationType == "Seed" & pest_prop_by_cropc$Commodity == "CORN",]
#test<-pest_prop_by_cropc[pest_prop_by_cropc$Year == 2018 & pest_prop_by_cropc$ApplicationType == "FoliarH" & pest_prop_by_cropc$Commodity == "CORN",]
#test<-pest_prop_by_cropc[pest_prop_by_cropc$ApplicationType == "Foliar" & pest_prop_by_cropc$Commodity == "SOYBEANS",]


##### Planting data preparation ----

## 1.In this section, I read in the planting dates and structure the corn and soy planting and emerging dates
#read in the crop planting probabilities
print(list.files(path=paste0(root_data_in,"/bombus/exposuredata/plantingdates/NASS"), pattern='.csv', all.files=TRUE, full.names=FALSE))
pd<- file.path(paste0(root_data_in,"/bombus/exposuredata/plantingdates/NASS"), list.files(paste0(root_data_in,"/bombus/exposuredata/plantingdates/NASS"), pattern='.csv', all.files=TRUE, full.names=FALSE))
pd<-setNames(lapply(pd, read.csv), tools::file_path_sans_ext(basename(pd)))
pd<-lapply(pd, function(y) { y["X"] <- NULL; y })

#crop dates for corn and soy
corn<-pd[[1]]
soy<-pd[[2]]

#remove columns that are 5 year averages, previous years, or CV
corn<- corn %>% select(-contains(c("5.YEAR","PREVIOUS","CV", "HARVESTED")))
soy<- soy %>% select(-contains(c("5.YEAR","PREVIOUS","CV","HARVESTED")))
corn<-corn[,-c(8:18)] #drop other columns we don't need
soy<-soy[,-c(8:18)] #drop other columns we don't need

#shorten names
status_corn<-c("DENTED","DOUGH","EMERGED","MATURE","PLANTED","SILKING")
names(corn)[8:13]<-status_corn
status_soy<-c("BLOOMING","COLORING","DROPPINGLEAVES","EMERGED","PLANTED","PODS")
names(soy)[8:13]<-status_soy

#remove all not relevent
corn[is.na(corn)] <- 0
soy[is.na(soy)]<-0
corn<-gather(corn, "Status", "Percent", 8:13)
soy<-gather(soy, "Status", "Percent", 8:13)
corn<-corn[!grepl(c("DENTED|MATURE|DOUGH|SILKING|EMERGED"), corn$Status),]
soy<-soy[!grepl(c("DROPPINGLEAVES|PODS|COLORING|BLOOMING|EMERGED"), soy$Status),]

#convert to percentage and lag 
corn<-corn%>% mutate(prob= (Percent- lag(Percent, default = first(Percent)))/100) #in order to have each week translate to x % corn planted at week ending x...
corn$prob<-ifelse(corn$prob<0, 1+corn$prob, corn$prob)

soy<-soy%>% mutate(prob= (Percent- lag(Percent, default = first(Percent)))/100) #in order to have each week translate to x % corn planted at week ending x...
soy$prob<-ifelse(soy$prob<0, 1+soy$prob, soy$prob)

#assign CDL code
corn$Crop = 1
soy$Crop = 5

#remove unwanted characters
soy$Period  <- gsub('#','',soy$Period)
corn$Period  <- gsub('#','',corn$Period)

#if prob= 0, remove
corn<-corn[corn$prob != 0, ]   
soy<-soy[soy$prob != 0, ] 

#add sample ID   
corn<- corn %>%
  group_by(Year,Status) %>%
  mutate(SampleP = 1:length(Period))
 
soy<- soy %>%
   group_by(Year,Status) %>%
   mutate(SampleP = 1:length(Period))


#### Pesticide sample data preparation ----

#now that we have the planting data, we need to prepare the pesticide data. 
pest_crop<-pest_prop_by_cropc
names(pest_crop) #just make sure that the columns below include acres_f, avgrate_kgacre, 

#first, let's get the temporal timing right
pest_crop<-pest_crop[!pest_crop$Year < 1999,] #remove years below 1999
pest_crop<-pest_crop[,-c(4:7,10:15,17:20,22)] #remove extraneous columnsn #NOTE: IF YOU RE-RUN, YOU NEED TO FIX THESE COLUMNS YOU EXCLUDE
pest_crop<-pest_crop %>% group_by(Compound) %>% arrange(Year)

#impute back where years missing for relevant crops
pest_crop<-pest_crop %>% 
  group_by(Compound,Commodity, ApplicationType) %>% 
  complete(Year = full_seq(1999:2021, 1)) %>% 
  fill(names(.))

pest_crop<-pest_crop[!is.na(pest_crop$acres_f),] #remove rows for which the application type doesn't actually exist
pest_crop$prop_treated_acresf<-as.numeric(pest_crop$prop_treated_acresf) #convert probs to numeric

#my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA) #create function to ignore NAs

#for any seed treatments prior to 2006, remove; remove soybean application for chlorpyrifos, and remove carbofuran post 2009
pest_cropf<-pest_crop %>%
  group_by(Compound,Commodity) %>%
  mutate(probsf = ifelse(ApplicationType == "Seed" & Year < 2006, 0, prop_treated_acresf)) %>%
  mutate(probsf = ifelse(Commodity == "SOYBEANS" & Compound == "CHLORPYRIFOS" & Year < 2006, 0, probsf)) %>%
  mutate(probsf = ifelse(Compound == "CARBOFURAN" & Year > 2009, 0, probsf)) %>%
  mutate(probs_treated= probsf)



#drop probabilities we don't need
pest_cropf <- subset(pest_cropf, select = -c(prop_treated_acresf,probsf))


#### Sampling treatment types and Dates ----
## 1. This is where I actually run the for-loop to assign treated and untreated fields, and dates  
names(fv)<-1999:2021
list_of_sampled_fields_by_year<-list()


### in this section, we sample what applications occur to each field and of those, which specific compounds will be used. we also join the application rate data. 
### there are two layers of sampling. First, we sample the individual application types based on compound proportion. then, we sample the actual proportion of treated fields by compound. 

for(Year in 1:length(fv)){
field<-fv[[Year]]
field<-st_as_sf(field) #convert to SF
field$id<-as.numeric(row.names(field))+1
field$year<-names(fv[Year])
field_cornsoy<-field[field$crop %in% c(1,5),]
field_other<-field[!field$crop %in% c(1,5),]


#we have several different compounds, so we need to group by compound and do for each field set and do the individual probability sets
treatment_probabilities_soy<-pest_cropf[pest_cropf$Year == names(fv[Year]) & pest_cropf$Commodity == "SOYBEANS",]
treatment_probabilities_corn<-pest_cropf[pest_cropf$Year == names(fv[Year]) & pest_cropf$Commodity == "CORN",]


# #remove carbofuran post 2009 (done above)
# ind <- with(treatment_probabilities_corn,(Compound == "CARBOFURAN" & Year > 2009))
# treatment_probabilities_corn <- treatment_probabilities_corn[!ind, ]       


#here, we add the no application option, and take the probabilities by compound to sample
add_summary_rows <- function(.data, ...) {
  group_modify(.data, function(x, y) bind_rows(x, summarise(x, ...)))
}

treatment_probabilities_corn <-treatment_probabilities_corn %>% 
  group_by(Compound) %>% 
  add_summary_rows(
  ApplicationType = "None",
  probs_treated = 1-sum(probs_treated)) %>%
  fill(Commodity,Year,ApplicationTiming..d.,acres_f,AvgRate_kgacre, .direction = "downup")


treatment_probabilities_soy<-treatment_probabilities_soy %>% 
  group_by(Compound) %>% 
  add_summary_rows(
    ApplicationType = "None",
    probs_treated = 1-sum(probs_treated)) %>%
  fill(Commodity,Year,ApplicationTiming..d.,acres_f,AvgRate_kgacre, .direction = "downup")


#assign unique treatment type IDs
treat<-c("Seed","FoliarI","FoliarH","Soil","None")
id<-1:5
treatmentid<-as.data.frame(cbind(treat,id))
names(treatmentid)<-c("ApplicationType","ID")
#match id values
treatment_probabilities_corn$ID <- as.numeric(treatmentid$ID[match(treatment_probabilities_corn$ApplicationType, treatmentid$ApplicationType)])
treatment_probabilities_soy$ID <- as.numeric(treatmentid$ID[match(treatment_probabilities_soy$ApplicationType, treatmentid$ApplicationType)])

#order to make sure that each application type is paired with its correct 'none' prob
# treatment_probabilities_corn<-treatment_probabilities_corn[with(treatment_probabilities_corn, order(ApplicationTiming..d.)), ]
# treatment_probabilities_soy<-treatment_probabilities_soy[with(treatment_probabilities_soy, order(ApplicationTiming..d.)), ]


#### we sample the actual treated fields      
#because for some compounds, we may have more than one application type, we need to break out the compounds that are double by adding a unique identifier
treatment_probabilities_corn<-treatment_probabilities_corn %>%
  group_by(Compound) %>%
  mutate(compID = paste(Compound, gl(n()/2, 2), sep = "."))

treatment_probabilities_soy<-treatment_probabilities_soy %>%
  group_by(Compound) %>%
  mutate(compID = paste(Compound, gl(n()/2, 2), sep = "."))

corn_samp<-as.data.frame(t(do.call(rbind, 
                           lapply(split(treatment_probabilities_corn, treatment_probabilities_corn$compID), 
                                  function(x) sample(unique(x$ID),size=nrow(field_cornsoy[field_cornsoy$crop==1,]),replace=T, prob=x$probs_treated)))))
      
soy_samp<-as.data.frame(t(do.call(rbind, 
                           lapply(split(treatment_probabilities_soy, treatment_probabilities_soy$compID), 
                                  function(x) sample(unique(x$ID),size=nrow(field_cornsoy[field_cornsoy$crop==5,]),replace=T, prob=x$probs_treated)))))
#combine 
sampled<-bind_rows(corn_samp, soy_samp)
      
#order as needed for rows to match sampled compounds
fcs<-field_cornsoy[order(field_cornsoy$crop),]
fcs<-cbind(fcs,sampled)
      
crop_probs_corn<-corn[which(corn$Year == names(fv[Year])), ] 
crop_probs_soy<-soy[which(soy$Year == names(fv[Year])), ] 

    sample_date_fields<-function(x){
        output<-sample(nrow(x),size=nrow(fcs[fcs$crop==x$Crop,]),replace=T, prob=x$prob)
         output<-x$Week.Ending[match(output,x$SampleP)]
        }
      
sampled_corn_dates<-sample_date_fields(crop_probs_corn)
sampled_soy_dates<-sample_date_fields(crop_probs_soy)
        
#add back to field layer and assign NA if 'none'
fcs$planteddates<-ifelse(fcs$crop ==1, sampled_corn_dates, sampled_soy_dates)

#gather to one column, add type  
fcs<- gather(fcs, "Compound", "n", (contains(c("BIFENTHRIN","CLOTHIANIDIN","CHLORPYRIFOS","CARBOFURAN","GLYPHOSATE","THIAMETHOXAM","CARBARYL","IMIDACLOPRID"))))

#align names and values with pest_cropf
fcs$Commodity<-ifelse(fcs$crop == 1, "CORN","SOYBEANS")
colnames(fcs)[4]<-"Year"

#match treatment IDs to names
fcs$ApplicationType<-treatmentid$ApplicationType[match(fcs$n, as.numeric(treatmentid$ID))]

#need to remove unique identifier from sampled compimds
fcs$Compound<- gsub("\\..*","",fcs$Compound)

#add emerged dates (days) from planting
fcs<-merge(fcs, apprates[ , c("Compound","Commodity","ApplicationType","ApplicationTiming..d.")], by = c("Compound","Commodity", "ApplicationType"), all.x=T)

#change name
colnames(fcs)[10]<-'applicationday'

#final emerged dates
fcs$applicationday<- as.Date(fcs$planteddates)+fcs$applicationday

#remove NA
fcs<-fcs[!is.na(fcs$ApplicationType),]

# head(fcs)
# head(pest_cropf)

#merge to get application rates and prepare to sample
fcst<-merge(fcs, pest_cropf[ , c("Compound","Commodity","Year","ApplicationType","AvgRate_kgacre","probs_application")], by = c("Compound","Commodity","Year","ApplicationType"), all.x=TRUE)


###finally, I need to select the applications that have more than one compound type. 
#I created a function to do this:

#sample based on 'probs application', i.e., the column based on the relative usage proportion by group of insecticides
sample_by_compound<-function(data,x) {filter(data,ApplicationType == x) %>% 
  group_by(id,crop) %>%
  filter(n()>1)%>%
  mutate(pick= sample(Compound, size = 1, prob = probs_application))%>%
  filter(Compound !=pick)%>%
  mutate(Compound=pick)
}


#sample foliar insecticides
fcst_sampled_foliar<- sample_by_compound(fcst,"FoliarI")
#drop pick
fcst_sampled_foliar<-fcst_sampled_foliar[,1:13]
#drop ids in the final field-corn dataset with more than 1 sample of foliar
fcstf<-fcst[!fcst$id %in% fcst_sampled_foliar$id,]
#add back with new assignment
fcstf<-rbind(fcstf,fcst_sampled_foliar)

#sample seed treatment
#if seed treatments are available (starting 2006), sample seed as well 
fcstf<-if(Year>=8){ 
  
  fcst_sampled_seed<- sample_by_compound(fcst,"Seed")
  fcst_sampled_seed<- fcst_sampled_seed[,1:13]
  fcstf<-fcstf[!fcstf$id %in% fcst_sampled_seed$id,]
  fcstf<-rbind(fcstf,fcst_sampled_seed)
  
  }else{fcstf}


fcstf<-fcstf[with(fcstf, order(crop, id)), ]

#head(fcstf)

fcstf<-fcstf[,c(1:8,10:11,13)]

#some optional code to make sure everything works correctly; use testy to pick a combo of id, crop, and application type
# testy<-filter(fcst,ApplicationType == "FoliarI") %>%
#   group_by(id,crop) %>%
#   filter(n()>1)
# #pick an id from the old batch and compare against new; below I used id 210
# testorig<-fcst[fcst$id == 210 & fcst$ApplicationType =='FoliarI' & fcst$crop== 1,]
# testnew<-fcstf[fcstf$id == 210 & fcstf$ApplicationType =='FoliarI'& fcstf$crop== 1,]

#also double check that there are no repeats on herbicide applications; should be 0
# testy<-filter(fcstf,ApplicationType == "FoliarH") %>%
#   group_by(id,crop) %>%
#   filter(n()>1)


#add back to list   
list_of_sampled_fields_by_year[[Year]]<-fcstf

}
      

###Finally, here we write everything out to ESRI shapefile 
#this contains the list of all treated fields by year, for each of 3 foliar/soil scenarios. 
names(list_of_sampled_fields_by_year)<-names(fv)
 
for(i in names(list_of_sampled_fields_by_year)){
  st_write(list_of_sampled_fields_by_year[[i]], paste0(root_data_out, "/all_bombus/modified_sampled_fields/sampled_fields_",i,".shp"), driver = "ESRI Shapefile")
}


}

 
 ##### Examine the output ----
 
 df<-list_of_sampled_fields_by_year[[20]]
 
 df<-scenarios[[20]]

 testy<-df[!df$ApplicationType == "None",]
 test<-testy[testy$Compound == "CHLORPYRIFOS",]
 test<-testy[testy$Compound == "GLYPHOSATE",]

 test_again<-testy[testy$id == 167,]
 unique(testy$Compound)


#let's look at the dates 
 plot(test["planteddates"], col = sf.colors(10), border = 'grey', 
      axes = TRUE)
 
#let's look at seed
 dfs<-df[df$ApplicationType == "FoliarI",]
 
 plot(dfs["Compound"], col = sf.colors(3), border = 'grey', 
      axes = TRUE)
 
 table(dfs$Compound)
 
 ggplot(data = dfs) +
   geom_sf(aes(fill = Compound))
 
 plot(scenarios[[2]]$geometry)
 