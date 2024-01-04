### Probabilistic Crop Loading 

### 07 Initial TESTING of Onsite and Offsite Base Models

# Edited by E. Paulukonis August 2023


##### Read in data for models ----

#### App rates and timing:
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)

#### Models:
## Li
Li<-read.csv(paste0(pest_dir,"/Models/LiParam.csv"))
Li<-Li %>% spread(Parameter, Value)

## Briggs

## Purucker 


##### Onsite ----

### Seed
# Soil

## Nectar
## Pollen
## Air
#This represents the Krupke relationship between distance and estimated concentration
d<-1:299*0.3048
x<-exp(1.68-0.0905*log(d))-1
orig_conc_at_distance<-(x/1855)*1000 #ug/m2

#because we don't exactly know the concentation from Krupke, we'll take the average of the two label rates and apply a standard seed estimate to get conc


#original conc in field: 
conc_in_field<-((2.06+11.92)/2)*1000#(ug/m2)

#to translate this relationship to other in-field concentrations, 
#I assume that the fraction of off-site deposition based on the original concentration is reflective of all seed treatments
deposition<-(orig_conc_at_distance/conc_in_field) #fraction of off


air_krupke<-as.data.frame(cbind(d,orig_conc_at_distance, deposition))
avg_frac<-(0.001849+0.000319)/2

#add in the initial estimate of the concentration at 0 (in-field)
air_krupke<-rbind(c(0,3.81,avg_frac), air_krupke)

#show original concentration
p <- ggplot(air_krupke, aes(d, deposition*100)) +
  geom_point()+
  geom_line()+
  ylab("Deposition (% of In-Field Concentration)")+
  xlab("Distance[m]")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p

#get the seed treatment data 
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]
seed_treatments$ug_m2<-seed_treatments$AvgRate*112085
seed_conc<-merge(seed_treatments,air_krupke)
seed_conc<-seed_conc[with(seed_conc, order(Compound,Commodity)), ]

seed_conc$dust_drift_conc<-seed_conc$ug_m2*seed_conc$deposition

#show seed dust concentration by group
p <- ggplot(seed_conc, aes(d, dust_drift_conc, group=Compound, color=Compound)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Commodity)+
  ylab("Deposition (ug/m2)")+
  xlab("Distance[m]")+
  theme_bw()
p

#idea is that we use the relationship between the % original concen to estimate that same relationship for other compounds

#how does it compare to ag_drift?

#show in comparison to AgDrift depositon
c<- ggplot(ag_drift, aes(distance,pond_ground_high_vf2f)) +
  geom_point()+
  geom_line()+
  geom_point(air_krupke,mapping=aes(d, (deposition), col='red'))+
  geom_line(air_krupke,mapping=aes(d, (deposition), col='red'))+
  ylab("Deposition [Fraction of In-Field Concentration]")+
  xlab("Distance[m]")+
  annotate(geom="text", x=25, y=0.03, label="AgDrift (Foliar",
           color="black")+
  annotate(geom="text", x=25, y=0.002, label="Krupke (Seed)",
           color="black")+
  theme_bw()+
  theme(legend.position = "none")

c

#let's compare the output to agdrift
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))
ag_drift_sub<-ag_drift[,c(1,5)]

ag_drift_sub$distance<-ag_drift_sub$distance*0.3048
ag_combo<-merge(apprates,ag_drift_sub)
ag_combo<-ag_combo[with(ag_combo, order(Compound,Commodity, ApplicationType)), ]

ag_combo$concentration_ugm2<-ag_combo$ug_m2*ag_combo$pond_ground_high_vf2f
ag_combo<-ag_combo[ag_combo$ApplicationType == "Foliar",]

c<- ggplot(ag_combo, aes(distance,concentration_ugm2, shape=Compound,group=Compound,color=Commodity)) +
  geom_point()+
  geom_line()+
  geom_point(seed_conc,mapping=aes(d, dust_drift_conc, group=interaction(Compound,Commodity), color=Commodity))+
  geom_line(seed_conc,mapping=aes(d, dust_drift_conc, group=interaction(Compound,Commodity), color=Commodity))+
  scale_shape_manual(values=c(1:7))+
  facet_wrap(~ApplicationType, scales='free')+
  ylab("Concentration on Soil[ug/m2]")+
  xlab("Distance[m]")+
  theme_bw()


c


# Water

### Soil
# Soil
# Nectar
# Pollen
# Air 
# Water

### Foliar
# Soil
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))

#feet to meters

p <- ggplot(ag_drift, aes(distance,pond_ground_high_vf2f)) +
  geom_point()+
  geom_line()
p


#let's attempt to fit the curve
fit<-nls(pond_ground_high_vf2f ~ SSasymp(distance, yf, y0, decrate), data = ag_drift)
summary(fit)

ag_drift$pred<-predict(fit)

p <- ggplot(ag_drift, aes(distance,pred)) +
  geom_point()+
  geom_line()+
  geom_point(aes(distance,pond_ground_high_vf2f), col='red')+
  geom_line(aes(distance,pond_ground_high_vf2f), col='red')

p



# Nectar
# Pollen
# Air
# Water





##### Offsite ----
### Seed
# Soil
# Nectar
# Pollen
# Air
# Water

### Soil
# Soil
# Nectar
# Pollen
# Air
# Water

### Foliar
# Soil
# Nectar
# Pollen
# Air
# Water