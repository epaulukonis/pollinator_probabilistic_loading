### Probabilistic Crop Loading 

### 07 Initial Onsite and Offsite Base Models

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
#defaults
depth<-15
theta<-0.2
foc<-0.01
bulkdensity<-1.5

## Purucker 
#elimination rate via growth
kel_grow<-0.035
#relative growth rate
rgr_corn<-0.066
rgr_soy<-0.06
#mass of seed (g)
m0_corn<-0.25
m0_soy<-0.30
#partition coeff
Fpl<-c(0.35,0.69, 0.69)
Fnl<-c(0.017,0.05, 0.05)
partcoeffn<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM"), (Fnl),paste0("Nectar")))
names(partcoeffn)<-c("Compound","F","Type")
partcoeffp<-as.data.frame(cbind(c("IMIDACLOPRID","CLOTHIANIDIN","THIAMETHOXAM"), (Fpl),paste0("Pollen")))
names(partcoeffp)<-c("Compound","F","Type")
part_coeff<-rbind(partcoeffp, partcoeffn)

# Time

#t<-63:110

##### Onsite ----

### Seed
# Soil
# Nectar and Pollen

seed<-apprates[apprates$ApplicationType == "Seed",]
seed<-merge(seed, part_coeff)

#remove nectar for corn
seed<-seed[!(seed$Commodity == "CORN" & seed$Type == "Nectar"),]
#add in variables
seed$rgr<-ifelse(seed$Commodity =="CORN",rgr_corn,rgr_soy)
seed$mass<-ifelse(seed$Commodity =="CORN",m0_corn,m0_soy)
seed$mg_ai_seed<-ifelse(seed$Commodity == "CORN", seed$AvgRate*(1/2.20462e-6)*(1/29000),  seed$AvgRate*(1/2.20462e-6)*(1/60000))

seed_model<-split(seed, seq(nrow(seed)))

residues_in_nectar_and_pollen_from_seed<-function(x){

  output<- matrix(data=NA, nrow=48, ncol=2)
  output[,2]<-t
  for(i in 1:nrow(output)){
   output[i,1] <-( ((x$mg_ai_seed*exp(-(kel_grow+x$rgr)*output[i,2]))/x$mass)*as.numeric(x$F) )*1000
  }
  output<-as.data.frame(output)
  names(output)<-c('residues',"days")
  output[,3:5]<-x[,c(1,5,11)]
  
  # output[1:31,1]<-0
  output
 
}

seed_residues<-lapply(seed_model,residues_in_nectar_and_pollen_from_seed)

testx<-do.call("rbind", seed_residues)




p <- ggplot(testx, aes(days, residues, group=interaction(Compound,Commodity), color=interaction(Compound,Commodity))) +
  geom_point()+
  geom_line()+
  facet_wrap(~Type)+
  ylab("Residues in Media [ug/g]")+
  xlab("Time [days]]")+
  guides(color=guide_legend(title="Compound and Crop"))+
  theme_bw()
p




# Air
#This represents the Krupke relationship between distance and estimated concentration
d<-1:299*0.3048
x<-exp(1.68-0.0905*log(d))-1
orig_conc_at_distance<-(x/1855)*1000 #ug/m2

#because we don't exactly know the concentration from Krupke, we'll take the average of the two label rates and apply a standard seed estimate to get conc

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
  theme_bw()
p

#get the seed treatment data 
seed_treatments<-apprates[apprates$ApplicationType == "Seed",]
seed_treatments$ug_m2<-seed_treatments$AvgRate*112085
seed_conc<-merge(seed_treatments,air_krupke)
seed_conc<-seed_conc[with(seed_conc, order(Compound,Commodity)), ]
seed_conc$dust_drift_conc<-seed_conc$ug_m2*seed_conc$deposition

#show seed dust concentration by group
# p <- ggplot(seed_conc, aes(d, dust_drift_conc, group=Compound, color=Compound)) +
#   geom_point()+
#   geom_line()+
#   facet_wrap(~Commodity)+
#   ylab("Deposition (ug/m2)")+
#   xlab("Distance[m]")+
#   theme_bw()
# p


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

#convert to meters
ag_drift$distance<-ag_drift$distance*0.3048
  
p <- ggplot(ag_drift, aes(distance,pond_ground_high_vf2f)) +
  geom_point()+
  geom_line()+
  ylab("Deposition (Fraction of In-Field Concentration)")+
  xlab("Distance[m]")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p


#let's attempt to fit the curve
fit<-nls(pond_ground_high_vf2f ~ SSasymp(distance, yf, y0, decrate), data = ag_drift)
summary(fit)
ag_drift$pred<-predict(fit)

data<-ag_drift[,c(1,5)]
names(data)<-c("x","y")

fo3 <- y ~ 1/(b + x^c) # omit .lin parameter; plinear will add it automatically
fm3 <- nls(fo3, data = data, start = list(b = 1, c = 1), alg = "plinear")



Plot <- function(data, fm, main) {
  plot(y ~ x, data, pch = 20)
  lines(fitted(fm) ~ x, data, col = "red")
  legend("topright", bty = "n", cex = 0.7, legend = capture.output(fm))
  title(main = paste(main, "- AIC:", round(AIC(fm), 2)))
}  

Plot(data, fm3, "3 parameters")



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
PWC
# Nectar
AgDrift + Li?
# Pollen
# Air
# Water

### Soil
# Soil
PWC?
# Nectar
Briggs
# Pollen
Briggs
# Water
PWC?

### Foliar
# Soil
Agdrift
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))

#convert to meters
ag_drift$distance<-ag_drift$distance*0.3048

p <- ggplot(ag_drift, aes(distance,pond_ground_high_vf2f)) +
  geom_point()+
  geom_line()
p
# Nectar
Li+AgDrift
# Pollen
Li+AgDrift
# Air
AgDrift
