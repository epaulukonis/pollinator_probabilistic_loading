### Probabilistic Crop Loading 

### 11 Comparison to available empirical values 

# Edited by E. Paulukonis December 2023


#### App rates and timing:
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)


#these are the only residues we'll measure as ug/m2
ag_drift <-read.csv(paste0(pest_dir, "/Models/agdrift_database.csv"))

#convert to meters
ag_drift$distance<-ag_drift$distance*0.3048


#### Set up and read in variables for models:
## Foliar
Li<-read.csv(paste0(pest_dir,"/Models/LiParam.csv"))
Li<-Li %>% spread(Parameter, Value)
Li$Compound<-toupper(Li$Compound)

## Seed
#elimination rate via growth
kel_grow<-0.035
#relative growth rate
rgr_soy<-0.06
#mass of seed (g)
m0_soy<-0.15

emp<-read.csv(paste0(pest_dir,"/Residues/EmpiricalComp.csv"))
emp$Compound<-toupper(emp$Compound)
emp<-merge(x = emp, y = apprates[ , c("Compound","k_values")], by = "Compound", all.x=TRUE)
emp<-emp[!duplicated(emp), ]

emp$DaysPostApplication<-ifelse(emp$DaysPostApplication == 0, 0.0833, emp$DaysPostApplication)
by_application_type<-split(emp,list(emp$ApplicationType))


#### Seed----
seed<-by_application_type[[2]]

##Soil
s_soil<-seed[seed$Media == "Soil",]
depo<-air_krupke[1,]
s_soil<-merge(s_soil,depo) #merge with the fraction estimated via Krupke
s_soil$ugm2<-s_soil$Application.Rate*112085 #convert to ug/m2

s_soil$conc<-s_soil$ugm2*s_soil$deposition #add soil_deposition
s_soil$soil_concentration<-s_soil$conc * (1/10000) #convert to ug/cm2

s_soil$conc_from_dust<-(s_soil$soil_concentration/2 * 1/1.95)*exp(-(s_soil$k_values*s_soil$DaysPostApplication)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
s_soil$conc_from_application<-( ((0.368*100) /(1.95*0.05*1000))*exp(0-s_soil$k_values * s_soil$DaysPostApplication)) *s_soil$Application.Rate*1.12085 #now add that amount to the amount in soil using Li (converting AR to kg/ha)estimate for soil concentration. I modified mass fraction to be 40%, and soil bulk density to be 1.44 to match 
s_soil$Soil_concentration_ug_g_from_seed<-s_soil$conc_from_dust+s_soil$conc_from_application

s_soil<-s_soil[,c(1:2,4:22,3)]
colnames(s_soil)[21]<-"Estimated"
colnames(s_soil)[22]<-"Empirical"
s_soil<-gather(s_soil, "Source", "Value", 21:ncol(s_soil))

s_soil$Source<-factor(s_soil$Source, levels = c("Empirical","Estimated"))

ssoil_comp<- ggplot(s_soil, aes(DaysPostApplication, Value, color=Source)) +
  geom_point(aes(shape=Plant), size=2.5)+
  geom_smooth(se = FALSE)+

  #scale_shape_manual(values=c(1,4,8))+
  scale_x_continuous()+
  # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(Compound), scales="free_y")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  ggtitle(paste0(s_soil$ApplicationType," Application - ", s_soil$Media)) +
  #geom_text(x=125, y=0.05, label="Soil", color="black", size=6)+
  theme_bw()+
  theme()+
  xlab("")+
  ylab("")+
  #theme(plot.margin = unit(c(1,1,1,1), "cm"))
  theme(legend.position="right", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
ssoil_comp





##Pollen
s_pol<-seed[seed$Media == "Pollen",]

s_pol<-merge(s_pol, part_coeff, by="Compound")
s_pol<-s_pol[!s_pol$Type == "Nectar",]
s_pol$kg_ha<-s_pol$Application.Rate*1.12085 

#add in variables
s_pol$rgr<-rgr_soy # rgr of seed, sub soy for cotton
s_pol$mass<-m0_soy #mass of seed, sub soy for cotton

# use conversion and seeding rates to mg ai per seed
s_pol$mg_ai_seed<- as.numeric(s_pol$Application.Rate)*(1/2.20462e-6)*(1/60000)
s_pol<-merge(s_pol,Li) 


TSCF<- 0.7*exp(-((s_pol$logKow-3.07)^2/2.44))   

# we have two components: a concentration in plant as growth occurs, and an uptake from soil aspect
  conc_in_plant <-( (s_pol$mg_ai_seed *0.20/s_pol$mass)*TSCF*exp(-(kel_grow+s_pol$rgr)*s_pol$DaysPostApplication)*as.numeric(s_pol$Fr) )*1000 # units will be ug per g of pollen; the 1000 converts mg to ug
  rud_from_soil_uptake<-(  ((s_pol$`Kp-L`*s_pol$fsoil*s_pol$cf1)/(s_pol$Psoil*s_pol$Hsoil*s_pol$cf2)) * (s_pol$KupSoil/(s_pol$KelAir+s_pol$KelDeg+s_pol$KelGrow - s_pol$KdissSoil)) * (exp(-s_pol$KdissSoil*s_pol$DaysPostApplication) - exp(-(s_pol$KelAir+s_pol$KelDeg+s_pol$KelGrow)*s_pol$DaysPostApplication)) ) 
  conc_from_soil_uptake<- rud_from_soil_uptake* s_pol$kg_ha
  s_pol$Pollen_concentration_ug_g_from_seed<- conc_in_plant +  conc_from_soil_uptake

  s_pol<-s_pol[,c(1:2,4:39,3)]
  colnames(s_pol)[38]<-"Estimated"
  colnames(s_pol)[39]<-"Empirical"
  s_pol<-gather(s_pol, "Source", "Value", 38:ncol(s_pol))
  
  s_pol$Source<-factor(s_pol$Source, levels = c("Empirical","Estimated"))
  
  spol_comp<- ggplot(s_pol, aes(DaysPostApplication, Value, color=Source)) +
    geom_point(aes(shape=Plant), size=2.5)+
    geom_smooth(se = FALSE)+
    #scale_shape_manual(values=c(1,4,8))+
    scale_x_continuous()+
    # facet_wrap(~Commodity,scales = "free", nrow=1)+
    facet_grid(rows = vars(Compound), scales="free_y")+
    ylab("Residues [ug/g]")+
    xlab("Days Post-Application")+
    #geom_text(x=125, y=0.05, label="Soil", color="black", size=6)+
    ggtitle(paste0(s_pol$ApplicationType," Application - ", s_pol$Media)) +
    theme_bw()+
    theme()+
    xlab("")+
    ylab("")+
    #theme(plot.margin = unit(c(1,1,1,1), "cm"))
    theme(legend.position="right", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
  spol_comp
  

#### Foliar----
foliar<-by_application_type[[1]]

##Soil
f_soil<-foliar[foliar$Media == "Soil",]
depo<-ag_drift[1,5]
f_soil$deposition<-depo #merge with the fraction estimated via Krupke
f_soil$ugm2<-as.numeric(f_soil$Application.Rate)*112085 #convert to ug/m2
  
f_soil$conc<-f_soil$ugm2*f_soil$deposition #add soil_deposition
f_soil$soil_concentration<-f_soil$conc * (1/10000) #convert to ug/cm2
  
conc_from_drift<- (f_soil$soil_concentration/2 * 1/1.95) *exp(-(f_soil$k_values*f_soil$DaysPostApplication)) #assume 2cm mixing depth, here we use soil bulk density to estimate ug per g of soil
conc_from_application<-( ((0.368*100) /(1.95*0.05*1000))*exp(0-f_soil$k_values * f_soil$DaysPostApplication)) *as.numeric(f_soil$Application.Rate)*1.12085 #now add that amount to the amount in soil using Li (converting AR to kg/ha)estimate for soil concentration. I modified mass fraction to be 40%, and soil bulk density to be 1.44 to match 

#here we combine the deposition from drift with the concentration already in the soil from the foliar treatment 
f_soil$Soil_concentration_ug_g_from_foliar<-conc_from_drift+conc_from_application

f_soil<-f_soil[,c(1:2,4:18,3)]
colnames(f_soil)[17]<-"Estimated"
colnames(f_soil)[18]<-"Empirical"
f_soil<-gather(f_soil, "Source", "Value", 17:ncol(f_soil))

f_soil$Source<-factor(f_soil$Source, levels = c("Empirical","Estimated"))

fsoil_comp<- ggplot(f_soil, aes(DaysPostApplication, Value, color=Source)) +
  geom_point(aes(shape=Plant), size=2.5)+
  geom_smooth(se = FALSE)+
  #scale_shape_manual(values=c(1,4,8))+
  scale_x_continuous()+
  # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(Compound), scales="free_y")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  #geom_text(x=125, y=0.05, label="Soil", color="black", size=6)+
  ggtitle(paste0(f_soil$ApplicationType," Application - ", f_soil$Media)) +
  theme_bw()+
  theme()+
  xlab("")+
  ylab("")+
  #theme(plot.margin = unit(c(1,1,1,1), "cm"))
  theme(legend.position="right", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
fsoil_comp


##Pollen
f_pol<-foliar[foliar$Media == "Pollen",]
f_pol$kg_ha<-f_pol$Application.Rate*1.12085 # convert to kg/ha for this one; we will convert to ug per g in a bit; this matches Li

f_pol<-merge(f_pol,Li) 

pollen_rud <-( (f_pol$`Kp-L`*f_pol$fsoil*f_pol$cf1/(f_pol$Psoil*f_pol$Hsoil*f_pol$cf2)) * (f_pol$KupSoil/(f_pol$KelAir+f_pol$KelDeg+f_pol$KelGrow - f_pol$KdissSoil)) * (exp(0-f_pol$KdissSoil*f_pol$DaysPostApplication) - exp(0-(f_pol$KelAir+f_pol$KelDeg+f_pol$KelGrow)*f_pol$DaysPostApplication)) )  +
  ( (f_pol$`Kp-L`*f_pol$fsurface*f_pol$cf1/(f_pol$LAI*f_pol$LMA*(1/(1-f_pol$Wleaf))*1000)) * (f_pol$KupSurface/(f_pol$KelAir+f_pol$KelDeg+f_pol$KelGrow - f_pol$KdissSurface)) * ( exp(-f_pol$KdissSurface*f_pol$DaysPostApplication) - exp(-(f_pol$KelAir+f_pol$KelDeg+f_pol$KelGrow)*f_pol$DaysPostApplication)) )

f_pol$Pollen_concentration_ug_g_from_foliar<-(pollen_rud*f_pol$kg_ha)

f_pol<-f_pol[,c(1:2,4:34,3)]
colnames(f_pol)[33]<-"Estimated"
colnames(f_pol)[34]<-"Empirical"
f_pol<-gather(f_pol, "Source", "Value", 33:ncol(f_pol))

f_pol$Source<-factor(f_pol$Source, levels = c("Empirical","Estimated"))
f_pol<-f_pol[f_pol$Application.Rate == 0.032,]

fpol_comp<- ggplot(f_pol, aes(DaysPostApplication, Value, color=interaction(Source))) +
  geom_point(aes(shape=Plant), size=2.5)+
  geom_smooth(se = FALSE)+
  #scale_shape_manual(values=c(1,4,8))+
  scale_x_continuous()+
  # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(Compound), scales="free_y")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  #geom_text(x=125, y=0.05, label="Soil", color="black", size=6)+
  ggtitle(paste0(f_pol$ApplicationType," Application - ", f_pol$Media)) +
  theme_bw()+
  theme()+
  xlab("")+
  ylab("")+
  #theme(plot.margin = unit(c(1,1,1,1), "cm"))
  theme(legend.position="right", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
fpol_comp


#### Plot ----

emp_vs_est<-plot_grid(
  fsoil_comp,
  fpol_comp,
  ssoil_comp,
  spol_comp,

  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=2, ncol=2,rel_widths = c(3,3,3,3))


emp_vs_est


y.grob <- textGrob("Residue Concentrations [ug/g]", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Days Post Application", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

grid.arrange(arrangeGrob(emp_vs_est, left = y.grob, bottom = x.grob))
