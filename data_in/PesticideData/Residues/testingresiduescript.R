
##### Residue Testing ----

## in this script, I will be testing 4 methods for deriving in-field residue amounts
## I wiill compare the results to reported residues from the Neonicotinoid Final Bee Risk Assessment

library(tidyverse)
library(dplyr)
library(stringr)
library(gridExtra)

options(scipen=999)

who_is_running<-'eap'
#who_is_running<-'stp'
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  root_dir <- file.path("c:", "git", "pollinator_probabilistic_loading")
}else if (Sys.info()[4]=="LZ26EPAULUKO"){
  root_dir <- 'C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/pollinator_probabilistic_loading'
}else{
  root_dir <- file.path("/work", "HONEYBEE", who_is_running, "pollinator_probabilistic_loading")
}
print(root_dir)

root_data_in <- file.path(root_dir, "data_in")
setwd(paste0(root_data_in,"/PesticideData/Residues"))

## read in residues from DER
res<-read.csv("DER_ResidueData.csv")
res<-na.omit(res)

#get accurate timing of plant
timing<-c(63,50,45)
plants<-c("Corn","Cotton","Soy")
# timing<-as.data.frame(cbind(plants, timing))
# names(timing)<-c("plants","t")
# timing$t<-as.numeric(timing$t)



##### Li et al. ----
Li<-read.csv("LiParam.csv")
Li<-Li %>% spread(Parameter, Value)
#t<-60

compounds<-Li[,1]
Li<-Li[,2:13] #need to coerce to dataframe without any character vectors

rud_listn<-list()
rud_listp<-list()
for(time in 1:length(timing)){
  t<-timing[time]

#here, we calculate the RUD for each unit
RUDn<-apply(Li, 1, function(x) ((x[9]*x[3]*x[1])/(x[12]*x[4]*x[2])) * (x[11]/(x[6]+x[7]+x[8] - x[5])) * (exp(-x[5]*t) - exp(-(x[6]+x[7]+x[8])*t)) )
RUDp<-apply(Li, 1, function(x) ((x[10]*x[3]*x[1])/(x[12]*x[4]*x[2])) * (x[11]/(x[6]+x[7]+x[8] - x[5])) * (exp(-x[5]*t) - exp(-(x[6]+x[7]+x[8])*t)) )

rud_listn[[time]]<-RUDn
rud_listp[[time]]<-RUDp

}

rudn<-as.data.frame(do.call("rbind", rud_listn))
names(rudn)<-paste0(plants)
rudn<-gather(rudn, "Plant", "RUDn",1:3)
#rudn$Type<-"Nectar"

rudp<-as.data.frame(do.call("rbind", rud_listp))
names(rudp)<-paste0(plants)
rudp<-gather(rudp, "Plant", "RUDp",1:3)
#rudp$Type<-"Pollen"


Li$Compound<-compounds
Li<-rbind(Li,Li,Li)
Li<-cbind(Li,rudn,rudp)


#now, we'll need to calculate a comparable application rate in the same units, which are kg/ha
#we'll need to figure out how to convert mg/a.i. seed to kg/ha 

#I also need to figure out if the N-L and P-L partition coeff are AR specific...

residues<-split(res,res$Plant)

#this is the max seeding rate per acre
seedcornha<-40250*2.47105 #seed/ha
seedsoyha<-130000*2.47105 #seed/ha
seedcottonha<-30000*2.47105 #seed/ha

#here, we need to convert the application rates in the residue data to kg/ha, same as Li, using a conversion based on seeds planted/ha
corn<-residues[[1]]
corn$kgperha<-corn$Original.treatment.rate.from.study..mg.a.i..seed.*seedcornha*0.000001 #convert from mg to kg

soy<-residues[[2]]
soy$kgperha<-soy$Original.treatment.rate.from.study..mg.a.i..seed.*seedsoyha*0.000001 #convert from mg to kg

cotton<-residues[[3]]
cotton$kgperha<-cotton$Original.treatment.rate.from.study..mg.a.i..seed.*seedcottonha*0.000001 #convert from mg to kg

residues<-rbind(corn,cotton,soy)
residues<- residues %>% mutate(Chemical = str_to_title(Chemical))

Li_sub<-Li[,c(13:15,17)]


#here is where we multiply the RUD at time t by the converted AR in kg/ha, and get the outcome in ug/g
residues<-merge(residues, Li_sub, by.x=c("Chemical", "Plant"), by.y=c("Compound", "Plant"), all.x=TRUE, all.y=FALSE) 
residues$Cn_Li<-(residues$RUDn*residues$kgperha)*1000 #convert to ug/g
residues$Cp_Li<- (residues$RUDp*residues$kgperha)*1000 #convert to ug/g

#remove irrelevant entries for pollen and nectar
residues$Cn_Li<-ifelse(residues$Type == "Pollen", residues$Cn_Li == NA, residues$Cn_Li)
residues$Cp_Li<-ifelse(residues$Type == "Nectar", residues$Cp_Li == NA, residues$Cp_Li)

#here, we un-normalize the residue values and convert them to ng/g
residues$Cpunits<-"ug/g"
residues$Residues_DER_ugg<-((as.numeric(residues$Mean..ng.a.i..g..normalized.to.1.3.mg.a.i..seed.)*as.numeric(residues$Original.treatment.rate.from.study..mg.a.i..seed.))/residues$Normalized.To)/1000
#residues$Residues_DER_ngg_MAX<-(as.numeric(residues$Maximum.single.value)*residues$Normalized.To)/1000


residues$Residues_Li_ugg<- coalesce(residues$Cn_Li, residues$Cp_Li)
residues_Li<-residues

#residues<-gather(residues, "source", "Dose",18:19)

# ggp <- ggplot(residues_Li, aes(log(Residues_DER_ngg), log(Residues_Li_ngg), color=Chemical, shape=Type)) +
#   geom_point()+
#   stat_smooth(method = "lm",
#               formula = y ~ x,
#               geom = "smooth") +
#   facet_grid(rows = vars(Chemical), cols = vars(Type),
#              scales="free_y", switch = 'y')
# 
# ggp

# p <- ggplot(residues_Li, aes(x=source, y=log(Dose))) +
#   geom_boxplot()+
#   facet_grid(rows = vars(Plant), cols = vars(Type),
#              scales="free_y", switch = 'y')+
#   labs(y = expression(paste('Residues [log ng ', g^-1, ' ]')),
#        x = 'Source')
# p

#der x, predictions on y and line through



##### Nomagram ----
nom<-read.csv("NomagramParam.csv")
halflife<-as.data.frame(cbind(c("Imidacloprid","Thiamethoxam","Clothianidin"), c(39,9,148)))
names(halflife)<-c("Compound","HL")

residues<-split(res,res$Plant)

seedcornha<-40250*2.47105 #seed/ha
seedsoyha<-130000*2.47105 #seed/ha
seedcottonha<-30000*2.47105 #seed/ha

#convert AR to lbs/acre
corn<-residues[[1]]
corn$lbperacre<-(corn$Original.treatment.rate.from.study..mg.a.i..seed.*seedcornha)/453592#convert mg to lbs

soy<-residues[[2]]
soy$lbperacre<-(soy$Original.treatment.rate.from.study..mg.a.i..seed.*seedsoyha)/453592 #convert mg to lbs

cotton<-residues[[3]]
cotton$lbperacre<-(cotton$Original.treatment.rate.from.study..mg.a.i..seed.*seedcottonha)/453592 #convert mg to lbs

residues<-rbind(corn,cotton,soy)
residues<- residues %>% mutate(Chemical = str_to_title(Chemical))
residues$HL<-as.numeric(halflife$HL[match(residues$Chemical, halflife$Compound)])#get foliar dissipation half life
residues$C0<-residues$lbperacre*110 #get initial concentration

#get accurate timing of plant
timing<-c(63,50,45)
plants<-c("Corn","Cotton","Soy")
timing<-as.data.frame(cbind(plants, timing))
names(timing)<-c("plants","t")
timing$t<-as.numeric(timing$t)

residues$t<-timing$t[match(residues$Plant, timing$plants)]
residues$k<-log(2)/residues$HL

residues$Ct_ppm<-residues$C0*exp(-(residues$k*residues$t))

residues$Ct_ugg<-residues$Ct_ppm
residues$Residues_DER_ugg<-((as.numeric(residues$Mean..ng.a.i..g..normalized.to.1.3.mg.a.i..seed.)*as.numeric(residues$Original.treatment.rate.from.study..mg.a.i..seed.))/residues$Normalized.To)/1000

residues_nom<-residues

# ggp2 <- ggplot(residues_nom, aes(log(Residues_DER_ngg), log(Ct_ngg), color=Chemical, shape=Type)) +
#   geom_point()+
#   stat_smooth(method = "lm",
#               formula = y ~ x,
#               geom = "smooth") +
#   facet_grid(rows = vars(Chemical), cols = vars(Type),
#              scales="free_y", switch = 'y')
# ggp2



#### Etterson et al. ----
ett<-read.csv("EttParam.csv")

#Cnectar =  Cpollen = Csoil(t)*[10(0.95*LogKow-2.05)+0.82]*TSCF*[p/( theta+p*Koc*foc)]
#Csoil(t)=[(AR*1.12)/d]*e-kt

residues<-split(res,res$Plant)

#first, convert AR to kg/ha
#this is the max seeding rate per acre
seedcornha<-40250*2.47105 #seed/ha
seedsoyha<-130000*2.47105 #seed/ha
seedcottonha<-30000*2.47105 #seed/ha

#here, we need to convert the application rates in the residue data to kg/ha, same as Li, using a conversion based on seeds planted/ha
corn<-residues[[1]]
corn$kgperha<-corn$Original.treatment.rate.from.study..mg.a.i..seed.*seedcornha*0.000001

soy<-residues[[2]]
soy$kgperha<-soy$Original.treatment.rate.from.study..mg.a.i..seed.*seedsoyha*0.000001

cotton<-residues[[3]]
cotton$kgperha<-cotton$Original.treatment.rate.from.study..mg.a.i..seed.*seedcottonha*0.000001

residues<-rbind(corn,cotton,soy)
residues<- residues %>% mutate(Chemical = str_to_title(Chemical))

#first, calculate Csoil from AR and basic half life information
d=15
residues$HL<-as.numeric(ett$Soil.Half.Life[match(residues$Chemical, ett$Compound)])#get foliar dissipation half life

#get accurate timing of plant
timing<-c(63,50,45)
plants<-c("Corn","Cotton","Soy")
timing<-as.data.frame(cbind(plants, timing))
names(timing)<-c("plants","t")
timing$t<-as.numeric(timing$t)
residues$t<-timing$t[match(residues$Plant, timing$plants)]

residues$k<-log(2)/residues$HL
residues$Csoil <- (residues$kgperha/d)*exp(-(residues$k * residues$t))

#Next, calculate TSCF from Log Kow for each coumpound
ett$TSCF<-0.0648*((ett$LogKow)^2)*0.241*ett$LogKow+0.5822

foc<-0.01
p<-1.5
swc<-0.2

residues$logKow<-ett$LogKow[match(residues$Chemical, ett$Compound)]
residues$TSCF<-ett$TSCF[match(residues$Chemical, ett$Compound)]
residues$koc<-ett$Koc[match(residues$Chemical, ett$Compound)]
residues$Cnp<-residues$Csoil*(10^(0.95*residues$logKow-2.05)+0.82)*residues$TSCF*(p/(swc+p*residues$koc*foc))

residues$Residues_DER_ugg<-((as.numeric(residues$Mean..ng.a.i..g..normalized.to.1.3.mg.a.i..seed.)*as.numeric(residues$Original.treatment.rate.from.study..mg.a.i..seed.))/ residues$Normalized.To)/1000

residues_ett<-residues

#### Weighted degradation ----

residues<-res
residues<- residues %>% mutate(Chemical = str_to_title(Chemical))
residues$HL<-as.numeric(ett$Soil.Half.Life[match(residues$Chemical, ett$Compound)])#get foliar dissipation half life

#get accurate timing of plant
timing<-c(63,50,45)
plants<-c("Corn","Cotton","Soy")
timing<-as.data.frame(cbind(plants, timing))
names(timing)<-c("plants","t")
timing$t<-as.numeric(timing$t)
residues$t<-timing$t[match(residues$Plant, timing$plants)]
residues$k<-log(2)/residues$HL

plants<-c("Corn","Cotton","Soy")
weight<-c(700,30,45)
weights<-as.data.frame(cbind(plants, weight))
names(weights)<-c("plants","weights")

residues$grams<-as.numeric(weights$weights[match(residues$Plant, weights$plants)])


residues$conc<-(residues$Original.treatment.rate.from.study..mg.a.i..seed.*exp(-(residues$k*residues$t))/residues$grams)*1000 #get to ug/g
residues_simple<-residues


#AR(g/seed)*decay function/weight of single plant in g)



##### ad hoc version ----
residues<-res
residues<- residues %>% mutate(Chemical = str_to_title(Chemical))

#get accurate timing of plant
timing<-c(63,50,45)
#timing<-c(5,5,5)
plants<-c("Corn","Cotton","Soy")
timing<-as.data.frame(cbind(plants, timing))
names(timing)<-c("plants","t")
timing$t<-as.numeric(timing$t)

residues$t<-timing$t[match(residues$Plant, timing$plants)]
residues$k<-0.035
residues$rgr<-0.066

#add weight of plants
plants<-c("Corn","Cotton","Soy")
weight<-c(0.25,0.1,0.30)
weights<-as.data.frame(cbind(plants, weight))
names(weights)<-c("plants","weights")
# plants<-c("Corn","Cotton","Soy")
# weight<-c(700,30,45)
# weights<-as.data.frame(cbind(plants, weight))
# names(weights)<-c("plants","weights")

#mg/g
((0.25/0.5)*exp(-(0.035+0.066)*60))*0.69
#based on mass instead of concentration?



residues$M0<-as.numeric(weights$weights[match(residues$Plant, weights$plants)])

#add partitioning coeff
# Knl<-c(0.807, 0.857, 0.880)
# Kpl<-c(0.859, 0.645, 0.349)
# partcoeffn<-as.data.frame(cbind(c("Clothianidin","Imidacloprid","Thiamethoxam"), Knl,paste0("Nectar")))
# names(partcoeffn)<-c("Compound","Kxl","Type")
# partcoeffp<-as.data.frame(cbind(c("Clothianidin","Imidacloprid","Thiamethoxam"), Kpl,paste0("Pollen")))
# names(partcoeffp)<-c("Compound","Kxl","Type")

#because I couldn't find any clothianidan residues for leaf, nectar, and pollen, I subbed thiamethhoxam, which has a similar 
Fpl<-c(0.35,0.69, 0.69)
Fnl<-c(0.017,0.05, 0.05)
partcoeffn<-as.data.frame(cbind(c("Imidacloprid","Clothianidin","Thiamethoxam"), Fnl,paste0("Nectar")))
names(partcoeffn)<-c("Compound","F","Type")
partcoeffp<-as.data.frame(cbind(c("Imidacloprid","Clothianidin","Thiamethoxam"), Fpl,paste0("Pollen")))
names(partcoeffp)<-c("Compound","F","Type")

partcoeff<-rbind(partcoeffn,partcoeffp)
residues<-merge(residues, partcoeff, by.x=c("Chemical", "Type"), by.y=c("Compound","Type"), all.x=TRUE, all.y=FALSE) 


#str(residues)
residues$Ct_ugg<-((  (residues$Original.treatment.rate.from.study..mg.a.i..seed.*exp(-(residues$k+residues$rgr)*residues$t)/residues$M0) * .20) *as.numeric(residues$F))*1000
#issue: growth function way underestimates growth of plants

residues$Residues_DER_ugg<-((as.numeric(residues$Mean..ng.a.i..g..normalized.to.1.3.mg.a.i..seed.)*as.numeric(residues$Original.treatment.rate.from.study..mg.a.i..seed.))/residues$Normalized.To)/1000

residues_adhoc<-residues

#AR(g/seed)*decay function/weight of single plant in g)


sd(residues_adhoc$Ct_ugg)
sd(residues_adhoc$Residues_DER_ugg)

mean(residues_adhoc$Ct_ugg)
mean(residues_adhoc$Residues_DER_ugg)

(0.02-0.0028)/0.0024


#### putting it all together ----

residues_all<-cbind(residues_Li[,c(1,12,20,21)], residues_nom[,20], residues_ett[,22], residues_simple[18], residues_adhoc[,19])

names(residues_all)[5:8]<-c("Residues_nomogram_ugg","Residues_Etterson_ugg", 'Residues_simple_ugg', 'Residues_adhoc_ugg')
residues_all<-gather(residues_all, "source", "Concentration",4:8)

# ggptype <- ggplot(residues_all, aes(log(Residues_DER_ugg), log(Concentration), color=Chemical, shape=Type)) +
#   geom_point()+
#   coord_cartesian(xlim =c(-10,10), ylim = c(-10,10))+
#   geom_abline(intercept = 0, slope = 1, size = 0.5)
# 
# ggptype

residues_all$residuals<-residues_all$Concentration -  residues_all$Residues_DER_ugg


residues_all<-residues_all %>% group_by(source) %>% mutate(maxr=max(Concentration)) %>% mutate(minr=min(Concentration))

mytable <- residues_all %>% 
  group_by(source) %>% 
  summarise(across(c(minr,maxr), unique))


mino<-min(residues_all$Residues_DER_ugg)
maxo<-max(residues_all$Residues_DER_ugg)


#log(absolute)

original<-paste0("min, max of DER:", round(mino,5),", ",round(maxo, 5))


#line plot using log10 (displays decimals)
ggpsource <- ggplot(residues_all, aes(log10(Residues_DER_ugg), log10(Concentration), color=Chemical, shape=source)) +
  geom_point()+
  coord_cartesian(xlim =c(-5,5), ylim = c(-5,5))+
  geom_abline(intercept = 0, slope = 1, size = 0.5)+
  annotation_custom(tableGrob(mytable), xmin=1, xmax=2, ymin=1.5, ymax=4)+
  annotate("text", x=3, y=0.5, label= original) +
  xlab("log10(Seed residues reported via EPA) [ug/g]")+
  ylab("log10(Modeled residues) [ug/g]")+
  theme_bw()+
  theme(legend.text=element_text(size=14), text=element_text(size=14))
  
ggpsource

#original boxplot
ggp2 <- ggplot(residues_all, aes(Chemical, residuals, color=Chemical)) +
  geom_boxplot()+
  #facet_wrap(~source, scales="free_y")
  facet_grid(rows = vars(source), cols = vars(Type),
             scales="free_y", switch = 'y')

ggp2
#add open/dark symbol for nectar/pollen



#MAD residuals
residues_all<-residues_all %>% group_by(Chemical, Type) %>% mutate(madR=((residuals-mean(residuals))/length(residuals)))
#MAD concentrations
residues_all<-residues_all %>% group_by(Chemical, Type) %>% mutate(abs=((Concentration-mean(Residues_DER_ugg))/length(Residues_DER_ugg)))


#absolute deviation of the residuals of the concentrations, grouped by chemical and type
ggp2 <- ggplot(residues_all, aes(source, (madR), color=Chemical)) +
  geom_boxplot()+
  #facet_wrap(~source, scales="free_y")
  facet_grid(rows = vars(Chemical), cols = vars(Type),
             scales="free_y", switch = 'y')+
   theme(axis.text.x = element_text(angle = 90))

ggp2


#absolute deviation of the individual concentrations from the mean DER, grouped by chemical and type 
ggp2 <- ggplot(residues_all, aes(source, (abs), color=Chemical)) +
  geom_boxplot()+
  #facet_wrap(~source, scales="free_y")
  facet_grid(rows = vars(Chemical), cols = vars(Type),
             scales="free_y", switch = 'y')+
  theme(axis.text.x = element_text(angle = 90))

ggp2


#raw resdiauls
ggp2 <- ggplot(residues_all, aes(source, log10(abs(residuals)), color=Chemical)) +
  geom_boxplot()+
  #facet_wrap(~source, scales="free_y")
  facet_grid(rows = vars(Chemical), cols = vars(Type),
             scales="free_y", switch = 'y')+
  theme(axis.text.x = element_text(angle = 90))

ggp2



residues_all$test<-log10(abs(residues_all$residuals))
