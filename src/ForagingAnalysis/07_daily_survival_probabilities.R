### Foraging Landscape

### 07 Estimating probability of survival




#param <-read.csv(paste0(pest_dir, "/HC_params.csv"))
tox<-read.csv(paste0(pest_dir, "/BeeTox.csv"))
tox<-tox[,1:3]
tox$Compound<-str_to_title(tox$Compound)

library(drc)
library(ordinal)
library(EnvStats)
library(ExtDist)
library(ggpmisc) 
library(stats)
grab_quantiles <- seq(0.00001, 1, 0.00001) #what's the associated mortality %?
# times 3 for Haber's rule (n=1) assuming derived relationships are for 72 hours and we want 24

#### Code for individual scenarios using Cassandra's data

#### ORAL
# bifenthrin gumbel -0.6375, 1.1569, log10
bifenthrin_tox_quantileso <- ((10^(qgumbel(grab_quantiles, -0.6375, 1.1569)))*3)/1000
# carbaryl gumbel 0.9888, 0.9538, log10
carbaryl_tox_quantileso <- ((10^(qgumbel(grab_quantiles, 0.9888, 0.9538)))*3)/1000
# chlorpyrifos triangular -1.919 1.5918, log10
chlorpyrifos_tox_quantileso <- ((10^qtri(grab_quantiles, min = -1.919, max = 1.5918, mode = (-1.919 + 1.5918)/2))*3)/1000
# clothianidin triangular -2.7065, 0.6504, log10
clothianidin_tox_quantileso <- ((10^qtri(grab_quantiles, min = -2.7065, max = 0.6504, mode = (-2.7065 + 0.6504)/2))*3)/1000
# imidacloprid weibull 8.255, 0.4450
imidacloprid_tox_quantileso <- ((10^qWeibull(grab_quantiles, 8.255, 0.445))*3)/1000
# thiamethoxam logistic -0.0566, 0.3559, log10
thiamethoxam_tox_quantileso <- (10^(qlogis(grab_quantiles, -0.0566, 0.3559))*3)/1000

tox_quantileso <- cbind(grab_quantiles, bifenthrin_tox_quantileso, carbaryl_tox_quantileso, chlorpyrifos_tox_quantileso,
                        clothianidin_tox_quantileso, imidacloprid_tox_quantileso, thiamethoxam_tox_quantileso)
tox_quantileso<-as.data.frame(tox_quantileso)
dro<-gather(tox_quantileso, "Compound","Dose", 2:7)
colnames(dro)[1]<-"Mortality"
dro$Compound<- str_to_title(sub("\\_.*", "", dro$Compound))
dro<-merge(x = dro, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)

# ec90s<-dro[dro$Mortality == 0.90,]
# ec10s<-dro[dro$Mortality == 0.10,]
# 
# slope<-log10(81)/(log10(ec90s$Dose/ec10s$Dose)) #calculates slope from the hill equation
# slopedf<-as.data.frame(cbind(ec90s$Compound,slope))
# names(slopedf)<-c("Compound","Slope")
# slopedf$Slope<-as.numeric(slopedf$Slope)
# dro<-merge(dro,slopedf,by="Compound")

# slope<-3.4 #from white paper, honeybee
# dro <- dro %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Oral_LD50_ug_bee)^Slope)) #again using hill equation
# 
# 
# #look at DR curves
# ggplot(dro, aes(x = (Dose), y = 1-phat)) + 
#   # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
#   geom_line(aes(color = Compound), size=1.2) +
#   geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
#   facet_wrap(~Compound,scales = "free_x")+
#   xlab("Dose (ug/bee)") +
#   ylab("Mortality") +
#   scale_x_continuous(trans=scales::log_trans(),
#                      labels = scales::format_format(digits=3))+
#   theme_bw() +
#   theme(legend.position = "none")


#### CONTACT
# bifenthrin weibull
bifenthrin_tox_quantilesd <- ((10^qWeibull(grab_quantiles, 0.0520, 0.9930))*3)/1000
# carbaryl triangular
carbaryl_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.2441, max = 2.9293, mode = (-3.2441 + 2.9293)/2))*3)/1000
# chlorpyrifos triangular
chlorpyrifos_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.4647, max = 1.0538, mode = (-3.4647 + 1.0538)/2))*3)/1000
# clothianidin triangular
clothianidin_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -2.6705, max = -1.0526, mode = (-2.6705 + -1.0526)/2))*3)/1000
# imidacloprid normal
imidacloprid_tox_quantilesd <- ((10^qnorm(grab_quantiles, -2.0408, 1.1062))*3)/1000
# thiamethoxam logistic -0.0566, 0.3559, log10
thiamethoxam_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.4085, max = -0.3356, mode = (-3.4085 + -0.3356)/2))*3)/1000

tox_quantilesd <- cbind(grab_quantiles, bifenthrin_tox_quantilesd, carbaryl_tox_quantilesd, chlorpyrifos_tox_quantilesd,
                        clothianidin_tox_quantilesd, imidacloprid_tox_quantilesd, thiamethoxam_tox_quantilesd)

tox_quantilesd<-as.data.frame(tox_quantilesd)
drd<-gather(tox_quantilesd, "Compound","Dose", 2:7)
colnames(drd)[1]<-"Mortality"
drd$Compound<- str_to_title(sub("\\_.*", "", drd$Compound))
drd<-merge(x = drd, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)

# ec90s<-drd[drd$Mortality == 0.90,]
# ec10s<-drd[drd$Mortality == 0.10,]
# 
# slope<-log10(81)/(log10(ec90s$Dose/ec10s$Dose)) #calculates slope from the hill equation 
# slopedf<-as.data.frame(cbind(ec90s$Compound,slope))
# names(slopedf)<-c("Compound","Slope")
# slopedf$Slope<-as.numeric(slopedf$Slope)
# drd<-merge(drd,slopedf,by="Compound")

# slope<-3.9 #from white paper, honeybee
# drd <- drd %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Contact_LD50_ug_bee)^slope)) #again using hill equation
# 
# 
# #look at DR curves
# ggplot(drd, aes(x = (Dose), y = 1-phat)) + 
#   # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
#   geom_line(aes(color = Compound), size=1.2) +
#   geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
#   facet_wrap(~Compound,scales = "free_x")+
#   xlab("Dose (ug/bee)") +
#   ylab("Mortality") +
#   scale_x_continuous(trans=scales::log_trans(),
#                      labels = scales::format_format(digits=3))+
#   theme_bw() +
#   theme(legend.position = "none")
# 
dro$Type<-"Oral"
drd$Type<-"Contact"

dr<-rbind(dro,drd)



#### this looks at the curves using the actual EEC data ----
gettails<-function(compound){
  #compound<-exp_dose_output_compounds[[2]]

  compound<- Map(cbind, compound, index = seq_along(compound))
  comp<-as.data.frame(do.call(rbind, compound))
  comp<-comp[!comp$Dose ==0,]

  comp<-comp %>% group_by(type) %>%
    summarise_at(vars(starts_with("Dose")), funs(min,max,median))
  
  
  #comp<-gather(comp, "Stat","EEC",2:4)
  comp
}

eecs<-lapply(exp_dose_output_compounds,gettails) #look at the median/maxs; are they very high? they should not be much higher than those reported for single days in Ch. 2. If so, something is iffy. 

names(eecs)<-c("BIFENTHRIN","CARBARYL","CLOTHIANIDIN","CHLORPYRIFOS","IMIDACLOPRID","THIAMETHOXAM")
eecs<-dplyr::bind_rows(eecs, .id = "Compound")


eecs_sim<-eecs %>% group_by(Compound,type) %>%  
  mutate(Dose= map2(min, max, seq, length.out = 10000)) %>%
  unnest(cols = Dose)

eecs_sim$Compound<-str_to_title(eecs_sim$Compound)
eecs_sim<-merge(x = eecs_sim, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)

eecs_contact<-eecs_sim[eecs_sim$type == "Contact",]
eecs_oral<-eecs_sim[eecs_sim$type == "Oral",]


slope<-3.4 #from white paper, honeybee
dro <- eecs_oral %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Oral_LD50_ug_bee)^slope)) #again using hill equation

slope<-3.9 #from white paper, honeybee
drd <- eecs_contact %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Contact_LD50_ug_bee)^slope)) #again using hill equation


#look at DR curves
ggplot(dro, aes(x = (Dose), y = 1-phat)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")

#look at DR curves
ggplot(drd, aes(x = (Dose), y = 1-phat)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")

dro$Type<-"Oral"
drd$Type<-"Contact"

dr<-rbind(dro,drd)




#### this looks at the curves using some raw sequence data ----

#### Evaluate by scenario ----
evaluate_by_scenario<-function(x){
  ind_scene<-x
  
  sim<-exp_dose_output[[1]]
  sim<-split(sim, sim$Compound)
  
  daily_prob_compound<-list()
  
  for(compound in 1:length(sim)){
    compound<-2
    
  ind_scene<-sim[[compound]]
  ind_scene$Compound<-str_to_title(ind_scene$Compound)

  #get DR curve 
  curve<-dr[dr$Compound %in% ind_scene$Compound,]
  
  #randomly select emergence day from plausible days
  emergence<-sample(90:110,1) #what day does she emerge? 

  #first, set any plausible doses on days associated with certain precipitation or temperature thresholds to 0
  ind_scene[ind_scene$Precip >= 0.4, c("Dose")] <-0 #if precip > 0.4, no foraging
  ind_scene[ind_scene$TempC <=6, c("Dose")] <-0 #if temperature less than 6 degrees C, no foraging
  ind_scene[ind_scene$Day >=283, c("Dose")] <-0 #if day is greater than 283, all dead
  ind_scene[ind_scene$Day <=emergence, c("Dose")] <-0 #if day is less than emergence day, no exposure
 
  # names(ind_scene)[[6]]<-"Dose"
  names(ind_scene)[[8]]<-"Type"
  

  #curve_sub<-curve[,c(6,9:10)] 
  curve_sub<-curve[,c(2,3,6)] 
  colnames(curve_sub)[2]<-"Survival"
  
  
  #Use datatable to get the rolling nearest value for mortality; we'll first establish that to decide how she moves forward
  
  ##ORAL
  require( data.table )
  ind_scenen <- data.table(ind_scene[ind_scene$Type == "Oral",])
  drsim <- data.table(curve_sub[curve_sub$Type == "Oral",])
  setkey(ind_scenen)
  setkey(drsim)
  daily_proboral<-drsim[ind_scenen , on="Dose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  
  ##CONTACT
  require( data.table )
  ind_scenen <- data.table(ind_scene[ind_scene$Type == "Contact",])
  drsim <- data.table(curve_sub[curve_sub$Type == "Contact",])
  setkey(ind_scenen)
  setkey(drsim)
  daily_probcont<-drsim[ind_scenen , on="Dose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  

  initation<-sample((emergence+4):(emergence+25),1) #what hypothetical day does she initiate?
  workerbrood<-sample((initation+22-7):(initation+22+7),1) #what hypothetical day does the first brood emerge? Banks gives 22 days, we add a 1-week SD
  endqueenforaging<-workerbrood
  
  #let's assign a daily pollen and nectar storage that involves the contaminated residues for the queen starting at emergence; we'll hone down these values later
  daily_proboral<- daily_proboral %>% group_by(Compound, Type, Media) %>% mutate(nect_store_res_ug_g = ifelse(Day >= initation & Media == "Nectar", 0.6*(Day-initation)* Dose, 0)) %>% mutate(poll_store_res_ug_g = ifelse(Day >= initation & Media == "Pollen", 0.4*(Day-initation) * Dose, 0))

  daily_proboral<-gather()
  
  
  
  route<-route %>%
  group_by(id = cumsum(!Conc==0)) %>%
    mutate(Concf = ifelse(Conc==0, first(Conc), Conc)) %>%
    mutate(dayn = ifelse(Conc>0, Day - first(Day)+1, Day))

    ind <- which(flower$Concf != lag(flower$Concf))
    flower$Concf[ind] <- sapply(ind, function(i) with(flower, sum(c(Concf[i-1], Concf[i+1]))))
    ind <- which(flower$Concf > lag(flower$Concf))
    flower$Concf[ind:nrow(flower)] <- sapply(ind, function(i) with(flower, Concf[ind-1]))

   flower$exp_dose<-(flower$Concf * 1e-04 * 6.5)* 90 * exp(-(flower$k_values*flower$Day))#calculated as the dose experienced from contact with deposition on flowers, assuming a visit of 90 repeat events, degrades over time
   flower<-subset(flower, select=-c(id,dayn,Concf))

  
  
  
  
  
  
  #now we can break this out into simple daily mortalities
  daily_proboral<-daily_proboral[daily_proboral$Media == "Pollen",]
  
  prob_nest_initation<- apply(daily_proboral[1:365,14], 2, prod) #but need to actually cut off at end of foraging!


  # in how many scenarios does the queen die before initiation? what is the threshold for survival to next day?
  # cumulative mortality at:
  # prob of nest initiation [emerge + random initiation] #if she dies in this period, no go
  # prob of nest establishment [initiation + 2/3 weeks] #if she dies in this period, no go
  # prob larval development # what survival do they have with consumption?
  
  
  daily_prob<-rbind(daily_proboral,daily_probcont)
  daily_prob_compound[[dim]]<-daily_prob
  
  }
  
  
  #order by index and day
  #daily_prob<- daily_prob[order(daily_prob$Type, daily_prob$Day),,drop=FALSE]
  
  

  
  
  
  
  daily_prob$Survival<-1-daily_prob$Mortality
  daily_prob$Survival <- ifelse(daily_prob$Survival >= 0.999, 1,daily_prob$Survival)
  
  daily_prob<- daily_prob[,-c(9,11,12)]
  
  # daily_prob_plot<-  ggplot(daily_prob, aes(x = (Day), y = Survival, group=index)) + 
  #   geom_line(size=1, alpha=0.2) +
  #   geom_vline(xintercept = 100,col="darkgreen")+
  #   geom_vline(xintercept = 121,col="darkblue")+
  #   geom_vline(xintercept = 151,col="darkred")+
  #   
  #   scale_y_continuous(limits = c(0, 1))+
  #   
  #   # geom_segment(aes(x=90,xend=110,y=0.60,yend=0.60), alpha = 0.5)+
  #   # geom_segment(aes(x=111,xend=131,y=0.60,yend=0.60), alpha = 0.5)+
  #   # geom_segment(aes(x=141,xend=161,y=0.60,yend=0.60), alpha = 0.5)+
  #   # 
  #   
  #   geom_text(aes(x=103, label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=11)) +
  #   geom_text(aes(x=124, label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=11))+
  #   geom_text(aes(x=154, label="Queen Ends Foraging", y=0.31), colour="darkred", angle=90, text=element_text(size=11))+
  #   
  #   xlab("Time (Day)") +
  #   ylab("Probability of Survival") +
  #   # scale_x_continuous(trans=scales::log_trans(),
  #   #                    labels = scales::format_format(digits=3))+
  #   theme_bw() +
  #   theme(legend.position = "none")
  # daily_prob_plot
  
  

  
  
  cumulative_survival<- apply(daily_prob[1:365,14], 2, prod) #but need to actually cut off at end of foraging!
  
}




#### Code for individual compound groups

####Evaluate by Compound----
#### Oral ----
# generate daily survival

# bifenthrin gumbel -0.6375, 1.1569, log10
bifenthrin_tox_quantileso <- ((10^(qgumbel(grab_quantiles, -0.6375, 1.1569)))*3)/1000
# carbaryl gumbel 0.9888, 0.9538, log10
carbaryl_tox_quantileso <- ((10^(qgumbel(grab_quantiles, 0.9888, 0.9538)))*3)/1000
# chlorpyrifos triangular -1.919 1.5918, log10
chlorpyrifos_tox_quantileso <- ((10^qtri(grab_quantiles, min = -1.919, max = 1.5918, mode = (-1.919 + 1.5918)/2))*3)/1000
# clothianidin triangular -2.7065, 0.6504, log10
clothianidin_tox_quantileso <- ((10^qtri(grab_quantiles, min = -2.7065, max = 0.6504, mode = (-2.7065 + 0.6504)/2))*3)/1000
# imidacloprid weibull 8.255, 0.4450
imidacloprid_tox_quantileso <- ((10^qWeibull(grab_quantiles, 8.255, 0.445))*3)/1000
# thiamethoxam logistic -0.0566, 0.3559, log10
thiamethoxam_tox_quantileso <- (10^(qlogis(grab_quantiles, -0.0566, 0.3559))*3)/1000

tox_quantileso <- cbind(grab_quantiles, bifenthrin_tox_quantileso, carbaryl_tox_quantileso, chlorpyrifos_tox_quantileso,
                       clothianidin_tox_quantileso, imidacloprid_tox_quantileso, thiamethoxam_tox_quantileso)

tox_quantileso<-as.data.frame(tox_quantileso)

dr<-gather(tox_quantileso, "Compound","Dose", 2:7)
colnames(dr)[1]<-"Mortality"
dr$Compound<- str_to_title(sub("\\_.*", "", dr$Compound))

dr<-merge(x = dr, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)


ec90s<-dr[dr$Mortality == 0.90,]
ec10s<-dr[dr$Mortality == 0.10,]

slope<-log10(81)/(log10(ec90s$Dose/ec10s$Dose)) #calculates slope from the hill equation 
slopedf<-as.data.frame(cbind(ec90s$Compound,slope))
names(slopedf)<-c("Compound","Slope")
slopedf$Slope<-as.numeric(slopedf$Slope)


dr<-merge(dr,slopedf,by="Compound")

dr <- dr %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Oral_LD50_ug_bee)^Slope)) #again using hill equation


#look at DR curves
ggplot(dr, aes(x = (Dose), y = 1-phat)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")




evaluate_by_compound<-function(x){
  ind_compound<-x
 # ind_compound<-exp_dose_output[[1]]
 
  ind_compound<- Map(cbind, ind_compound, index = seq_along(ind_compound))
  ind_compound<-as.data.frame(do.call(rbind, ind_compound))
  ind_compound$Compound<-str_to_title(ind_compound$Compound)
  
  curve<-dr[dr$Compound %in% ind_compound$Compound,]
  
  
  ### oral
  oral<-ind_compound[ind_compound$type == "Oral",]
 # oral$Dose_original<-oral$Dose #if needed, you can compare the OG with these
 
 # oral<-oral[oral$index == 1 | oral$index ==2,]
  

  
 #first, set any plausible doses on days associated with certain precipitation or temperature thresholds to 0
  oral$Dose[oral$Precip >= 0.4] <- 0
  oral$Dose[oral$TempC <=6] <- 0

  
  oral$nectcoll<-0.6*oral$Day
  
  #we want to add some way to measure the queen's daily consumption

  
  #use datatable to get the rolling nearest value
  require( data.table )
  sim <- data.table(oral)
  dr <- data.table(curve)
  setkey(sim, Dose)
  setkey(dr, Dose)
  
  daily_prob<-dr[sim , on="Dose", roll = "nearest" ][order(index,Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  
  
  #order by index and day
  daily_prob<- daily_prob[order( daily_prob$index, daily_prob$Day),,drop=FALSE]
  
  daily_prob$Survival<-1-daily_prob$Mortality
  daily_prob$Survival <- ifelse(daily_prob$Survival >= 0.999, 1,daily_prob$Survival)
  
  daily_prob<- daily_prob[,-c(9,11,12)]
  
  
daily_prob_plot<-  ggplot(daily_prob, aes(x = (Day), y = Survival, group=index)) + 
    geom_line(size=1, alpha=0.2) +
    geom_vline(xintercept = 100,col="darkgreen")+
    geom_vline(xintercept = 121,col="darkblue")+
    geom_vline(xintercept = 151,col="darkred")+
  
   scale_y_continuous(limits = c(0, 1))+
    
    # geom_segment(aes(x=90,xend=110,y=0.60,yend=0.60), alpha = 0.5)+
    # geom_segment(aes(x=111,xend=131,y=0.60,yend=0.60), alpha = 0.5)+
    # geom_segment(aes(x=141,xend=161,y=0.60,yend=0.60), alpha = 0.5)+
    # 
  
    geom_text(aes(x=103, label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=11)) +
    geom_text(aes(x=124, label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=11))+
    geom_text(aes(x=154, label="Queen Ends Foraging", y=0.31), colour="darkred", angle=90, text=element_text(size=11))+
    
    xlab("Time (Day)") +
    ylab("Probability of Survival") +
    # scale_x_continuous(trans=scales::log_trans(),
    #                    labels = scales::format_format(digits=3))+
    theme_bw() +
    theme(legend.position = "none")
daily_prob_plot

cumulative_survival<- apply(daily_prob[1:365,14], 2, prod) #but need to actually cut off at end of foraging!
  
}

### Contact ----
# generate daily survival
# bifenthrin weibull
bifenthrin_tox_quantilesd <- ((10^qWeibull(grab_quantiles, 0.0520, 0.9930))*3)/1000
# carbaryl triangular
carbaryl_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.2441, max = 2.9293, mode = (-3.2441 + 2.9293)/2))*3)/1000
# chlorpyrifos triangular
chlorpyrifos_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.4647, max = 1.0538, mode = (-3.4647 + 1.0538)/2))*3)/1000
# clothianidin triangular
clothianidin_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -2.6705, max = -1.0526, mode = (-2.6705 + -1.0526)/2))*3)/1000
# imidacloprid normal
imidacloprid_tox_quantilesd <- ((10^qnorm(grab_quantiles, -2.0408, 1.1062))*3)/1000
# thiamethoxam logistic -0.0566, 0.3559, log10
thiamethoxam_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.4085, max = -0.3356, mode = (-3.4085 + -0.3356)/2))*3)/1000

tox_quantileso <- cbind(grab_quantiles, bifenthrin_tox_quantileso, carbaryl_tox_quantileso, chlorpyrifos_tox_quantileso,
                        clothianidin_tox_quantileso, imidacloprid_tox_quantileso, thiamethoxam_tox_quantileso)

tox_quantileso<-as.data.frame(tox_quantileso)

dr<-gather(tox_quantileso, "Compound","Dose", 2:7)
colnames(dr)[1]<-"Mortality"
dr$Compound<- str_to_title(sub("\\_.*", "", dr$Compound))

dr<-merge(x = dr, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)


ec90s<-dr[dr$Mortality == 0.90,]
ec10s<-dr[dr$Mortality == 0.10,]

slope<-log10(81)/(log10(ec90s$Dose/ec10s$Dose)) #calculates slope from the hill equation 
slopedf<-as.data.frame(cbind(ec90s$Compound,slope))
names(slopedf)<-c("Compound","Slope")
slopedf$Slope<-as.numeric(slopedf$Slope)


dr<-merge(dr,slopedf,by="Compound")

dr <- dr %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Contact_LD50_ug_bee)^Slope)) #again using hill equation


#look at DR curves
ggplot(dr, aes(x = (Dose), y = 1-phat)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")



evaluate_by_compound<-function(x){
  ind_compound<-x
 # ind_compound<-exp_dose_output[[2]]
  
  ind_compound<- Map(cbind, ind_compound, index = seq_along(ind_compound))
  ind_compound<-as.data.frame(do.call(rbind, ind_compound))
  ind_compound$Compound<-str_to_title(ind_compound$Compound)
  
  
  curve<-dr[dr$Compound %in% ind_compound$Compound,]
  
  
  ###contact
  contact<-ind_compound[ind_compound$type == "Contact",]
  colnames(contact)[6]<-"Dosen"
  
  
  test = curve %>% left_join(contact , by='Compound') %>%
    mutate(dif = abs(Dosen - Dose)) %>%
    group_by(Day) %>% filter(dif == min(dif))
  
  
  daily_prob<-test[order(test$index,test$Day),,drop=FALSE]
  daily_prob$Survival<-1-daily_prob$Mortality
  daily_prob$Survival <- ifelse(daily_prob$Survival >= 0.999, 1,daily_prob$Survival)
  
  
  daily_prob_plot<-  ggplot(daily_prob, aes(x = (Day), y = Survival)) + 
    geom_line(aes(color = index), size=1.2) +
    geom_vline(xintercept = 100,col="darkgreen")+
    geom_vline(xintercept = 121,col="darkblue")+
    geom_vline(xintercept = 151,col="darkred")+
    
    geom_segment(aes(x=90,xend=110,y=0.60,yend=0.60), alpha = 0.5)+
    geom_segment(aes(x=111,xend=131,y=0.60,yend=0.60), alpha = 0.5)+
    geom_segment(aes(x=141,xend=161,y=0.60,yend=0.60), alpha = 0.5)+
    
    
    geom_text(aes(x=103, label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=11)) +
    geom_text(aes(x=124, label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=11))+
    geom_text(aes(x=154, label="Queen Ends Foraging", y=0.31), colour="darkred", angle=90, text=element_text(size=11))+
    
    xlab("Time (Day)") +
    ylab("Probability of Survival") +
    # scale_x_continuous(trans=scales::log_trans(),
    #                    labels = scales::format_format(digits=3))+
    theme_bw() +
    theme(legend.position = "none")
  daily_prob_plot
  
  cumulative_survival<- apply(daily_prob[1:365,14], 2, prod) #but need to actually cut off at end of foraging! i.e., after certain day mortality to foundress queen not important
  
}
