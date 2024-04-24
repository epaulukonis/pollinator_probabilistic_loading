### Foraging Landscape

### 07 Estimating probability of survival




#param <-read.csv(paste0(pest_dir, "/HC_params.csv"))
tox<-read.csv(paste0(pest_dir, "/BeeTox.csv"))
tox<-tox[,1:3]
tox$Compound<-str_to_title(tox$Compound)

#get k-valeus for deacy rates
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)
apprates<-apprates %>% distinct(k_values, .keep_all = T)


library(drc)
library(ordinal)
library(EnvStats)
library(ExtDist)
library(ggpmisc) 
library(stats)


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

tox$Compound<-toupper(tox$Compound)
eecs<-merge(x = eecs, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)
  
  
  eeco<-eecs[eecs$type=="Oral",]
  eecd<-eecs[eecs$type=="Contact",]
  

eecs_simo<-eeco %>% group_by(Compound,type) %>%  
  mutate(Dose= map2(min*15, Oral_LD50_ug_bee*25, seq, length.out = 1000)) %>%
  unnest(cols = Dose)


eecs_simd<-eecd %>% group_by(Compound,type) %>%  
  mutate(Dose= map2(min*15, Contact_LD50_ug_bee*25, seq, length.out = 1000)) %>%
  unnest(cols = Dose)

eecs_simo$Compound<-str_to_title(eecs_simo$Compound)
eecs_simd$Compound<-str_to_title(eecs_simd$Compound)


slope<-3.4 #from white paper, honeybee
dro <- eecs_simo %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Oral_LD50_ug_bee)^slope)) #again using hill equation

slope<-3.9 #from white paper, honeybee
drd <- eecs_simd %>% group_by(Compound) %>% mutate(phat = 1/(1+(Dose/Contact_LD50_ug_bee)^slope)) #again using hill equation


# quantile(dro$phat, c(0.95,0.50,0.05))
# 
# droq<-dro %>% group_by(Dose) %>% summarize(
#           quant5 = quantile(phat, probs = 0.95),
#           quant50 = quantile(phat, probs = 0.50),
#           quant95 = quantile(phat, probs = 0.05))




drd<-drd[drd$phat < 0.9999 & drd$phat > 0.001, ]
dro<-dro[dro$phat < 0.9999 & dro$phat > 0.001, ]


#look at DR curves
drop<-ggplot(dro, aes(x = (Dose), y = 1-phat)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  ggtitle("Oral")+
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.8))
drop

#look at DR curves
drdp<-ggplot(drd, aes(x = (Dose), y = 1-phat)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), linetype=2, size=1.2) +
  geom_point(aes(x=Contact_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  ggtitle("Contact")+
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.8))

dr<-plot_grid(
  drop,
  drdp,
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=2, rel_widths = c(4,4))
dr

dr<-rbind(dro,drd)







#### Evaluate by scenario ----

# sim<-exp_dose_output[[1]]
# sim<-split(sim, sim$Compound)



get_daily_and_event_prob<-function(compounds){
#compounds<-sim[[1]]
  ind_scene<-compounds
 # ind_scene<-merge(x = ind_scene, y = apprates[ , c("Compound",  "k_values")], by = "Compound", all.x=TRUE)
  
  #get DR curve 
  ind_scene$Compound<-str_to_title(ind_scene$Compound)
  curve<-dr[dr$Compound %in% ind_scene$Compound,]
  
  #randomly select emergence day from plausible days
  emergence<-sample(90:110,1) #what day does she emerge? 
  
  #first, set any plausible doses on days associated with certain precipitation or temperature thresholds to 0
  ind_scene[ind_scene$Precip >= 0.4, c("Dose","Dosesum","Conc")] <-0 #if precip > 0.4, no foraging
  ind_scene[ind_scene$TempC <=6, c("Dose","Dosesum","Conc")] <-0 #if temperature less than 6 degrees C, no foraging
  ind_scene[ind_scene$Day >=283, c("Dose","Dosesum","Conc")] <-0 #if day is greater than 283, all dead
  ind_scene[ind_scene$Day <=emergence, c("Dose","Dosesum","Conc")] <-0 #if day is less than emergence day, no exposure
  
  initation<-sample((emergence+4):(emergence+25),1) #what hypothetical day does she initiate?
  workerbrood<-sample((initation+22-7):(initation+22+7),1) #what hypothetical day does the first brood emerge? Banks gives 22 days, we add a 1-week SD
  endqueenforaging<-workerbrood
  queenreproduction<-sample((endqueenforaging+18-4):(endqueenforaging+18+4),1) #what hypothetical day does the first set of gynes/drones emerge? Banks gives 18 days, +4
  
 
  
  #let's assign a daily pollen and nectar storage that involves the contaminated residues for the queen starting at emergence
  daily_proboral <-ind_scene[ind_scene$Type == "Oral",]
  daily_proboral$store_res_ug_g<-NA
  
  pollen<-daily_proboral[daily_proboral$Media=="Pollen",]
  nectar<-daily_proboral[daily_proboral$Media=="Nectar",]
  
  
  ### Queen contribution to stored residues
  #assign the residues stored that she contributes to 
  # nectar$store_res_ug_g[which(nectar$Day >= initation & nectar$Day <= endqueenforaging)] <- 0.6*nectar$Conc 
  # pollen$store_res_ug_g[which(pollen$Day >= initation & pollen$Day <= endqueenforaging)] <- 0.4*pollen$Conc
  
  nectar$store_res_ug_g<-ifelse(nectar$Day >= initation & nectar$Day <= endqueenforaging, 0.6*nectar$Conc , nectar$store_res_ug_g)
  pollen$store_res_ug_g<-ifelse(pollen$Day >= initation & pollen$Day <= endqueenforaging, 0.4*pollen$Conc , pollen$store_res_ug_g)
  
  
  #any remaining values are 0 as her contribution
  nectar$store_res_ug_g[is.na(nectar$store_res_ug_g)] <- 0
  pollen$store_res_ug_g[is.na(pollen$store_res_ug_g)] <- 0
  
  
  #this function calculates the residues in the individual wax pots and pollen balls starting at collection day, using an exponential decay function 
  get_queen_stores<-function(med){
    med$valuf<-0
    for(row in 2:nrow(med)){
      med[row,ncol(med)]<-ifelse(med[row-1,2]>=initation, med[row,ncol(med)-1] + med[row-1,ncol(med)], 0) 
    }
    med$dayn<-ifelse(med$Day >= initation,med$Day-initation,0)
    med$residues_stored<-med$valuf * exp(-(med$k_values*med$dayn))
    med
    
  }
  
  pollen<-get_queen_stores(pollen)
  nectar<-get_queen_stores(nectar)
  

  ### Colony contribution to stored residues
  # #baseline natural worker numbers; we add 22 to simulate the last hatching of eggs 
  # nectar$nworkers<-ifelse(nectar$Day>=workerbrood & nectar$Day <= queenreproduction+22,((nectar$Day-workerbrood)*8.5+144)/2-((nectar$Day-workerbrood)*8.5+144)/2*0.05,0) # calculate the number of workers foraging on a given day based on banks; 144 is number of workers emerged in nest establishment; 0.05 is death rate
  # pollen$nworkers<-ifelse(pollen$Day>=workerbrood & pollen$Day <= queenreproduction+22,((pollen$Day-workerbrood)*8.5+144)/2-((pollen$Day-workerbrood)*8.5+144)/2*0.05,0) # calculate the number of workers foraging on a given day based on banks; 144 is number of workers emerged in nest establishment; 0.05 is death rate
  # 
  # #add in function to estimate n workers after she stops laying workers/they stop hatching, with natural mortality
  # get_nworkers<-function(med){
  #   for(row in 2:nrow(med)){
  #     med[row,ncol(med)]<-ifelse(med[row,2] > queenreproduction+22 & med[row,2]<=283, med[row-1,ncol(med)]-med[row-1,ncol(med)]*0.05, med[row,ncol(med)]) 
  #   }
  #   med
  # }
  # nectar<-get_nworkers(nectar)
  # pollen<-get_nworkers(pollen)
  # 
  # #get concentration that the workers take up to estimate reduction in nworkers by exposure
  # nectar$nworkersdose<-0.35*nectar$Conc*nectar$nworkers *0.5
  # pollen$nworkersdose<-0.25*pollen$Conc*pollen$nworkers *0.5
  # 
  # curve_sub<-curve[,c(2,8,9)]
  # colnames(curve_sub)[3]<-"Survival"
  # colnames(curve_sub)[1]<-"Type"
  # 
  # 
  # #let's match the survival estimate
  # require( data.table )
  # workers <- data.table(nectar)
  # drsim <- data.table(curve_sub[curve_sub$Type == "Oral",])
  # setkey(workers)
  # setkey(drsim)
  # drsim$nworkersdose<-drsim$Dose
  # drsim$workersurvival<-drsim$Survival
  # drsim<-drsim[,4:5]
  # nectar<-drsim[workers , on="nworkersdose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  # nectar<-nectar[,c(3:21,1:2)]
  # nectar$nworkers<-nectar$nworkers-((nectar$nworkers*(1-nectar$workersurvival))/2) # we assume roughly half of the workers are foraging 
  # 
  # require( data.table )
  # workers <- data.table(pollen)
  # drsim <- data.table(curve_sub[curve_sub$Type == "Oral",])
  # setkey(workers)
  # setkey(drsim)
  # drsim$nworkersdose<-drsim$Dose
  # drsim$workersurvival<-drsim$Survival
  # drsim<-drsim[,4:5]
  # pollen<-drsim[workers , on="nworkersdose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  # pollen<-pollen[,c(3:21,1:2)]
  # pollen$nworkers<-pollen$nworkers-((pollen$nworkers*(1-pollen$workersurvival))/2) # we assume roughly half of the workers are foraging 
  # 
  # nectar<- nectar%>% mutate(worker_res_ug_g = ifelse(Day>=workerbrood & Day<= 283, ((0.6*nworkers)*Conc)*0.01 + residues_stored, 0) ) #let's assume 1% of that nectar is contaminated; add any leftover concentration brought in by queen
  # pollen<- pollen%>% mutate(worker_res_ug_g = ifelse(Day>=workerbrood & Day<= 283, ((0.4*nworkers)*Conc)*0.01 + residues_stored, 0) ) #let's assume 1% of that pollen is contaminated; add any leftover concentration brought in by queen
  # 
  # 
  # nectar<-as.data.frame(nectar)
  # pollen<-as.data.frame(pollen)
  # 
  # #add function to add stores over time and decay
  # get_colony_stores<-function(med){
  #   med$valuc<-0
  # 
  #   for(row in 2:nrow(med)){
  #     med[row,ncol(med)]<-ifelse(med[row-1,2]>=workerbrood, med[row,(ncol(med)-1)] + med[row-1,ncol(med)], 0)  
  #   }
  #   med$dayn<-ifelse(med$Day >= workerbrood & med$Day <=283,med$Day-workerbrood,0)
  #   med$worker_res_ug_g<-med$valuc * exp(-(med$k_values*med$dayn))
  #   
  #   med$worker_res_ug_g<-ifelse(med$Day >= 283,0,med$worker_res_ug_g)
  #   med
  #   
  # }
  # 
  # nectar<-get_colony_stores(nectar)
  # pollen<-get_colony_stores(pollen)
  
  
  #if raining/or too cold, she eats from her store; else she is exposed while foraging
  pollen$Dose<-ifelse(pollen$Precip >= 0.4, pollen$residues_stored*0.0485, pollen$Dose )
  pollen$Dose<-ifelse(pollen$TempC <=6,  pollen$residues_stored*0.0485,pollen$Dose )
  pollen$Dosesum<-ifelse(pollen$Precip >= 0.4, pollen$residues_stored*0.0485, pollen$Dosesum )
  pollen$Dosesum<-ifelse(pollen$TempC <=6,  pollen$residues_stored*0.0485,pollen$Dosesum )
  
  
  nectar$Dose<-ifelse(nectar$Precip >= 0.4, nectar$residues_stored*0.7565, nectar$Dose ) 
  nectar$Dose<-ifelse(nectar$TempC <=6,  nectar$residues_stored*0.7565, nectar$Dose )
  nectar$Dosesum<-ifelse(nectar$Precip >= 0.4, nectar$residues_stored*0.7565, nectar$Dosesum ) 
  nectar$Dosesum<-ifelse(nectar$TempC <=6,  nectar$residues_stored*0.7565, nectar$Dosesum )
  
  
  
  #if raining/or too cold, she eats from her store; else she is exposed while foraging
  # pollen$Dose_while_forage<-ifelse(pollen$Precip >= 0.4, pollen$residues_stored*0.0485, pollen$Dose )
  # pollen$Dose_while_forage<-ifelse(pollen$TempC <=6,  pollen$residues_stored*0.0485,pollen$Dose )
  # nectar$Dose_while_forage<-ifelse(nectar$Precip >= 0.4, nectar$residues_stored*0.7565, nectar$Dose ) 
  # nectar$Dose_while_forage<-ifelse(nectar$TempC <=6,  nectar$residues_stored*0.7565, nectar$Dose )
  # 
  # #if it's after her in-nest activities, set to NA
  # pollen$Dose_while_forage<-ifelse(pollen$Day >= endqueenforaging,NA, pollen$Dose_while_forage)
  # nectar$Dose_while_forage<-ifelse(nectar$Day >= endqueenforaging,NA, nectar$Dose_while_forage)
  # 
  # 
  # #if after her ending of foraging, she eats from stores collected from workers
  # nectar$Dose_in_hive<-nectar$worker_res_ug_g*0.7565 # she eats approximately 100% of her daily stores in contaminated nectar in-hive
  # pollen$Dose_in_hive<-pollen$worker_res_ug_g*0.0485 # she eats approximately 100% of her daily stores in contaminated pollen in-hive
  # 
  # #if before in-nest, set to NA
  # pollen$Dose_in_hive<-ifelse(pollen$Day < endqueenforaging, NA,pollen$Dose_in_hive)
  # nectar$Dose_in_hive<-ifelse(nectar$Day < endqueenforaging, NA,nectar$Dose_in_hive)
  # 
  # pollen$Dose<-coalesce(pollen$Dose_while_forage, pollen$Dose_in_hive)
  # nectar$Dose<-coalesce(nectar$Dose_while_forage, nectar$Dose_in_hive)
  
  
  daily_proboral<-rbind(nectar,pollen)
  daily_proboral<-daily_proboral[,-c(16:19)]
  #daily_proboral<-daily_proboral[,-c(15:25)]
  

  
  #curve_sub<-curve[,c(6,9:10)] 
  curve_sub<-curve[,c(2,8,9)] 
  colnames(curve_sub)[3]<-"Survival"
  colnames(curve_sub)[2]<-"Dosesum"
  colnames(curve_sub)[1]<-"Type"
  
  #Use datatable to get the rolling nearest value for mortality; we'll use the summed contact and oral doses, which reflect the total combined pathways
  ##ORAL
  require( data.table )
  ind_scenen <- data.table(daily_proboral)
  drsim <- data.table(curve_sub[curve_sub$Type == "Oral",])
  setkey(ind_scenen)
  setkey(drsim)
  daily_proboralf<-drsim[ind_scenen , on="Dosesum", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  
  ##CONTACT
  require( data.table )
  ind_scenen <- data.table(ind_scene[ind_scene$Type == "Contact",])
  drsim <- data.table(curve_sub[curve_sub$Type == "Contact",])
  setkey(ind_scenen)
  setkey(drsim)
  daily_probcontf<-drsim[ind_scenen , on="Dosesum", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  
  
  # daily_proboral$Survival<-ifelse( daily_proboral$Dose ==0 &  daily_proboral$Survival >= 0.9994333 , 1,    daily_proboral$Survival) #convert to 100% survival if dose=0
  # daily_probcont$Survival<-ifelse( daily_probcont$Dose ==0 &  daily_probcont$Survival >= 0.9994333 , 1,    daily_probcont$Survival) #convert to 100% survival if dose=0

  daily_probcont<-daily_probcontf[,-c(12)]
  daily_proboral<-daily_proboralf[,-c(12)]
  
 overallcont<-daily_probcont[daily_probcont$Media == "Soil",] #just subset one set with combined contact doses
 overalloral<-daily_proboral[daily_proboral$Media == "Pollen",] #just subset one set with combined contact doses
  
  
  get_survivals<-as.data.frame(rbind(overallcont,overalloral))
  get_survivals<-get_survivals[order(get_survivals$Day),]
  get_survivals<- get_survivals[c("Type","Survival","Day")]
  get_survivals<-spread(get_survivals,key="Type",value="Survival")
  get_survivals<-get_survivals[,-(1)]

  cumulativesurvival<-get_survivals %>%   
    rowwise() %>%
    do(data.frame(., Prod = prod(unlist(.))))


  daily_prob<-as.data.frame(cbind(1:365, cumulativesurvival$Prod, unique(daily_probcont$Compound)))
  names( daily_prob)<-c("Day","Survival","Compound")
 # daily_prob$Survival <- ifelse( daily_prob$Survival >= 0.999, 1,daily_prob$Survival)

  daily_prob$Survival<-as.numeric(daily_prob$Survival)
  daily_prob$Day<-as.numeric(daily_prob$Day)
  
  daily_prob[daily_prob$Day >=283, c("Survival")] <-1 #if day is greater than 283, all dead
  daily_prob[daily_prob$Day <=emergence, c("Survival")] <-1  #if day is less than emergence day, no exposure

  #old code for when we did queen reproduction
  #daily_prob$Survival<-ifelse(daily_prob$Day > endqueenforaging & daily_prob$Day < queenreproduction, daily_proboral_in_nest$Survival,daily_prob$Survival  )
  

  
  #What is the survival between events?
  #### Probability of nest initiation
  prob_nest_initation<- apply(as.matrix(daily_prob[emergence:initation,2]), 2, prod) 
  
  #### Probability of queen making it throughout nest establishment and larvae development 
  prob_nest_establish<- apply(as.matrix(daily_prob[initation:endqueenforaging,2]), 2, prod) 
  
  #### Probability of queen dying before she can fully care for and produce the first workers
  prob_nest_queen_produces_workers<- apply(as.matrix(daily_prob[endqueenforaging-7:endqueenforaging+7,2]), 2, prod)
  
  #### Probability of queen dying before she can produce the first gynes/males
  #prob_nest_queen_produces_gynes<- apply(as.matrix(daily_prob[endqueenforaging:queenreproduction,2]), 2, prod)
  
  survival_to<-c("Initation","Establishment","Workers")
  probs<-c(prob_nest_initation, prob_nest_establish, prob_nest_queen_produces_workers)
  Compound<-unique(daily_prob$Compound)
  probability_of_events<-data.frame(survival_to,probs, Compound)
  
  
  
  #what are the results during the survival periods?
  # survival_to<-c("Initation","Establishment","Gyne")
  # probs<-c(prob_nest_initation, prob_nest_establish, prob_nest_queen_produces_gynes)
  # Compound<-unique(daily_prob$Compound)
  # probability_of_events<-data.frame(survival_to,probs, Compound)
  
  
  #### Now here, we get individual pathways survival instead of the overall contact/oral survival for the cumulative
  #curve_sub<-curve[,c(6,9:10)] 
  curve_sub<-curve[,c(2,8,9)] 
  colnames(curve_sub)[3]<-"Survival"
  colnames(curve_sub)[2]<-"Dose"
  colnames(curve_sub)[1]<-"Type"
  
  #Use datatable to get the rolling nearest value for mortality; we'll use the summed contact and oral doses, which reflect the total combined pathways
  ##ORAL
  require( data.table )
  ind_scenen <- data.table(daily_proboral)
  drsim <- data.table(curve_sub[curve_sub$Type == "Oral",])
  setkey(ind_scenen)
  setkey(drsim)
  daily_proboralf<-drsim[ind_scenen , on="Dose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  
  ##CONTACT
  require( data.table )
  ind_scenen <- data.table(daily_probcont)
  drsim <- data.table(curve_sub[curve_sub$Type == "Contact",])
  setkey(ind_scenen)
  setkey(drsim)
  daily_probcontf<-drsim[ind_scenen , on="Dose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  
  #what are the top pathways?
  
  cont<-daily_probcontf
  oral<-daily_proboralf
  
  
  pathwayscontd<-cont %>% group_by(Media) %>% summarise(across(c(Dose,Survival,Conc), list(mean)))
  pathwaysorald<-oral %>% group_by(Media) %>% summarise(across(c(Dose,Survival,Conc), list(mean)))
  
  pathwayscontmin<-cont %>% group_by(Media) %>% summarise(across(c(Dose,Survival,Conc), list(min)))
  pathwaysoralmin<-oral %>% group_by(Media) %>% summarise(across(c(Dose,Survival,Conc), list(min)))
  
  pathwayscontmax<-cont %>% group_by(Media) %>% summarise(across(c(Dose,Survival,Conc), list(max)))
  pathwaysoralmax<-oral %>% group_by(Media) %>% summarise(across(c(Dose,Survival,Conc), list(max)))
  
  
  # pathwayscontd<-  pathwayscontd[,c( 1,3,4,10)]
  # pathwaysorald<-  pathwaysorald[,c( 1,3,4,10)]
  # 
  # pathwayscontmin<-  pathwayscontmin[,c( 1,3,4,10)]
  # pathwaysoralmin<-  pathwaysoralmin[,c( 1,3,4,10)]
  # 
  # pathwayscontmax<-  pathwayscontmax[,c( 1,3,4,10)]
  # pathwaysoralmax<-  pathwaysoralmax[,c( 1,3,4,10)]
  
  names(pathwayscontd)[2:4]<-c("Dose","Survival","Conc")
  names(pathwaysorald)[2:4]<-c("Dose","Survival","Conc")
  
  names(pathwayscontmax)[2:4]<-c("Dose","Survival","Conc")
  names(pathwaysoralmax)[2:4]<-c("Dose","Survival","Conc")
  
  names(pathwayscontmin)[2:4]<-c("Dose","Survival","Conc")
  names(pathwaysoralmin)[2:4]<-c("Dose","Survival","Conc")
  
  
  pathwaysoral<-gather(pathwaysorald, "Type","Value",2:4)
  pathwayscont<-gather(pathwayscontd, "Type","Value",2:4)
  
  pathwaysoralmax<-gather(pathwaysoralmax, "Type","Value",2:4)
  pathwayscontmax<-gather(pathwayscontmax, "Type","Value",2:4)
  
  pathwaysoralmin<-gather(pathwaysoralmin, "Type","Value",2:4)
  pathwayscontmin<-gather(pathwayscontmin, "Type","Value",2:4)
  
  
  pathways<-rbind(pathwaysoral,pathwayscont)
  pathways$Result<-"Mean"
  pathwaysmax<-rbind(pathwaysoralmax,pathwayscontmax)
  pathwaysmax$Result<-"Maximum"
  pathwaysmin<-rbind(pathwaysoralmin,pathwayscontmin)
  pathwaysmin$Result<-"Min"
  
  pathways<-rbind(pathways,pathwaysmax,pathwaysmin)
  # pathways$Type<-NA
  # pathways[1:4,4]<-"Dose"
  # pathways[5:8,4]<-"Survival"
  # 
  # pathways$mean<-ifelse(pathways$mean >=0.999, 1,pathways$mean)
  

  #what are the actualy daily doses?
  contactf<-daily_probcontf[,c(2,5,7,10)]
  oralf<-daily_proboralf[,c(2,5,7,10)]
  finaldoses<-rbind(contactf,oralf)
  

  return(list(daily_prob, probability_of_events, pathways, finaldoses))
  
} #function to estimate the individual events and daily survival probability

evaluate_by_scenario<-function(x){
  sim<-x
  sim<-split(sim, sim$Compound)

  output<-lapply(sim, get_daily_and_event_prob)
  
#here we pull out the individual daily survival and get the product to extract a cumulative daily survival 
 daily_prob_compound<- lapply(output,`[[`, 1)
 probability_of_events_data<-lapply(output,`[[`, 2)
 pathways<-lapply(output,`[[`, 3)
 doses<-lapply(output,`[[`, 4)
 
 
 ## Daily prob
 survivaldf = lapply(daily_prob_compound, "[", , "Survival")
 survivaldf<-as.data.frame(do.call(cbind,survivaldf))
 names(survivaldf)<-1:ncol(survivaldf)

 cumulativesurvival<-survivaldf %>%   
                      rowwise() %>%
                      do(data.frame(., Prod = prod(unlist(.))))
  
  
  cumulativesurvival<-as.data.frame(cbind(1:365, cumulativesurvival$Prod))
  names(cumulativesurvival)<-c("Day","Survival")
  cumulativesurvival$Survival <- ifelse(cumulativesurvival$Survival >= 0.999999, 1,cumulativesurvival$Survival)
  
  #Overall probability of events
  probability_of_events<-do.call(rbind, probability_of_events_data)
  probability_of_events$scenario<-rownames(probability_of_events)
  probability_of_events<-probability_of_events %>% group_by(survival_to) %>% summarize(event_cum = prod(probs))
  
  
  ##pathways
  pathways<-do.call(rbind, pathways)
  pathways$compound<-row.names(pathways)
  
  ##doses
  doses<-dplyr::bind_rows(doses, .id = "Compound")
  

 
  return(list(cumulativesurvival, probability_of_events, pathways, doses))
  
}



start.time<-Sys.time()
final_results<-lapply(exp_dose_output, evaluate_by_scenario)
end.time<-Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

saveRDS(set1, file=paste0(root_data_out, "/results/set1.RData"))
set1<-readRDS(paste0(root_data_out, "/results/set1.RData"))


start.time<-Sys.time()
set2<-lapply(exp_dose_output, evaluate_by_scenario)
end.time<-Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

saveRDS(set2, file=paste0(root_data_out, "/results/set2.RData"))
#readRDS("set1.RData")


# get_only_legend <- function(plot) { 
#   plot_table <- ggplot_gtable(ggplot_build(plot)) 
#   legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
#   legend <- plot_table$grobs[[legend_plot]] 
#   return(legend) 
# } 

options(scipen=0)

##### Figure 3.3A - cumulative daily mortality ----

dailyprob <- lapply(final_results,`[[`, 1)
dailyprob<- Map(cbind, dailyprob, index = seq_along(dailyprob))
dailyprob<-do.call(rbind,dailyprob)
#dailyprob<-dailyprob %>% group_by(index) %>% mutate(Survival = if_else(Day > 140, 1, Survival))
#write.csv(dailyprob,paste0(root_data_out,"/dailprob_example.csv"))

# head(dailyprob)
# 
# daily_prob_plot<-  ggplot(dailyprob, aes(x = (Day), y = Survival-1, group=index)) +
#   geom_line(size=1, alpha=0.2) +
#   geom_vline(xintercept = 100,col="darkgreen")+
#   geom_vline(xintercept = 121,col="darkblue")+
#   geom_vline(xintercept = 143,col="darkorange")+
#   geom_vline(xintercept = 161,col="darkred")+
# 
#   scale_y_continuous(limits = c(0, 1))+
# 
#   # geom_segment(aes(x=90,xend=110,y=0.60,yend=0.60), alpha = 0.5)+
#   # geom_segment(aes(x=111,xend=131,y=0.60,yend=0.60), alpha = 0.5)+
#   # geom_segment(aes(x=141,xend=161,y=0.60,yend=0.60), alpha = 0.5)+
# 
# 
#   geom_text(aes(x=103, label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=11)) +
#   geom_text(aes(x=124, label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=11))+
#   geom_text(aes(x=146, label="Queen Ends Foraging", y=0.31), colour="darkorange", angle=90, text=element_text(size=11))+
#   geom_text(aes(x=164, label="First Gynes/Drones Emerge", y=0.31), colour="darkred", angle=90, text=element_text(size=11))+
# 
#   xlab("Time (Day)") +
#   ylab("Daily Mortality") +
#   # scale_x_continuous(trans=scales::log_trans(),
#   #                    labels = scales::format_format(digits=3))+
#   theme_bw() +
#   theme(legend.position = "none")
# daily_prob_plot


dailyprob$Mortality<-1-dailyprob$Survival
mean_scenarios<-dailyprob%>%
  group_by(Day) %>%
  summarize(quant1 = quantile(Mortality, probs = 0.99), 
            quant5 = quantile(Mortality, probs = 0.95),
            quant16 = quantile(Mortality, probs = 0.84),
            quant50 = quantile(Mortality, probs = 0.50),
            quant84 = quantile(Mortality, probs = 0.16),
            quant95 = quantile(Mortality, probs = 0.05),
            quant99 = quantile(Mortality, probs = 0.01),)



mean_scenarios$Date<-as.Date(1:365, origin = '2014-01-01')
mean_scenarios<-mean_scenarios[mean_scenarios$Day >= 95 & mean_scenarios$Day <= 150,]

daily_prob_plot<-  ggplot(mean_scenarios, aes(x = as.Date(Date))) +
  annotate("rect", xmin=as.Date('2014-04-01'), xmax=as.Date('2014-04-20'), ymin=0, ymax=0.5,alpha=0.2, fill="darkgreen") +
  annotate("rect", xmin=as.Date('2014-04-21'), xmax=as.Date('2014-05-11'), ymin=0, ymax=0.5,alpha=0.2, fill="darkblue") +
  annotate("rect", xmin=as.Date('2014-05-16'), xmax=as.Date('2014-05-30'), ymin=0, ymax=0.5,alpha=0.2, fill="plum4") +
  
  geom_ribbon(aes(ymin = 0, ymax = quant5, color="95th percentile"), fill = "brown3", alpha=0.8) +
  geom_ribbon(aes(ymin = 0, ymax = quant16, color="84th percentile"), fill = "orange1", alpha=0.8) +
  geom_ribbon(aes(ymin = 0, ymax = quant50, color="50th percentile"), fill = "grey", alpha=0.8) +
  geom_ribbon(aes(ymin = 0, ymax = quant84, color="16th percentile"), fill = "goldenrod", alpha=0.8) +
  #geom_line(aes(y = quant50), size=1, alpha=1,color="black") +
  guides(color = guide_legend(override.aes = list(color = "white"))) +
  
 geom_vline(xintercept = as.Date('2014-04-10'),col="darkgreen")+
 geom_vline(xintercept = as.Date('2014-05-01'),col="darkblue")+
 geom_vline(xintercept = as.Date('2014-05-23'),col="plum4")+
 #geom_vline(xintercept = as.Date('2014-06-10'),col="darkred")+

  
  scale_y_continuous(limits = c(0, 0.50))+
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day",
               date_labels = "%b %d")+
  
  geom_text(aes(x=as.Date('2014-04-11'), label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=11)) +
  geom_text(aes(x=as.Date('2014-05-02'), label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=11))+
  geom_text(aes(x=as.Date('2014-05-24'), label="Queen Ends Foraging", y=0.25), colour="plum4", angle=90, text=element_text(size=11))+
  #geom_text(aes(x=as.Date('2014-06-11'), label="First Gynes/Drones Laid", y=0.27), colour="darkred", angle=90, text=element_text(size=11))+
  
  xlab("") +
  ylab("Daily Mortality") +
  # scale_x_continuous(trans=scales::log_trans(),
  #                    labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(
    legend.title=element_blank(),
        axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),
    plot.margin = margin(10, 10, 10, 10))
daily_prob_plot


figa<-daily_prob_plot
#legend<-get_only_legend(daily_prob_plot)

##### Figure 3.3B - simulated queen survival ----
dailyprob <- lapply(final_results,`[[`, 1)
dailyprob<- Map(cbind, dailyprob, index = seq_along(dailyprob))
dailyprob<-do.call(rbind,dailyprob)


set.seed(7585)
###Just pesticides
indqueens<-spread(dailyprob,key="index",value="Survival")
# indqueens[1:365,ncol(indqueens):251]<-1
simulation<-matrix(runif(365*250),nrow=365,ncol=250)
output<-matrix(NA, nrow=365,ncol=250)
       #rowise and columnwise
              for(row in 1:nrow(indqueens)){
                for(col in 2:ncol(indqueens)){
                  output[row,col-1]<-   ifelse(indqueens[row,col] < simulation[row,col-1], 0, 1 )
                }
              }

       #columnwise
                 for(col in 1:ncol(output)){
                        column<-as.data.frame(output[,col])
                        names(column)<-"simu"
                        result<-column%>% mutate(num2 = replace(simu, cumany(simu ==0), 0))
                        output[,col]<-result[,2]
                      }

anyNA(output)


final<-as.data.frame(rowSums(output)/250)
final<-cbind(final,1:365)
names(final)<-c("Pesticide","Day")
final$Date<-as.Date(1:365, origin = '2014-01-01')
final<-final[final$Day >= 95 & final$Day <= 150,]


###Natural mortality
indqueens<-matrix(1, nrow=365,ncol=250)
indqueens[(emergence-7):initation,1:250]<-indqueens[(emergence-7):initation,1:250]*0.9985
indqueens[initation:workerbrood,1:250]<-indqueens[initation:workerbrood,1:250]*0.9985

#indqueens[(emergence-7):workerbrood,2:250]<-indqueens[(emergence-7):workerbrood,2:250]*0.99
#indqueens[1:365,ncol(indqueens):251]<-1
simulation<-matrix(runif(365*250),nrow=365,ncol=250)
output<-matrix(NA, nrow=365,ncol=250)
#rowise and columnwise
for(row in 1:nrow(indqueens)){
  for(col in 1:ncol(indqueens)){
    output[row,col]<-   ifelse(indqueens[row,col] < simulation[row,col], 0, 1 )
  }
}

#columnwise
for(col in 1:ncol(output)){
  column<-as.data.frame(output[,col])
  names(column)<-"simu"
  result<-column%>% mutate(num2 = replace(simu, cumany(simu ==0), 0))
  output[,col]<-result[,2]
}

anyNA(output)

finalnat<-as.data.frame(rowSums(output)/250)
finalnat<-cbind(finalnat,1:365)
names(finalnat)<-c("Natural","Day")
finalnat$Date<-as.Date(1:365, origin = '2014-01-01')
finalnat<-finalnat[finalnat$Day >= 95 & finalnat$Day <= 150,]

### Combined
indqueens<-spread(dailyprob,key="index",value="Survival")
indqueens[(emergence-7):initation,2:251]<-indqueens[(emergence-7):initation,2:251]*0.9985
indqueens[initation:workerbrood,2:251]<-indqueens[initation:workerbrood,2:251]*0.9985

#indqueens[(emergence-7):workerbrood,2:251]<-indqueens[(emergence-7):workerbrood,2:251]*0.99
# indqueens[1:365,ncol(indqueens):251]<-1
simulation<-matrix(runif(365*250),nrow=365,ncol=250)
output<-matrix(NA, nrow=365,ncol=250)
#rowise and columnwise
for(row in 1:nrow(indqueens)){
  for(col in 2:ncol(indqueens)){
    output[row,col-1]<-   ifelse(indqueens[row,col] < simulation[row,col-1], 0, 1 )
  }
}

#columnwise
for(col in 1:ncol(output)){
  column<-as.data.frame(output[,col])
  names(column)<-"simu"
  result<-column%>% mutate(num2 = replace(simu, cumany(simu ==0), 0))
  output[,col]<-result[,2]
}

anyNA(output)

finalall<-as.data.frame(rowSums(output)/250)
finalall<-cbind(finalall,1:365)
names(finalall)<-c("All","Day")
finalall$Date<-as.Date(1:365, origin = '2014-01-01')
finalall<-finalall[finalall$Day >= 95 & finalall$Day <= 150,]





founds<-cbind(final,finalnat,finalall)


founds<-founds[,c(2:3,1,4,7)]
str(founds)




daily_survival_plot<-  ggplot(founds, aes(x = as.Date(Date))) +
  
  annotate("rect", xmin=as.Date('2014-04-01'), xmax=as.Date('2014-04-20'), ymin=0, ymax=1,alpha=0.2, fill="darkgreen") +
  annotate("rect", xmin=as.Date('2014-04-21'), xmax=as.Date('2014-05-11'), ymin=0, ymax=1,alpha=0.2, fill="darkblue") +
  annotate("rect", xmin=as.Date('2014-05-16'), xmax=as.Date('2014-05-30'), ymin=0, ymax=1,alpha=0.2, fill="plum4") +
  
  geom_line(aes(y = All), lientype=1,size=1, alpha=1,color="black") +
  geom_line(aes(y = Natural), linetype=2,size=1, alpha=1,color="black") +
  geom_line(aes(y = Pesticide), linetype=3,size=1, alpha=1,color="black") +
  
  geom_vline(xintercept = as.Date('2014-04-10'),col="darkgreen")+
  geom_vline(xintercept = as.Date('2014-05-01'),col="darkblue")+
  geom_vline(xintercept = as.Date('2014-05-23'),col="plum4")+
  #geom_vline(xintercept = as.Date('2014-06-10'),col="darkred")+
  scale_x_date(date_breaks = "1 weeks",
               date_labels = "%b %d")+
  scale_y_continuous(limits = c(0,1))+
  
  
  geom_text(aes(x=as.Date('2014-06-01'), label="All", y=0.24), colour="black", angle=0, text=element_text(size=12)) +
  geom_text(aes(x=as.Date('2014-06-01'), label="Natural", y=0.28), colour="black", angle=0, text=element_text(size=12))+
  geom_text(aes(x=as.Date('2014-06-01'), label="Pesticide", y=0.83), colour="black", angle=0, text=element_text(size=12))+
  #
  
  
  
  geom_text(aes(x=as.Date('2014-04-12'), label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=16)) +
  geom_text(aes(x=as.Date('2014-05-03'), label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=16))+
  geom_text(aes(x=as.Date('2014-05-25'), label="Queen Ends Foraging", y=0.25), colour="plum4", angle=90, text=element_text(size=16))+
 # geom_text(aes(x=as.Date('2014-06-12'), label="First Gynes/Drones Laid", y=0.728), colour="darkred", angle=90, text=element_text(size=11))+

  xlab("") +
  ylab("Proportion of Foundresses Surviving") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = 1.5, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = 1.5, face = "plain"),
        plot.margin = margin(10, 10, 10, 10))
daily_survival_plot
figb<-daily_survival_plot






##### Figure 3.3C - simulated queen survival with natural forcing variables ----
# dailyprob <- lapply(final_results,`[[`, 1)
# dailyprob<- Map(cbind, dailyprob, index = seq_along(dailyprob))
# dailyprob<-do.call(rbind,dailyprob)
# 
# 
# 
# daily_survival_plot<-  ggplot(final, aes(x = as.Date(Date))) +
#   
#   annotate("rect", xmin=as.Date('2014-04-01'), xmax=as.Date('2014-04-20'), ymin=0, ymax=1,alpha=0.2, fill="darkgreen") +
#   annotate("rect", xmin=as.Date('2014-04-21'), xmax=as.Date('2014-05-11'), ymin=0, ymax=1,alpha=0.2, fill="darkblue") +
#   annotate("rect", xmin=as.Date('2014-05-16'), xmax=as.Date('2014-05-30'), ymin=0, ymax=1,alpha=0.2, fill="plum4") +
#   
#   geom_line(aes(y = Foundresses), size=1, alpha=1,color="black") +
#   geom_vline(xintercept = as.Date('2014-04-10'),col="darkgreen")+
#   geom_vline(xintercept = as.Date('2014-05-01'),col="darkblue")+
#   geom_vline(xintercept = as.Date('2014-05-23'),col="plum4")+
#   #geom_vline(xintercept = as.Date('2014-06-10'),col="darkred")+
#   scale_x_date(date_breaks = "1 weeks",
#                date_labels = "%b %d")+
#   scale_y_continuous(limits = c(0,1))+
#   
#   
#   geom_text(aes(x=as.Date('2014-04-12'), label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=11)) +
#   geom_text(aes(x=as.Date('2014-05-03'), label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=11))+
#   geom_text(aes(x=as.Date('2014-05-25'), label="Queen Ends Foraging", y=0.25), colour="plum4", angle=90, text=element_text(size=11))+
#   # geom_text(aes(x=as.Date('2014-06-12'), label="First Gynes/Drones Laid", y=0.728), colour="darkred", angle=90, text=element_text(size=11))+
#   
#   xlab("") +
#   ylab("Proportion of Foundresses Surviving: B") +
#   theme_bw() +
#   theme(legend.title=element_blank(),
#         axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),
#         axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),  
#         axis.title.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = 1.5, face = "plain"),
#         axis.title.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = 1.5, face = "plain"),
#         plot.margin = margin(10, 10, 10, 10)
#         
#         
#         )
# daily_survival_plot
# figc<-daily_survival_plot






##### Combine all for Figure 3.3----
fig3f<-plot_grid(
  figa,
  figb,
  hjust=0.5, vjust=0.5, align= "h",  label_x = 0.01, nrow=2, ncol=1, rel_widths = c(5,5,5))

# 
# fig3f<-plot_grid(fig3, legend, rel_widths = c(6,1))
# fig3f



library(grid)
library(egg)
y.grob <- textGrob("", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Date", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

grid.arrange(arrangeGrob(fig3f, bottom = x.grob))




##### Events for general results ----
events <- lapply(final_results,`[[`, 2)
events<- Map(cbind, events, index = seq_along(events))
events<-do.call(rbind,events)
names(events)

eventssum<-events %>% group_by(survival_to) %>% summarise(med = median(event_cum)) 
#eventssummax<-events %>% group_by(survival_to) %>% summarise(maximum = min(event_cum)) 


##### Table 2 ----
paths <- lapply(final_results,`[[`, 3)
paths<- Map(cbind, paths, index = seq_along(paths))
paths<-do.call(rbind,paths)
paths$compound<-gsub("\\..*","",paths$compound)
names(paths)


max<-paths[paths$Result == "Maximum",]
min<-paths[paths$Result == "Min",]
med<-paths[paths$Result == "Mean",]




med<-med %>% group_by(Media, Type, compound) %>%
  summarize(
            quant5 = quantile((Value), probs = 0.95),
            quant50 = quantile((Value), probs = 0.50),
            quant95 = quantile((Value), probs = 0.05))

min<-min %>% group_by(Media, Type, compound) %>%
  summarize(min=min(Value))


max<-max %>% group_by(Media, Type, compound) %>%
  summarize(max=max(Value))


pathways<-cbind(med, min[,4],max[,4])

write.csv(pathways, paste0(root_data_out,"/all_bombus/outputs_for_manuscript/pathways.csv"))


library(ggh4x)

# conc<-filter(pathways, Type=="Conc")
# dose<-filter(pathways, Type=="Dose")


scales_y <- list(
  `Conc` =   scale_y_continuous(trans=scales::log_trans(), 
                                labels = scales::format_format(digits=3),expand=c(0.05,0)),
  
  `Dose` =   scale_y_continuous(trans=scales::log_trans(),
                                labels = scales::format_format(digits=3),expand=c(0.05,0)),
  
  `Survival` = scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand=c(0.05,0))
)


pathways<-gather(pathways,"Result","Value",4:8)




out<- ggplot(pathways, aes(x=compound,)) +
  
  geom_boxplot(aes(y=Value, fill=compound), show.legend = FALSE)+
  # geom_point(aes(y=min), color="lightblue", size=3,show.legend = FALSE)+
  # geom_point(aes(y=max), color="darkblue", size=3,show.legend = FALSE)+
  # geom_point(aes(y=quant5), color="brown3", size=3,show.legend = FALSE)+
  # geom_point(aes(y=quant50), color="grey", size=3,show.legend = FALSE)+
  # geom_point(aes(y=quant95), color="goldenrod",size=3, show.legend = FALSE)+
 # facet_nested_wrap(~ Type + Media, scales="free",nrow=1)+
  facet_grid(cols=vars(Media), rows=vars(Type), scales='free')+
  # scale_y_continuous(trans=scales::log_trans(),
  #                    labels = scales::format_format(digits=3))+

  ggh4x::facetted_pos_scales(
    y = scales_y
  )+

  xlab("Compound") +
  ylab("") +
  theme_bw()+
  theme(legend.title=element_blank(),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(color = "grey20", size = 14, angle = 90, vjust = 0.5, hjust=1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"))
out









write.csv(testy,paste0(root_data_out,"/all_bombus/outputs_for_manuscript/pathways.csv"))
write.csv(paths_survival,paste0(root_data_out,"/all_bombus/outputs_for_manuscript/pathssurvival.csv"))

###### Fig Appendix C, C3 Weighted Doses ----

doses<- lapply(final_results,`[[`, 4)
doses<- Map(cbind, doses, index = seq_along(doses))
doses<-do.call(rbind,doses)
doses$compound<-gsub("\\..*","",doses$compound)

doses<- doses %>% group_by(Day,Compound,Media) %>% summarize(AverageDose =  mean(Dose))
test<-doses[doses$Compound == "IMIDACLOPRID" & doses$Media == "Pollen",]

doses$date<-as.Date(1:365, origin = '2014-01-01')

library(ggh4x)
doses<-doses[doses$AverageDose != 0,]

output<- ggplot(doses, aes(x=Compound)) +
 # geom_line(aes(x = Day, (AverageDose), color=Compound, group=Compound),size=1.2)+
  geom_boxplot(aes(y=AverageDose, color=Compound), fill="lightgrey", show.legend = FALSE)+
 facet_nested_wrap(~ Media, scales="free",nrow=1)+
 # facet_wrap(~Media)+
  scale_y_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  
  xlab("Compound") +
  ylab("Average Weighted Exposure Dose (ug/bee)") +
  theme_bw()+

  theme(legend.title=element_blank(),

        axis.text.x = element_text(color = "grey20", size = 14, angle = 90, vjust = 0.5, hjust=1, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"))

output










