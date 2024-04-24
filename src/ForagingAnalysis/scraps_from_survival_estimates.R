get_daily_and_event_prob<-function(compounds){
  compounds<-sim[[1]]
  ind_scene<-compounds
  ind_scene<-merge(x = ind_scene, y = apprates[ , c("Compound",  "k_values")], by = "Compound", all.x=TRUE)
  
  #get DR curve 
  ind_scene$Compound<-str_to_title(ind_scene$Compound)
  curve<-dr[dr$Compound %in% ind_scene$Compound,]
  
  #randomly select emergence day from plausible days
  emergence<-sample(90:110,1) #what day does she emerge? 
  
  #first, set any plausible doses on days associated with certain precipitation or temperature thresholds to 0
  ind_scene[ind_scene$Precip >= 0.4, c("Dose","Conc")] <-0 #if precip > 0.4, no foraging
  ind_scene[ind_scene$TempC <=6, c("Dose","Conc")] <-0 #if temperature less than 6 degrees C, no foraging
  ind_scene[ind_scene$Day >=283, c("Dose","Conc")] <-0 #if day is greater than 283, all dead
  ind_scene[ind_scene$Day <=emergence, c("Dose","Conc")] <-0 #if day is less than emergence day, no exposure
  
  initation<-sample((emergence+4):(emergence+25),1) #what hypothetical day does she initiate?
  workerbrood<-sample((initation+22-7):(initation+22+7),1) #what hypothetical day does the first brood emerge? Banks gives 22 days, we add a 1-week SD
  endqueenforaging<-workerbrood
  queenreproduction<-sample((endqueenforaging+18-4):(endqueenforaging+18+4),1) #what hypothetical day does the first set of gynes/drones emerge? Banks gives 18 days, +4
  
  
  
  #let's assign a daily pollen and nectar storage that involves the contaminated residues for the queen starting at emergence
  daily_proboral <-ind_scene[ind_scene$Type == "Oral",]
  daily_proboral$store_res_ug_g<-NA
  
  
  ### Queen contribution to stored residues
  #assign the residues stored that she contributes to 
  daily_proboral$store_res_ug_g[which(daily_proboral$Day >= initation & daily_proboral$Day <= endqueenforaging & daily_proboral$Media == "Nectar")] = 0.6* (daily_proboral$Day-initation)*daily_proboral$Conc 
  daily_proboral$store_res_ug_g[which(daily_proboral$Day >= initation & daily_proboral$Day <= endqueenforaging & daily_proboral$Media == "Pollen")] = 0.4* (daily_proboral$Day-initation)*daily_proboral$Conc
  
  #any remaining values are 0 as her contribution
  daily_proboral$store_res_ug_g[is.na(daily_proboral$store_res_ug_g)] <- 0
  
  
  pollen<-daily_proboral[daily_proboral$Media=="Pollen",]
  nectar<-daily_proboral[daily_proboral$Media=="Nectar",]
  
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
  #baseline natural worker numbers; we add 22 to simulate the last hatching of eggs 
  nectar$nworkers<-ifelse(nectar$Day>=workerbrood & nectar$Day <= queenreproduction+22,((nectar$Day-workerbrood)*8.5+144)/2-((nectar$Day-workerbrood)*8.5+144)/2*0.05,0) # calculate the number of workers foraging on a given day based on banks; 144 is number of workers emerged in nest establishment; 0.05 is death rate
  pollen$nworkers<-ifelse(pollen$Day>=workerbrood & pollen$Day <= queenreproduction+22,((pollen$Day-workerbrood)*8.5+144)/2-((pollen$Day-workerbrood)*8.5+144)/2*0.05,0) # calculate the number of workers foraging on a given day based on banks; 144 is number of workers emerged in nest establishment; 0.05 is death rate
  
  
  #add in function to estimate n workers after she stops laying workers/they stop hatching, with natural mortality
  get_nworkers<-function(med){
    for(row in 2:nrow(med)){
      med[row,ncol(med)]<-ifelse(med[row,2] > queenreproduction+22 & med[row,2]<=283, med[row-1,ncol(med)]-med[row-1,ncol(med)]*0.05, med[row,ncol(med)]) 
    }
    med
  }
  nectar<-get_nworkers(nectar)
  pollen<-get_nworkers(pollen)
  
  #get concentration that the workers take up to estimate reduction in nworkers by exposure
  nectar$nworkersdose<-0.35*nectar$Conc*nectar$nworkers *0.5
  pollen$nworkersdose<-0.25*pollen$Conc*pollen$nworkers *0.5
  
  curve_sub<-curve[,c(2,8,9)]
  colnames(curve_sub)[3]<-"Survival"
  colnames(curve_sub)[1]<-"Type"
  
  
  #let's match the survival estimate
  require( data.table )
  workers <- data.table(nectar)
  drsim <- data.table(curve_sub[curve_sub$Type == "Oral",])
  setkey(workers)
  setkey(drsim)
  drsim$nworkersdose<-drsim$Dose
  drsim$workersurvival<-drsim$Survival
  drsim<-drsim[,4:5]
  nectar<-drsim[workers , on="nworkersdose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  nectar<-nectar[,c(3:21,1:2)]
  nectar$nworkers<-nectar$nworkers-((nectar$nworkers*(1-nectar$workersurvival))/2) # we assume roughly half of the workers are foraging 
  
  
  require( data.table )
  workers <- data.table(pollen)
  drsim <- data.table(curve_sub[curve_sub$Type == "Oral",])
  setkey(workers)
  setkey(drsim)
  drsim$nworkersdose<-drsim$Dose
  drsim$workersurvival<-drsim$Survival
  drsim<-drsim[,4:5]
  pollen<-drsim[workers , on="nworkersdose", roll = "nearest" ][order(Day)] #this gives us, for each day and index, the closest match to the associated mortality on the DR
  pollen<-pollen[,c(3:21,1:2)]
  pollen$nworkers<-pollen$nworkers-((pollen$nworkers*(1-pollen$workersurvival))/2) # we assume roughly half of the workers are foraging 
  
  
  
  nectar<- nectar%>% mutate(worker_res_ug_g = ifelse(Day>=workerbrood & Day<= 283, ((0.6*nworkers)*Conc)*0.01 + residues_stored, 0) ) #let's assume 1% of that nectar is contaminated; add any leftover concentration brought in by queen
  pollen<- pollen%>% mutate(worker_res_ug_g = ifelse(Day>=workerbrood & Day<= 283, ((0.4*nworkers)*Conc)*0.01 + residues_stored, 0) ) #let's assume 1% of that pollen is contaminated; add any leftover concentration brought in by queen
  
  
  nectar<-as.data.frame(nectar)
  pollen<-as.data.frame(pollen)
  
  #add function to add stores over time and decay
  get_colony_stores<-function(med){
    med$valuc<-0
    
    for(row in 2:nrow(med)){
      med[row,ncol(med)]<-ifelse(med[row-1,2]>=workerbrood, med[row,(ncol(med)-1)] + med[row-1,ncol(med)], 0)  
    }
    med$dayn<-ifelse(med$Day >= workerbrood & med$Day <=283,med$Day-workerbrood,0)
    med$worker_res_ug_g<-med$valuc * exp(-(med$k_values*med$dayn))
    
    med$worker_res_ug_g<-ifelse(med$Day >= 283,0,med$worker_res_ug_g)
    med
    
  }
  
  nectar<-get_colony_stores(nectar)
  pollen<-get_colony_stores(pollen)
  
  
  #if raining/or too cold, she eats from her store; else she is exposed while foraging
  pollen$Dose_while_forage<-ifelse(pollen$Precip >= 0.4, pollen$residues_stored*0.0485, pollen$Dose )
  pollen$Dose_while_forage<-ifelse(pollen$TempC <=6,  pollen$residues_stored*0.0485,pollen$Dose )
  nectar$Dose_while_forage<-ifelse(nectar$Precip >= 0.4, nectar$residues_stored*0.7565, nectar$Dose ) 
  nectar$Dose_while_forage<-ifelse(nectar$TempC <=6,  nectar$residues_stored*0.7565, nectar$Dose )
  
  #if it's after her in-nest activities, set to NA
  pollen$Dose_while_forage<-ifelse(pollen$Day >= endqueenforaging,NA, pollen$Dose_while_forage)
  nectar$Dose_while_forage<-ifelse(nectar$Day >= endqueenforaging,NA, nectar$Dose_while_forage)
  
  
  #if after her ending of foraging, she eats from stores collected from workers
  nectar$Dose_in_hive<-nectar$worker_res_ug_g*0.7565 # she eats approximately 100% of her daily stores in contaminated nectar in-hive
  pollen$Dose_in_hive<-pollen$worker_res_ug_g*0.0485 # she eats approximately 100% of her daily stores in contaminated pollen in-hive
  
  #if before in-nest, set to NA
  pollen$Dose_in_hive<-ifelse(pollen$Day < endqueenforaging, NA,pollen$Dose_in_hive)
  nectar$Dose_in_hive<-ifelse(nectar$Day < endqueenforaging, NA,nectar$Dose_in_hive)
  
  
  pollen$Dose<-coalesce(pollen$Dose_while_forage, pollen$Dose_in_hive)
  nectar$Dose<-coalesce(nectar$Dose_while_forage, nectar$Dose_in_hive)
  
  
  daily_proboral<-rbind(nectar,pollen)
  daily_proboral<-daily_proboral[,-c(15:25)]
  
  
  
  #curve_sub<-curve[,c(6,9:10)] 
  curve_sub<-curve[,c(2,8,9)] 
  colnames(curve_sub)[3]<-"Survival"
  colnames(curve_sub)[1]<-"Type"
  
  #Use datatable to get the rolling nearest value for mortality; we'll first establish that to decide how she moves forward
  ##ORAL
  require( data.table )
  ind_scenen <- data.table(daily_proboral[daily_proboral$Type == "Oral",])
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
  
  
  daily_proboral$Survival<-ifelse( daily_proboral$Dose ==0 &  daily_proboral$Survival >= 0.9994333 , 1,    daily_proboral$Survival) #convert to 100% survival if dose=0
  daily_probcont$Survival<-ifelse( daily_probcont$Dose ==0 &  daily_probcont$Survival >= 0.9994333 , 1,    daily_probcont$Survival) #convert to 100% survival if dose=0
  
  daily_probcont<-daily_probcont[,-c(10)]
  daily_proboral<-daily_proboral[,-c(10)]
  
  
  get_survivals<-as.data.frame(rbind(daily_probcont,daily_proboral))
  get_survivals<-get_survivals[order(get_survivals$Day),]
  get_survivals<- get_survivals[c("Media","Survival","Day")]
  get_survivals<-spread(get_survivals,key="Media",value="Survival")
  get_survivals<-get_survivals[,-(1)]
  
  cumulativesurvival<-get_survivals %>%   
    rowwise() %>%
    do(data.frame(., Prod = prod(unlist(.))))
  
  
  daily_prob<-as.data.frame(cbind(1:365, cumulativesurvival$Prod, unique(daily_probcont$Compound)))
  names( daily_prob)<-c("Day","Survival","Compound")
  daily_prob$Survival <- ifelse( daily_prob$Survival >= 0.999, 1,daily_prob$Survival)
  
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
  
  #### Probability of queen dying before she can produce the first gynes/males
  prob_nest_queen_produces_gynes<- apply(as.matrix(daily_prob[endqueenforaging:queenreproduction,2]), 2, prod)
  
  # survival_to<-c("Initation","Establishment")
  # probs<-c(prob_nest_initation, prob_nest_establish)
  # Compound<-unique(daily_prob$Compound)
  # probability_of_events<-data.frame(survival_to,probs, Compound)
  
  
  
  #what are the resuklts during the survival periods?
  survival_to<-c("Initation","Establishment","Gyne")
  probs<-c(prob_nest_initation, prob_nest_establish, prob_nest_queen_produces_gynes)
  Compound<-unique(daily_prob$Compound)
  probability_of_events<-data.frame(survival_to,probs, Compound)
  
  
  #what are the top pathways?
  pathwayscontd<-daily_probcont %>% group_by(Media) %>% summarize(maxdose = max(Dose)) 
  pathwaysorald<-daily_proboral %>% group_by(Media) %>% summarize(maxdose = max(Dose)) 
  
  pathwaysconts<-daily_probcont %>% group_by(Media) %>% summarize(minsurvival= min(Survival),medsurvival= median(Survival))
  pathwaysorals<-daily_proboral %>% group_by(Media) %>% summarize(minsurvival= min(Survival),medsurvival= median(Survival)) 
  
  pathways<-cbind(rbind(pathwaysorald, pathwayscontd),rbind(pathwaysorals, pathwaysconts))
  pathways<-pathways[,c(1:2,4)]
  
  
  #what the differences between in-hive and out of hive exposures?
  contactf<-daily_probcont[,c(2,3,4,5,6,7)]
  oralf<-daily_proboral[,c(2,3,4,5,6,7)]
  finaldoses<-rbind(contactf,oralf)
  
  
  return(list(daily_prob, probability_of_events,pathways, finaldoses))
  
} #function to estimate the individual events and daily survival probability

evaluate_by_scenario<-function(x){
  sim<-x
  sim<-split(sim, sim$Compound)
  
  output<-lapply(sim, get_daily_and_event_prob)
  
  #here we pull out the individual daily survival and get the product to extract a cumulative daily survival 
  daily_prob_compound<- lapply(output,`[[`, 1)
  probability_of_events_data<-lapply(output,`[[`, 2)
  pathways<-lapply(output,`[[`, 3)
  
  
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
  
  
  return(list(cumulativesurvival, probability_of_events, pathways))
  
}