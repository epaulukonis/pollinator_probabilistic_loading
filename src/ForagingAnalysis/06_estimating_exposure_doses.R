### Foraging Landscape

### 06 Estimating exposure doses

print("stepping into 06: estimating probability of survival")
library(readr)


#get k values for flower contact
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)
apprates<-apprates %>% distinct(k_values, .keep_all = T)

#get endpoints
endp<-read.csv(paste0(root_data_in,"/PesticideData/BeeTox.csv"))
endp$Compound<-toupper(endp$Compound)
endp<-merge(endp, apprates[,c("Compound", "k_values")], by="Compound", all.x=T, all.y=F)


weather <-read.csv(paste0(bombus_dir, "/foraging/weather.csv"))
weather<-weather[weather$Year == 2014,]
colnames(weather)[4:8]<-c("Precip","Evap","TempC","Windcmsec",'Solar')
weather$Day<-1:365




#### Calculate daily exposure doses based on a separate system for contact and oral; doses are assumed to be additive under worst case scenarios

  print(list.files(path=paste0(root_data_out,'/all_forage/media_tables/1-250'), pattern='.csv', all.files=TRUE, full.names=FALSE))
  scenarios<- file.path(paste0(root_data_out,'/all_forage/media_tables/1-250'), list.files(path=paste0(root_data_out,'/all_forage/media_tables/1-250'), pattern='.csv', all.files=TRUE, full.names=FALSE))
  scenarios<-setNames(lapply(scenarios, read.csv), tools::file_path_sans_ext(basename(scenarios)))
  
 ##Two ways to split, if needed:

 #split by compound
 bifenthrin<- scenarios[grep("BIFENTHRIN", names(scenarios))]
 clothianidin<- scenarios[grep("CLOTHIANIDIN", names(scenarios))]
 chlorpyrifos<- scenarios[grep("CHLORPYRIFOS", names(scenarios))]
 imidacloprid<- scenarios[grep("IMIDACLOPRID", names(scenarios))]
 carbaryl<- scenarios[grep("CARBARYL", names(scenarios))]
 thiamethoxam<- scenarios[grep("THIAMETHOXAM", names(scenarios))]

 list_of_compound_scenarios<-list(bifenthrin,carbaryl,clothianidin,chlorpyrifos,imidacloprid,thiamethoxam)

 #visual<-clothianidin[[1]]
 
 
 #split by scenario number
 scenario_numbers<-parse_number(names(scenarios))
 names(scenarios)<-scenario_numbers
 scenarios<-map2(scenarios, names(scenarios), ~ mutate(.x, new_col = .y)) #add new column for scenario number
 scenario_numbers<-as.character(1:9)
       list_by_gen_number<-list()
     for(number in 1:length(scenario_numbers)){
       value<-scenario_numbers[[number]]
       output<-scenarios[grep(value, names(scenarios))]
       get_gen_scenario<-do.call(rbind,output)
       list_by_gen_number[[number]]<-get_gen_scenario
     }

      output<-do.call(rbind,list_by_gen_number)

 list_of_individual_scenarios<-split(output, output$new_col)

 

 #### first let's do some quality checking 
# options(scipen = 999)
 QA<-function(compound){
  
 compound<- Map(cbind, compound, index = seq_along(compound))
 
 comp<-as.data.frame(do.call(rbind, compound))
 comp<-comp %>% group_by(index, Media) %>%
   mutate(max = max(Conc)) %>%
   mutate(min = min(Conc)) %>%
   mutate(median = median(Conc)) 
   
 comp<-comp[comp$Day == 1,]
 comp<- comp[order(comp$Media),,drop=FALSE]
 comp
 }

 qa_test<-lapply(list_of_compound_scenarios,QA) #look at the median/maxs; are they very high? they should not be much higher than those reported for single days in Ch. 2. If so, something is iffy. 
 

 
#Calculate doses if by scenario ----
 
 options(scipen = 0)
 get_exposure_dose<-function(x){
   
   scenario<-x
   scenario<-list_of_individual_scenarios[[1]]
   scenarios<-split(scenario,scenario$Compound)
   
   
   daily_exposure_list<-list()
   for(n in 1:length(scenarios)){
     scenario<-scenarios[[n]]
     scenarion<-merge(x = scenario, y = endp[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee","k_values")], by = "Compound", all.x=TRUE)
     
     # add in a new media for 'flower' for contact
     # flower<-scenarion[scenarion$Media =="Dust"|scenarion$Media == "Air",]
     # flower$Media<-"Flower"
     # scenarion<-rbind(scenarion, flower)
     
     
     contact<-scenarion[scenarion$Media =="Dust"|scenarion$Media == "Air"| scenarion$Media =="Soil", ] #|scenarion$Media == "Flower"
     oral<-scenarion[scenarion$Media == "Pollen" | scenarion$Media == "Nectar" , ]
     
     ### CONTACT ---
     
     #daily dose for aerial deposition
     aerial<-contact[contact$Media == "Dust"| contact$Media == "Air",]
     aerial$exp_dose<- (aerial$Conc/2) *1.6E-4*(10) #calculated as the dose experienced from single-day contact with aerial deposition within a hypothetical 'flight tube'; assume one flight through cloud
     
     
     # #daily dose for flower contact from deposition
     # flower<-contact[contact$Media == "Flower",]
     # 
     # flower<-flower %>%
     # group_by(id = cumsum(!Conc==0)) %>%
     #   mutate(Concf = ifelse(Conc==0, first(Conc), Conc)) %>%
     #   mutate(dayn = ifelse(Conc>0, Day - first(Day)+1, Day))
     # 
     #   ind <- which(flower$Concf != lag(flower$Concf))
     #   flower$Concf[ind] <- sapply(ind, function(i) with(flower, sum(c(Concf[i-1], Concf[i+1]))))
     #   ind <- which(flower$Concf > lag(flower$Concf))
     #   flower$Concf[ind:nrow(flower)] <- sapply(ind, function(i) with(flower, Concf[ind-1]))
     # 
     #  flower$exp_dose<-(flower$Concf * 1e-04 * 6.5)* 90 * exp(-(flower$k_values*flower$Day))#calculated as the dose experienced from contact with deposition on flowers, assuming a visit of 90 repeat events, degrades over time
     #  flower<-subset(flower, select=-c(id,dayn,Concf))
     
     #daily dose for soil exposures; #soil conc in ug/m2 needs to be converted to cm2
     soil<-contact[contact$Media == "Soil",]
     soil$exp_dose <- (soil$Conc/10000 * 1.95 * 2 ) * 6.5 #surface area of bumblebee queen  # we assume that at some point, the foundress makes contact with contaminated soil
     
     contact<-rbind(aerial,soil)
     
     #sum all contact doses from different sources
     contactf<-contact %>% group_by(Day) %>% mutate(Dose = sum(exp_dose))
     contactf<-contactf[,c(1:4,6:7,10)] #just get overall daily contact exposure
     contactf$Type<-"Contact"
     
     ### calculate overall daily probability of survival from contact doses
     #scenarion$daily_prob_survival<-1-((scenarion$exp_dose/scenarion$Contact_LD50_ug_bee)*0.5) #center around LD50, i.e., above or below LD50 value translates to probability of mortality; this is scrapped in favor of ssdtoolbox approach
     
     
     ### ORAL ---
     
     #daily dose for oral exposures; let's assume for now that a bombus queen would hypothetically be getting 50% of her resources from contaminated areas
     oral<-oral %>%
       mutate(exp_dose = Conc * case_when(
         Media == "Pollen" ~ (0.0485),
         Media == "Nectar" ~  (0.75658)
       ))
     
     oralf<-oral %>% group_by(Day) %>% mutate(Dose = sum(exp_dose))
     oralf<-oralf[,c(1:4,6:7,10)] #just get overall daily oral exposure
     oralf$Type<-"Oral"
     
     
     daily_exposures<-as.data.frame(rbind(contactf,oralf))
     
     daily_exposures<-merge(x = daily_exposures, y = weather[ , c("Day", "Precip","Evap","TempC","Windcmsec",'Solar')], by = "Day", all.x=TRUE)
     
     
     #daily_exposures<-daily_exposures[daily_exposures$Day <283,] #remove values from end of season 
     
     daily_exposure_list[[n]]<-daily_exposures
     
   }
   daily_exposure_list<-do.call(rbind,daily_exposure_list)
   daily_exposure_list
  
   
   #names(daily_exposure_list)<-names(x)
   
   
 }
 
 
 exp_dose_output<-lapply(list_of_individual_scenarios, get_exposure_dose)  
 names(exp_dose_output)<-names(list_of_individual_scenarios)
 


#Calculate doses if by compound ----
 options(scipen = 0)
  get_exposure_dose_compound<-function(x){

  scenarios<-x
  
  daily_exposure_list<-list()
  for(n in 1:length(scenarios)){
    scenario<-scenarios[[n]]
    scenarion<-merge(x = scenario, y = endp[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee","k_values")], by = "Compound", all.x=TRUE)
    
   # add in a new media for 'flower' for contact
   # flower<-scenarion[scenarion$Media =="Dust"|scenarion$Media == "Air",]
   # flower$Media<-"Flower"
   # scenarion<-rbind(scenarion, flower)
   
   
   contact<-scenarion[scenarion$Media =="Dust"|scenarion$Media == "Air"| scenarion$Media =="Soil", ] #|scenarion$Media == "Flower"
      oral<-scenarion[scenarion$Media == "Pollen" | scenarion$Media == "Nectar" , ]
 
      ### CONTACT ---
       
       #daily dose for aerial deposition
        aerial<-contact[contact$Media == "Dust"| contact$Media == "Air",]
        aerial$exp_dose<- (aerial$Conc/2) *1.6E-4*(20) #calculated as the dose experienced from single flight through aerial deposition within a hypothetical 'flight tube'
      

        # #daily dose for flower contact from deposition
        # flower<-contact[contact$Media == "Flower",]
        # 
        # flower<-flower %>%
        # group_by(id = cumsum(!Conc==0)) %>%
        #   mutate(Concf = ifelse(Conc==0, first(Conc), Conc)) %>%
        #   mutate(dayn = ifelse(Conc>0, Day - first(Day)+1, Day))
        # 
        #   ind <- which(flower$Concf != lag(flower$Concf))
        #   flower$Concf[ind] <- sapply(ind, function(i) with(flower, sum(c(Concf[i-1], Concf[i+1]))))
        #   ind <- which(flower$Concf > lag(flower$Concf))
        #   flower$Concf[ind:nrow(flower)] <- sapply(ind, function(i) with(flower, Concf[ind-1]))
        # 
        #  flower$exp_dose<-(flower$Concf * 1e-04 * 6.5)* 90 * exp(-(flower$k_values*flower$Day))#calculated as the dose experienced from contact with deposition on flowers, assuming a visit of 90 repeat events, degrades over time
        #  flower<-subset(flower, select=-c(id,dayn,Concf))
   
        #daily dose for soil exposures
         soil<-contact[contact$Media == "Soil",]
         soil$exp_dose <- (soil$Conc/10000 * 1.95 * 2 ) * 6.5 #surface area of bumblebee queen
      
      contact<-rbind(aerial,soil)
      
      #sum all contact doses from different sources
      contactf<-contact %>% group_by(Day) %>% mutate(Dose = sum(exp_dose))
      contactf<-contactf[1:365,c(1:3,5:6,9)] #just get overall daily contact exposure
      contactf$type<-"Contact"
      
      ### calculate overall daily probability of survival from contact doses
       #scenarion$daily_prob_survival<-1-((scenarion$exp_dose/scenarion$Contact_LD50_ug_bee)*0.5) #center around LD50, i.e., above or below LD50 value translates to probability of mortality; this is scrapped in favor of ssdtoolbox approach

  
       ### ORAL ---
      
        #daily dose for oral exposures
       oral<-oral %>%
           mutate(exp_dose = Conc * case_when(
             Media == "Pollen" ~ 0.0485,
             Media == "Nectar" ~  0.7565
           ))
    
        oralf<-oral %>% group_by(Day) %>% mutate(Dose = sum(exp_dose))
        oralf<-oralf[1:365,c(1:3,5:6,9)] #just get overall daily oral exposure
        oralf$type<-"Oral"

      
  daily_exposures<-as.data.frame(rbind(contactf,oralf))

  daily_exposures<-merge(x = daily_exposures, y = weather[ , c("Day", "Precip","Evap","TempC","Windcmsec",'Solar')], by = "Day", all.x=TRUE)
  
  
  #daily_exposures<-daily_exposures[daily_exposures$Day <283,] #remove values from end of season 
  
  daily_exposure_list[[n]]<-daily_exposures
    
  }
  
  names(daily_exposure_list)<-names(scenarios)
  daily_exposure_list
  }

  
  exp_dose_output_compounds<-lapply(list_of_compound_scenarios, get_exposure_dose_compound)  
  
  
  

  
  
  
  
