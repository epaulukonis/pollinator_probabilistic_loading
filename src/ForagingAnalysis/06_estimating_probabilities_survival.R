### Foraging Landscape

### 06 Estimating probability of survival

print("stepping into 06: estimating probability of survival")


apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)

apprates<-apprates %>% distinct(k_values, .keep_all = T)


### There are a number of ways we can estimate the daily survival probability, ranging from relatively simple (based simply on ratios from RQs) or more complex (involving DR curves or toxicodynamic relationships)


#### Simplistic Approach

  print(list.files(path=paste0(root_data_out,'/all_forage/media_tables'), pattern='.csv', all.files=TRUE, full.names=FALSE))
  scenarios<- file.path(paste0(root_data_out,'/all_forage/media_tables'), list.files(path=paste0(root_data_out,'/all_forage/media_tables'), pattern='.csv', all.files=TRUE, full.names=FALSE))
  
  scenarios<-setNames(lapply(scenarios, read.csv), tools::file_path_sans_ext(basename(scenarios)))
  
  scenario<-scenarios[[6]]
  
  
  endp<-read.csv(paste0(root_data_in,"/PesticideData/BeeTox.csv"))
  endp$Compound<-toupper(endp$Compound)
  
  endp<-merge(endp, apprates[,c("Compound", "k_values")], by="Compound", all.x=T, all.y=F)
    
  
  for(scenario in 1:length(scenario)){
    scenarion<-merge(x = scenario, y = endp[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee","k_values")], by = "Compound", all.x=TRUE)
    
    #add in a new media for 'flower' for contact
   flower<-scenarion[scenarion$Media =="Dust"|scenarion$Media == "Air",]
   flower$Media<-"Flower"
   scenarion<-rbind(scenarion, flower)
   
   
   contact<-scenarion[scenarion$Media =="Dust"|scenarion$Media == "Air"| scenarion$Media =="Soil"|scenarion$Media == "Flower", ]
      oral<-scenarion[scenarion$Media == "Pollen" | scenarion$Media == "Nectar" , ]
 
      ### CONTACT
      
      if (contact$Media == "Dust" | contact$Media == "Air"){
        
        contact<-contact[contact$Media == "Dust",]
      
      #daily dose for aerial deposition
      contact$exp_dose<- (contact$Conc/2) *1.6E-4*991 #calculated as the dose experienced from single-day contact with aerial deposition within a hypothetical 'flight tube'
      
      } else if (contact$Media == "Flower"){
        
        contact<-contact[contact$Media == "Flower",]
        
        
        #this section handles the subsequent addition of new dust events and appropriately assigns values; it's tricky due to the addition so this is a less efficient way to do this but it works 
        contact<-contact %>%
        group_by(id = cumsum(!Conc==0)) %>%
          mutate(Concf = ifelse(Conc==0, first(Conc), Conc)) %>%
          mutate(dayn = ifelse(Conc>0, Day - first(Day)+1, Day))

        ind <- which(contact$Concf != lag(contact$Concf))
        contact$Concf[ind] <- sapply(ind, function(i) with(contact, sum(c(Concf[i-1], Concf[i+1]))))
        
        ind <- which(contact$Concf < lag(contact$Concf))
        contact$Concf[ind:nrow(contact)] <- sapply(ind, function(i) with(contact, Concf[ind-1]))
  
        
       #daily dose for flower contact from deposition
       contact$exp_dose<-(contact$Concf * 1e-04 * 6.5)* 90 * exp(-(contact$k_values*contact$Day))#calculated as the dose experienced from contact with deposition on flowers, assuming a visit of 90 repeat events, degrades over time
      
        } else {
      
      #daily dose for soil exposures
          contact$exp_dose <- (contact$Conc * 1.95 * 2 ) * 6.5 #surface area of bumblebee queen
        }
      
      
      
      
      #sum all contact doses from different sources
      contactf<-contact %>% group_by(day) %>% mutate(exp_dose_total = sum(exp_dose))
      
      ### calculate overall daily probability of survival from contact doses
       #scenarion$daily_prob_survival<-1-((scenarion$exp_dose/scenarion$Contact_LD50_ug_bee)*0.5) #center around LD50, i.e., above or below LD50 value translates to probability of mortality; this is scrapped in favor of ssdtoolbox approach

  
       ### ORAL
      
    
      
      
      
      
      

    
  }
  
  
  unique(endp$Compound)
  unique(scenario$Compound)  
 