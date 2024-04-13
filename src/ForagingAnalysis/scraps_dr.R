


#let's modify the input data (LD50s/LC50s) to be standard 24 hour estimates

ld50s<-c(0.05,0.08,0.04,0.10) #associated ld50/lc50 doses
duration<-c(96,48,96,24) #time of experiment

example_df<-data.frame(ld50s,duration)

#we originally used this approach to modify survival rates for doses to a 96 hour standard; based off of an exponential growth curve as the 
example_df$how_long_until_100<-(example_df$duration*log(2))/log(1/0.50) #how long does it take for the substance to kill ha
example_df$adj_ld50_24<-round(1/(2^(24/example_df$duration)),3) #concentations should be higher if timeframe is 48/96h; this is obviously an extreme example but this is the general conceptual framework 




effects<-read.csv('C:\\Users\\epauluko\\OneDrive - Environmental Protection Agency (EPA)\\Profile\\Documents\\GitHub\\amphibian_effects_model\\data_in\\Headline_updated.csv') #run code in Data_Sim to clean up the effects and remove rows we don't want
effects$how_long_until_100_mortality<-(effects$Duration_h*log(2))/log(1/effects$Survival) #need to modify survival by duration, using an exponential growth curve
effects$adj_sur_96<-round(1/(1^(96/effects$half_life)),3)

install.packages('drc')
library(drc)
clothi_dose <- c(0.0000067, 0.0000302, 0.0000937, 0.0000067, 0.0000302, 0.0000937, 0.0000067, 0.0000302, 0.0000937, 0.0000067, 0.0000302, 0.0000937,0.04,0.10,0.08)
clothi_response <- c(0.05, 0.25, 0.5,0.05, 0.25, 0.5, 0.05, 0.25, 0.5, 0.05, 0.25, 0.5, 0.9,0.99,0.999)
clothi_ll4 <- drm(clothi_response ~ clothi_dose, 
                  fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(clothi_ll4)

LD(clothi_ll4,c(5,10,50,90,99))


#code to find closest matching value 
# test = oral %>% group_by(index)%>%
#   left_join(curve , by='Compound') %>%
#   group_by(Day) %>% 
#   mutate(dif = abs(Dosen - Dose)) %>%
#   filter(dif == min(dif))
# 




#we want to extract the slope from the line, and use that to center our LD50

#imidacloprid
imidaslope<-as.data.frame(cbind(grab_quantiles,imidacloprid_tox_quantileso))
colnames(imidaslope)<-c("y","x")
# riserun <-  diff(imidaslope$y)/diff(imidaslope$x)
# slope<-(max(riserun))
# # [1] 487.7172
# which.max(riserun)
# # [1] 168
# 
# imidaslope[531,] #look right?


eecs<-imidacloprid_tox_quantileso
ld50 <- 0.006
# looks like this gives kernel density and not cumulative
# density and needs to be converted
# z<-(slope)*(log(eec)-log(ld50))
# phat<-(1/(2*pi)^0.5)*exp(-(z^2)/2)
# phat

ec90<-imidaslope[imidaslope$y == 0.90,2]
ec10<-imidaslope[imidaslope$y == 0.10,2]
slope<-log10(81)/(log10(ec90/ec10))

phatun<-1/(1+(eecs/ld50)^slope)
phatun




# ld50_point <- data.frame(x = 0.006, y = 0.5)
# 
# ggplot(data = data.frame(x = eecs, y = 1-phatun), aes(x = x, y = y)) +
#   geom_line()+
#   geom_point(data = ld50_point, aes(x = x, y = y), color = "red", size = 3)



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

tox_quantilesd <- cbind(grab_quantiles, bifenthrin_tox_quantilesd, carbaryl_tox_quantilesd, chlorpyrifos_tox_quantilesd,
                        clothianidin_tox_quantilesd, imidacloprid_tox_quantilesd, thiamethoxam_tox_quantilesd)

tox_quantilesd<-as.data.frame(tox_quantilesd)

dr<-gather(tox_quantilesd, "Compound","Dose", 2:7)
colnames(dr)[1]<-"Mortality"
dr$Compound<- str_to_title(sub("\\_.*", "", dr$Compound))

dr<-merge(x = dr, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)

ggplot(dr, aes(x = (Dose), y = Mortality)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Contact_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  # scale_x_continuous(trans=scales::log_trans(),
  #                    labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")


#### Another option is to just simulate some hypothetical ranges of doses and use the bombus LD50 and honeybee slope with those. 
seq_doses <- seq(0.000001, 10, 0.0001) #what's the associated mortality %?

drd<-drd %>% group_by(Compound) %>% mutate(Dose=seq_doses)
dro<-dro %>% group_by(Compound) %>% mutate(Dose=seq_doses)








grab_quantiles <- seq(0.00001, 1, 0.00001) #what's the associated mortality %?
# times 3 for Haber's rule (n=1) assuming derived relationships are for 72 hours and we want 24

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











prob_nest_establishment<- apply(daily_proboral[1:365,14], 2, prod) #but need to actually cut off at end of foraging!
prob_larval_development<- apply(daily_proboral[1:365,14], 2, prod) #but need to actually cut off at end of foraging!


# in how many scenarios does the queen die before initiation? what is the threshold for survival to next day?
# cumulative mortality at:
# prob of nest initiation [emerge + random initiation] #if she dies in this period, no go
# prob of nest establishment [initiation + 2/3 weeks] #if she dies in this period, no go
# prob larval development # what survival do they have with consumption?


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
    theme(legend.position = "none",
        axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"))
  
  
  
  daily_prob_plot
  
  cumulative_survival<- apply(daily_prob[1:365,14], 2, prod) #but need to actually cut off at end of foraging! i.e., after certain day mortality to foundress queen not important
  
}


