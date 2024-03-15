


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

