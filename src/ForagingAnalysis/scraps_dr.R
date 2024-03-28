


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