

library(drc)
library(ordinal)
library(EnvStats)
library(ExtDist)
library(ggpmisc) 

#add beetox to check ld50 on curve

tox<-read.csv(paste0(pest_dir, "/BeeTox.csv"))
tox<-tox[,1:3]
tox$Compound<-str_to_title(tox$Compound)
grab_quantiles <- seq(0.001, 0.999, 0.001) #what's the associated mortality %?

#distribtuion for oral
imidacloprid_tox_quantileso <- ((10^qWeibull(grab_quantiles, 8.255, 0.445))*3)/1000
dr<-as.data.frame(cbind(grab_quantiles,imidacloprid_tox_quantileso))
colnames(dr)<-c("Mortality","Dose")
dr$Compound<- "Imidacloprid"
dr<-merge(x = dr, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)

ggplot(dr, aes(x = (Dose), y = Mortality)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+

  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")

imidaslope<-as.data.frame(cbind(grab_quantiles,imidacloprid_tox_quantileso))
colnames(imidaslope)<-c("y","x")
riserun <-  diff(imidaslope$y)/diff(imidaslope$x)
slope<-max(riserun)
slope
which.max(riserun)
imidaslope[531,] #look right on the curve?

eec<-0.007
ld50<-0.006
z<-log10(slope)*(log(eec)-log(ld50)) #standard normal deviate
phat<-(1/(2*pi)^0.5)*exp(-(z^2)/2) # p-hat corresponds to the proportion of honeybee mortality associated with eec
phat #should be > 0.5...?

#this ld50 doesn't make sense either
10^(log(ld50)+z/slope)


install.packages('ecotox')
library(ecotox)

test<-LC_probit()


results <- LC_probit(Mortality ~ (Dose),
                     p = c(grab_quantiles ) ,
                     data=dr)
results


p1 <- ggplot(data = dr,
             aes(x = (Dose), y = Mortality)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = binomial(link = "probit")),
              colour = "#FF0000", se = TRUE)
p1



##what we want to be able 


#get example of modeled min/max
# library(purrr)
# comp<-map(exp_dose, 1)
# comp<-do.call(rbind,comp)
# comp<-comp %>% group_by(Compound) %>%
#   mutate(max = max(Dose)) %>%
#   mutate(min = min(Dose))
# 
# comp$Compound<-str_to_title(comp$Compound)
# 
# comp<-comp[comp$Day==1,]
# comp<-comp[comp$type == "Oral",]
# 
# dr<-merge(x = dr, y = comp[ , c("Compound",  "max", "min")], by = "Compound", all.x=TRUE)








