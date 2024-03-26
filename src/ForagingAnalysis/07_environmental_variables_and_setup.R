### Foraging Landscape

### 07 Importing weather and simulation of the 1000 scenarios 

weather <-read.csv(paste0(bombus_dir, "/foraging/weather.csv"))
weather<-weather[weather$Year == 2014,]

weather$Date <- as.Date(with(weather, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")




comp<-map(exp_dose_output, 1)
comp<-do.call(rbind,comp)