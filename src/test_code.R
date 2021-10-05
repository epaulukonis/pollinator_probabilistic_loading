

print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE))
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE))
shape<-readOGR(state_dir, layer = "IL_BNDY_County_Py")
plot(shape)
shape

bomb_h <- readOGR(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
bomb_l <- readOGR(bombus_dir, layer = "RPBB_Low_Potential_Zones_03172021")
plot(bomb_l)

shape <- rea
plot(bomb_h, add=T, col='red')

shape<-spTransform(shape, crs(bomb_l))

plot(shape)
plot(bomb_l, add=T, col='blue')
plot(bomb_h, add=T, col='red')


# start with peoria county
il.peoria<-shape[shape$COUNTY_NAM == "PEORIA",]
plot(il.peoria)
plot(bomb_l, add=T)
