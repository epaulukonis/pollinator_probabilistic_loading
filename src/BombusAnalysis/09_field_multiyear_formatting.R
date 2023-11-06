### Probabilistic Crop Loading 

### 09 Spatial data formatting

# Edited by E. Paulukonis August 2023


nc <- st_read(system.file("shape/nc.shp", package="sf"))
st_equals(nc[1:5,], nc[1:4,])

ab <- st_join(df1,df2, join = st_equals_exact, suffix = c("_df1", "_df2"), left = F)

df1<-scenarios[[1]]
df2<-scenarios[[2]]
testy<-st_equals(df1,df2)

threshold <- 5
testy<-purrr::map(scenarios, function(x) {
  x[x[["id"]] == threshold & x[["Commdty"]] == "CORN", ]
})

test_df<-testy[[3]]
test_df2<-testy[[8]]

plot(test_df2["geometry"])