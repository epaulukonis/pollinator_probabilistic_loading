#######################################################
#                 Animate rasters with R
#                 Milos Popovic
#                 2023/06/29
########################################################

remotes::install_github(
    "dieghernan/tidyterra"
)

libs <- c(
    "tidyverse", "terra", "tidyterra",
    "osmdata", "sf", "ggmap", "classInt",
    "gifski"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(lapply(
    libs, library, character.only = T
))


### 5. ANIMATE MAP
### --------------

names(istanbul_stacked) <- years
istanbul_layers <- names(istanbul_stacked)

for (i in seq_len(length(daily_scenario))) {
    day_layer <- daily_scenario[[i]]

    day <- as.numeric(i)
    names(day_layer)
    
    habitat <- mask(crop(nlcd, colony), colony)

    p <- ggplot()+
        tidyterra::geom_spatraster(data=habitat)+
      scale_fill_gradientn(
        name = "",
        colours = hcl.colors(
          20, "Light Grays",
          alpha = .4
        ),
        na.value = NA
      ) +
      ggnewscale:::new_scale_fill() +# plotting the map
      tidyterra::geom_spatraster(data = day_layer) +
      scale_fill_gradientn(
        name = "",
        colours = hcl.colors(
          20, "viridis",
          alpha = .8
        ),
        na.value = NA
      ) +
  
        # guides(
        #     fill = guide_legend(
        #         direction = "horizontal",
        #         keyheight = unit(1.25, "mm"),
        #         keywidth = unit(15, "mm"),
        #         label.position = "bottom",
        #         label.hjust = .5,
        #         nrow = 1,
        #         byrow = T
        #     )
        # ) +
        theme_void() 
        
     
    
    p

    map_name <- file.path(
        paste0(root_data_out, "/animations/",
            "animate_",i,
            "_layer.png"
        )
    )

              ggsave(
                  map_name, p,
                  width = 6, height = 8.5,
                  units = "in", bg = "white"
              )
          }



  map_files <- file.path(paste0(root_data_out, "/animations"), list.files(path=paste0(root_data_out, "/animations"), pattern='.png$', all.files=TRUE, full.names=FALSE))
  
  
map_files<-map_files[(mixedsort(as.character(map_files)))]


  gifski::gifski(
      map_files,
      loop = T,
      delay = .5,
      width = 1200,
      height = 1200,
      gif_file = "dailypollen.gif"
  )

