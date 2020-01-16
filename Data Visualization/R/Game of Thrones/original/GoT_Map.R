setwd("D:/IE/Term 2/Data Visualization/Datasets for Visualization/GoT Deaths/Westeros_Essos_shp/GoTRelease/")

library(Hmisc)
library(rgdal)
library(tmap)
library(RColorBrewer)
#Read in two shapefiles
westeros <- readOGR(".", "political")
locations <- readOGR(".", "locations")
#Cleaning factor levels
westeros@data$name <- `levels<-`(addNA(westeros@data$name),
                                 c(levels(westeros@data$name),
                                   "The Lands of Always Winter"))
levels(westeros@data$name)[1] <- "The Wall"
levels(westeros@data$name)[4] <- ""
levels(westeros@data$ClaimedBy)[11] <- "White Walkers"
#Identify capitals
places <- as.character(locations@data$name)
places <- gsub(" ", "_", places)
capitals <- c("Winterfell", "The Eyrie", "Harrenhal", "Sunspear",
              "King's Landing", "Castle Black", "Pyke",
              "Casterly Rock", "Storm's End", "Highgarden")
holds <- locations[locations@data$name %in% capitals, ]
#Castle icon
castle <- tmap_icons(file = "https://image.ibb.co/kykHfR/castle.png", keep.asp = TRUE)
#Locations we rather keep
interesting <- c("Fist of the First Men", "King's Landing",
                 "Craster's Keep", "Tower of Joy")
#Locations we rather get rid of
intheway <- c("Sarsfield", "Hornvale", "Cider Hall",
              "Hayford Castle", "Griffin's Roost", "Vulture's Roost")
#Subsetting for keeping only "castles" and interesting places
locations <- locations[locations@data$type == "Castle" |
                         locations@data$name %in% interesting, ]
#Subsetting for places in the way and capitals - we will plot them with the holds layer
locations <- locations[locations@data$name %nin% c(intheway, capitals), ]
#Color palettes - the hard way
blues <- brewer.pal(6, "Blues")
reds <- brewer.pal(7, "Reds")
sorted <- c(blues[3], reds[4], blues[4], reds[2], reds[6],
            #vale, stormlands, iron islands, westerlands, dorne
            blues[6], blues[5], reds[3], reds[1], reds[5], blues[1])
#wall, winterfell, crownsland, riverlands, reach, beyond the wall
#Map
m <- tm_shape(westeros) +
  #Colour regions using the sorted palette and plot their names
  tm_fill("ClaimedBy", palette = sorted) +
  tm_text("name", fontfamily = "Game of Thrones", size = .4, alpha = .6) +
  #Plot location names and put a dot above them
  tm_shape(locations) +
  tm_text("name", size = .2, fontfamily = "Roboto Condensed", just = "top") +
  tm_dots("name", size = .01, shape = 20, col = "black", ymod = .1) +
  #Plot capitals and add custom shape
  tm_shape(holds) +
  tm_text("name", size = .25, fontfamily = "Roboto Condensed") +
  tm_dots("name", size = .05, alpha = .5, shape = castle, border.lwd = NA, ymod = .3) + 
  #Fluff
  tm_compass(type = "8star", position = c("right", "top"), size = 1.5) +
  tm_layout(bg.color = "lightblue", main.title = "Westeros", frame.lwd = 2,
            fontfamily = "Game of Thrones") +
  tm_legend(show = FALSE)
m
#Code for hi-res version
#save_tmap(m, "westeros_hires.png", dpi = 300, asp = 0, height = 30, scale = 3)