# nathaniel_lolo_mapdeck
pacman::p_load(dplyr,here,scales,sf,htmlwidgets)
setwd("/Users/malishev/Documents/Data/misc/nathaniel_lolo")
# devtools::install_github("SymbolixAU/mapdeck") # install latest mapdeck
library(mapdeck)


# data --------------------------------------------------------------------
point_height <- 10
latlon <- data.frame("elevation"=1:point_height,
                     "lat"=rep(33.7865,point_height),
                     "lon"=rep(-84.3773,point_height),
                     "opacity"=rep(150,point_height), # sample(1:255, size = nrow(latlon),replace=T) 
                     "label"=c("Nathaniel and Lolo",rep(NA,point_height-1))
)

# inputs ------------------------------------------------------------------
my_style <- "mapbox://styles/darwinanddavis/ck980j6wc3knc1imtt565hl5r/draft"
zoom <- 3
width <- 1500
height <- 1500
padding <- 0
pitch <- 30
radius <- 5
col1 <- "#a46cd9"
col2 <- "#5800df"
col3 <- "#2c00af"
colvec <- c(col1,col2,col3)
fill_colour <- "#fcfafe"
highlight_colour <- "#CC5800df" # needs to be hexcode with alpha prefix  
scales::show_col(colvec)
fill_opacity <- 150 # 0:255
auto_highlight <- T # highlight on hover

# opac for hexcode 
# https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4

# plot --------------------------------------------------------------------

# save to html to view OR viewer > new window
p <- mapdeck(latlon, 
             style = my_style, #apdeck_style("dark"), # my_style, 
             zoom = zoom,
             width = width,
             height = height,
             padding = padding,
             pitch = pitch)

p <- p %>% add_column(lat = "lat",
                      lon = "lon",
                      elevation = 1,
                      elevation_scale = 1*10^10,
                      radius = 200,
                      coverage = 0.5,
                      angle = 180,
                      fill_colour = "#ffffff",
                      fill_opacity = 150,
                      auto_highlight = T,
                      highlight_colour = "#CC030303",
                      layer_id = "column_layer")
p
  
# pointcloud layer 
add_pointcloud(lat = "lat", 
                 lon = "lon",
                 elevation = "elevation",
                 radius = radius, 
                 fill_colour = fill_colour,
                 fill_opacity = "opacity",
                 auto_highlight = auto_highlight, 
                 highlight_colour = highlight_colour,
                 palette = colvec,
                 update_view = T
  )  
p

p %>% saveWidget(paste0(getwd(),"/index.html"))

# 
# p <- p %>% 
#   add_text(lat = "lat", 
#            lon = "lon",
#            text="label",
#            fill_colour = fill_colour,
#            size = 2,
#            auto_highlight = T,
#            highlight_colour = highlight_colour,
#            update_view = T
#            )
# p