# @knitr pictograms

# save pictograms  -----------------------------------------------
li <- 1
for(li in seq_along(pictogram_df$event)){
  img <- pictogram_df$img[li] %>% img_convert()
  png(here::here("legend") %>% paste0("/",pictogram_df$event[li],".png"),width = width, height = height, units = "cm", bg = "transparent", res = 250)
  par(mfrow=c(1,6),mar=rep(0,4))
  plot.new()
  grid.raster(img,0.5,0.5,width=0.5, height = 0.5) # add country flag
  dev.off()
}

# get pictogram labels
slist <- list()
for(ci in seq_along(clist)){
  cd <- clist[[ci]] %>% 
    select(Sport) 
  slist <- c(slist,cd)
}