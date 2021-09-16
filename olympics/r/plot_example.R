# @knitr plotexample

elist <- here::here("data","medals_event.Rda") %>% readRDS() # load tally by event 

fh <- "Diving" # plot by event 
d <- elist[[fh]]
dtab <- d %>% dplyr::select(NOCCode,Gold,Silver,Bronze) %>% # convert df to table for chord
  melt() %>%
  uncount(value) %>% # expand table by number of each medal 
  dplyr::select(variable,NOCCode) # reorder for colpal
colpal <- c(colv_pal,rep(col_lab,dtab[,2] %>% unique %>% length)) # match colpal to data length

# plot save 
require(circlize)
width <- 10
png(here::here("img") %>% paste0("/",fh %>% str_to_lower(),".png"),width = width, height = width, units = "cm", bg = "transparent", res = 250)

circos.clear()
par(mar = rep(2, 4),mfrow = c(1, 1),family = "HersheySans",font = 2) # plot pars
circos.par(start.degree = -185, # chord setup 
           gap.degree = 2, track.margin = c(-0.1, 0.1), 
           points.overflow.warning = F,
           track.height = 0.2)
circos.par(cell.padding =c(0.02, 0, 0.02, 0))


# plot 
# plot 
chordDiagram(dtab,
             grid.col = colpal,
             transparency = 0.3,
             directional = 1, # 1 = link origin is from sectors
             diffHeight  = -0.05,
             link.border = NA, # add border
             annotationTrack = c("grid"
                                 # ,"name" # to check name placment
             ),
             annotationTrackHeight = c(0.05, 0.1),
             big.gap = 5, small.gap = 2, # gaps between sectors
             link.sort = T, link.decreasing = T, # link overlap
             link.largest.ontop = T,
             preAllocateTracks = list(track.height = 0.1)
)

# add custom image per sector (here you can instead use {ggflags})
# select custom images
fid <- c("china","great-britain","united-states","canada","germany","australia","mexico","roc")
fidn <- flags_df %>% filter(name %in% fid) %>% arrange(name = factor(name,levels = fid)) %>% pull(flag)
picid <- pictogram_df %>% filter(event == fh %>% str_to_upper()) %>% pull(img) %>% img_convert()
piclab <- fh %>% str_sub(1,3) %>% str_to_upper()
  
# create img labels
imgl <- as.list(fidn) # match imgs to events in elist
imglist <- lapply(imgl,img_convert) # apply convert to raster func
imgtab <- c(as.list(rep(NA,3)),imglist) # add country and empty imgs for three medal sectors  
names(imgtab) <- get.all.sector.index() # get names from plot sector indices 

ylim <- 0.7
im <- "4mm"
cex <- 0.5
circos.track(track.index = 1, 
             panel.fun = function(x,y){ # add text/img per sector
               circos.raster(x = CELL_META$xcenter, 
                             y = ylim,
                             image = imgtab[[CELL_META$sector.numeric.index]], # add image by indexing each cell sector from img df (imgtab) 
                             width = im, height = im,
                             facing = "clockwise",niceFacing = T)
               circos.text(x = CELL_META$xcenter, 
                           y = ylim + 1.2, 
                           # remove medal sector labels but keep other sectors
                           labels =  ifelse(CELL_META$sector.numeric.index <= dtab[,1] %>% unique %>% length, NA, CELL_META$sector.index),
                           facing = "clockwise", niceFacing = T, cex = cex, col = col_lab
               )}
             , bg.border = NA) # set bg to NA

# add icon stamp 
xx <- 0.2; yy <- 0.8
grid.raster(picid, x=xx, y=yy, width=0.1, height = 0.1) # add country flag
text(-0.99, yy+0.2, piclab, col = col_lab, cex = 1, pos = 4)

dev.off() # close plot save
