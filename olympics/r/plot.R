# @knitr plot

# plot tally by country ----------------------------------------------------------
lid <- 0 # add leaderboard id
for(countryid in clist %>% names){
  # pull hires country flag
  fid <- flags_df %>% filter(name %in% countryid) %>% pull(flag) %>% magick::image_read() %>% as.raster() # get selected country flag
  fidn <- flags_df %>% filter(name %in% countryid) %>% pull(name) %>% str_sub(1,3) %>% str_to_upper()
  
  d <- clist[[countryid]] 
  d <- d %>% 
    mutate_at("Medal", funs(case_when(
      Medal == 1 ~ "Gold",
      Medal == 2 ~ "Silver",
      Medal == 3 ~ "Bronze"
    )))
  dtab <- d %>% 
    dcast(Sport~Medal, fill = 0) %>% # turn rows into cols
    melt() %>% # stack df
    arrange(desc(value)) %>% # rearrange
    tidyr::uncount(value) %>% # expand table by number of each medal 
    dplyr::select(variable,Sport) %>% # reorder for colpal
    mutate("variable" = factor(variable, levels = colv_label)) %>% arrange(variable) # order by medals
  colpal <- c(colv_df %>% filter(label %in% dtab$variable) %>% pull,rep(col_lab,dtab[,2] %>% unique %>% length)) # match colpal to data length
  
  # plot save 
  fh <- countryid
  png(here::here("plots") %>% paste0("/",lid,"_",fh,".png"),width = width, height = height, units = "cm", bg = "transparent", res = 250)
  # plot pars
  par(mar = rep(2, 4),mfrow = c(1, 1),family = "HersheySans",font = 2) # plot pars
  circos.clear()
  circos.par(start.degree = -180, # chord setup 
             gap.degree = 2, track.margin = c(-0.1, 0.1), 
             points.overflow.warning = F,
             track.height = 0.2)
  circos.par(cell.padding =c(0.02, 0, 0.02, 0))
  
  # plot 
  chordDiagram(dtab,
               grid.col = colpal,
               transparency = 0.3,
               directional = 1, # 1 = link origin is from sectors
               diffHeight  = -0.05,
               # link.border = "#FFFFFF",
               annotationTrack = c("grid"
                                   # ,"name" # to check name placment
               ),
               annotationTrackHeight = c(0.05, 0.1),
               big.gap = 5, small.gap = 2, # gaps between sectors
               link.sort = F, link.decreasing = T, # define link overlap
               link.largest.ontop = T,
               preAllocateTracks = list(track.height = 0.1)
               # symmetric = F
               # scale = F # weight links equally
  )
  
  # add labels to chord 
  ylim <- 0.85
  cex <- 0.6
  circos.track(track.index = 1,  
               panel.fun = function(x,y){ # add text labels
                 sector_index = get.cell.meta.data("sector.numeric.index")
                 circos.text(x = CELL_META$xcenter, 
                             y = ylim, 
                             # remove medal labels
                             labels =  ifelse(sector_index <= dtab[,1] %>% unique %>% length, NA, CELL_META$sector.index),
                             facing = "clockwise", niceFacing = T, cex = cex, col = col_lab
                 )}
               , bg.border = NA) # set bg to NA
  
  # add flag and flag id
  xx <- 0.2; yy <- 0.8
  grid.raster(fid, x=xx, y=yy, width=0.1, height = 0.075) # add country flag
  text(-1, yy+0.16, fidn, col = col_lab, cex = 1, pos = 4)
  dev.off() # close plot save
  
  cat(rep("\n",3),"Plot saved as",fh %>% paste0(".png in"),here::here("plots"))
  message("\n\n",countryid,appendLF = F) # display status
  flush.console()
  lid <- lid + 1
} # end loop