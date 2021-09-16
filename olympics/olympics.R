# tokyo olympics 2020 dataviz 
# matt malishev
# @darwinanddavis

# pcks ----------------------------------------------------------
pacman::p_load(here,rvest,xml2,dplyr,circlize,tidyr,stringr,purrr,magick,reshape2)

# enter vars ----------------------------------------------------
country <- "china" # choose country
event <- "judo" # or choose event 
by_event <- T # switch between medals by event or country

# source data ---------------------------------------------------
flags_df <- here::here("r","flags_df.Rda") %>% readRDS()

# vars ----------------------------------------------------------
base_url <- "https://olympics.com/tokyo-2020/olympic-games"
event_url <- paste0(base_url,"/en/results/all-sports/medal-standings.htm")
pictogram_url <- "https://www.theolympicdesign.com/olympic-design/pictograms/tokyo-2020/"
flag_url <- "https://www.countryflags.com/icons-overview/"
col_lab <- "#434343" # sector colour
colv_label <- c("Gold","Silver","Bronze") # col labels
colv_pal <- c("#C09F68","#C5C3C3","#AA7C64") # col hex
colv_df <- tibble("label" = colv_label,"col" = colv_pal)
height <- 10; width <- height # plot dims

# webscrape funcs ------------------------------------------------
get_webdata <- function(att) event_url %>% read_html() %>% html_nodes(".dropdown-link") %>% html_attr(att) %>% return() # pull webdata func
get_pictogram <- function() pictogram_url %>% read_html() %>% html_nodes(".j-module") # pictogram text and img func 
get_flag <- function() flag_url %>% read_html() %>% html_nodes(".thumb") # get flag func
img_convert <- function(img){ # add raster img as chord labels
  imgr <- img %>% magick::image_read() %>% as.raster() # convert img to raster layer
  imgr[imgr == "#002163ff"] <- col_lab # change main img color
  imgr %>% return()} 

# webscrape data ------------------------------------------------
# get all countries 
country_names <- event_url %>% read_html() %>% 
  html_table() %>% .[[1]] %>%
  pull("Team/NOC")

# get urls for each country 
get_country_data <- function(){
  event_url %>% read_html() %>% 
  html_node("table") %>% # get first table
  html_nodes(".playerTag") %>% 
  html_node("a")
}

# full country names
country_total <- get_country_data() %>% 
  html_attr("href") %>% 
  str_replace_all("entries","medalist-by-sport") %>% # get 'by sport' links
  str_replace_all("../../..",base_url) 

# country url titles
country_title <- get_country_data() %>% 
  html_attr("title") %>% 
  str_remove_all("NOC Entries-") %>% 
  str_replace_all(" ","-") %>% str_to_lower()
  
# get all events 
event_title <-  event_url %>% read_html() %>% 
  html_nodes(".dropdown-link") %>% 
  html_text(trim = T)

# get urls for each event 
event_total <- get_webdata("href") %>% 
  str_subset("results") %>% 
  str_replace_all("../../..",base_url)

# pictogram text
pictogram_text <- get_pictogram() %>% 
  html_nodes("span") %>% html_text() %>% unique
# hi-res event pictograms 
pictogram_total <- get_pictogram() %>% 
  html_nodes("a") %>% html_attr("data-href") %>% na.omit() %>% .[-1] # remove first header img
# pictogram id
pictogram_id <- get_pictogram() %>% 
  html_nodes("img") %>% html_attr("data-image-id")
# final pictogram df
pictogram_df <- tibble("event" = pictogram_text,
                       "img" = pictogram_total)

# hi-res flags
flags_df <- tibble("name" = get_flag() %>% html_nodes("span") %>% html_text() %>% str_remove_all(" flag icon") %>% str_replace_all(" ","-") %>% str_to_lower(),
                   "flag" = get_flag() %>% html_nodes("img") %>% html_attr("src"))

# medal data by event (table) ----------------------------------------
if(by_event){
  url <- event_total %>% str_subset(event) # find event url
  d <- url %>% 
    read_html() %>% 
    html_table(trim = T) %>% .[[1]] %>% # get first table
    rename(.cols = 2:5, # rename cols 
           "Country" = 2,
           "Gold" = 3,
           "Silver" = 4,
           "Bronze" = 5) %>% 
    mutate("Event" = event %>% str_to_sentence())
  dtab <- d %>% select(NOCCode,Gold,Silver,Bronze) %>% 
    melt() %>%
    tidyr::uncount(value) %>% # expand table by number of each medal 
    select(variable,NOCCode) # reorder for colpal
  colpal <- c(colv_pal,rep(col_lab,dtab[,2] %>% unique %>% length)) # match colpal to data length
  }else{
  # medal data by country --------------------------------------------
  # medal count 
  countryid <- country %>% str_to_lower() %>% str_replace_all(" ","-")
  url <- country_total %>% str_subset(countryid) # pull url from url stack
  mc <- url %>% read_html %>% html_nodes(".medal-icon") %>% html_attr("alt") %>% as.numeric() # get medal count
  # count by country
  d <- url %>% read_html() %>% 
    html_table(trim = T) %>% .[[1]] %>% 
    replace("Medal", mc) %>%
    mutate("Country" = country) %>% 
    select(Medal,Sport) %>% 
    group_by(Sport) %>%
    count(Medal) %>% 
    arrange(n)
  dtab <- d %>% select(Medal,Sport) %>% table()
  row.names(dtab) <- colv_label[1:nrow(dtab)] # match row names to no. of rows
  colpal <- c(colv_pal[1:nrow(dtab)],rep(col_lab,dtab %>% dim %>% max)) # set colpal matching no. of rows/events
} 
dtab

# plot ----------------------------------------------------------

# select either country or event data 
elist <- here::here("r","medals_event.Rda") %>% readRDS()
clist <- here::here("r","medals_country.Rda") %>% readRDS()
event <- "Diving"
country <- "Brazil"
d <- elist[[event]]
d <- clist[[country_title %>% str_subset(country %>% str_to_lower()) %>% .[1]]]
by_event <- T # by event or country?

# create table from data
if(by_event){ # use event data
  dtab <- d %>% select(NOCCode,Gold,Silver,Bronze) %>% 
    melt() %>%
    tidyr::uncount(value) %>% # expand table by number of each medal 
    select(variable,NOCCode) # reorder for colpal
  colpal <- c(colv_pal,rep(col_lab,dtab$NOCCode %>% unique %>% length)) # match colpal to data length
}else{ # use country data 
  d <- d %>% 
    mutate_at("Medal", funs(case_when(
      Medal == 1 ~ "Gold",
      Medal == 2 ~ "Silver",
      Medal == 3 ~ "Bronze"
    )))
  dtab <- d %>% 
    dcast(Sport~Medal, fill = 0) %>% 
    melt() %>%
    arrange(desc(value)) %>%
    uncount(value) %>% # expand table by number of each medal 
    select(variable,Sport) %>% # reorder for colpal
    mutate("variable" = factor(variable, levels = colv_label)) %>% arrange(variable) # order by medals
  colpal <- c(colv_df %>% filter(label %in% dtab$variable) %>% pull,rep(col_lab,dtab[,2] %>% unique %>% length)) # match colpal to data length
}

require(circlize)
# plot save 
fh <- ifelse(by_event, fh <- event, fh <- country)
png(here::here("plots") %>% paste0("/",fh,".png"),width = width, height = height, units = "cm", bg = "transparent", res = 250)

circos.clear()
par(mar = rep(2, 4),mfrow = c(1, 1),family = "HersheySans",font = 2) # plot pars
circos.par(start.degree = -185, # chord setup 
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
             link.sort = T, link.decreasing = T, # define link overlap
             link.largest.ontop = T,
             preAllocateTracks = list(track.height = 0.1)
             # symmetric = F
             # scale = F # weight links equally
)

# set custom circ labels
ylim <- 1
circos.track(track.index = 1, 
             panel.fun = function(x, y){
               # circos.raster(image = imgr, # add image
               #               x = CELL_META$xcenter,
               #               y = ylim,
               #               width = "1.5mm", height = "1.5mm",
               #               facing = "downward")
               circos.text(x = CELL_META$xcenter, # add text
                           y = ylim, 
                           labels =  CELL_META$sector.index,
                           facing = "clockwise", niceFacing = T,
                           cex = 0.8, col = col_lab,
                           adj = c(0.2, 0) # label xy position
                           ### for image and text
                           # cex = 0.5, col = col_lab,
                           # adj = c(0, 0.5) # label xy position
               )}
             , bg.border = NA) # set bg to NA

dev.off() # close plot save
cat(rep("\n",3),"Plot saved as",fh %>% paste0(".png in"),here::here())

# save country/event data to dir  -------------------------------
# all country data
clist <- list() # store country medal data 
for(cn in seq_along(country_title)){
  url <- country_total[cn]
  mc <- url %>% read_html %>% html_nodes(".medal-icon") %>% html_attr("alt") %>% as.numeric() # get medal count
  d <- url %>% read_html() %>% 
    html_table(trim = T) %>% .[[1]] %>% 
    replace("Medal", mc) %>%
    mutate("Country" = country) %>% 
    select(Medal,Sport) %>% 
    group_by(Sport) %>%
    count(Medal) %>% 
    arrange(n)
  ll <- list(d) # save df 
  nn <- country_title[cn] # name each entry
  names(ll) <- nn
  clist <- c(clist,ll)
  message(nn,rep(" ",10),"\r",appendLF = F) # display status
  flush.console()
}
saveRDS(clist,here::here("r","medals_country.Rda"))

# all event data
elist <- list() # store event medal data 
event_title <- event_title[1:47] # retain just sports
for(cn in seq_along(event_title)){
  url <- event_total[cn]
  d <- url %>% 
    read_html() %>% 
    html_table(trim = T) %>% .[[1]] %>% # get first table
    rename(.cols = 2:5, # rename cols 
           "Country" = 2,
           "Gold" = 3,
           "Silver" = 4,
           "Bronze" = 5) %>% 
    mutate("Event" = event_title[cn] %>% str_to_sentence())
  ll <- list(d) # save df 
  nn <- event_title[cn] # name each entry
  names(ll) <- nn
  elist <- c(elist,ll) 
  message(nn,rep(" ",10),"\r",appendLF = F) # display status
  flush.console()
}
saveRDS(elist,here::here("r","medals_event.Rda"))
