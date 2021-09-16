# @knitr webscrape

# webscrape funcs ------------------------------------------------
get_webdata <- function(att) event_url %>% read_html() %>% html_nodes(".dropdown-link") %>% html_attr(att) %>% return() # pull webdata func
get_pictogram <- function() pictogram_url %>% read_html() %>% html_nodes(".j-module") # pictogram text and img func 
get_flag <- function() flag_url %>% read_html() %>% html_nodes(".thumb") # get flag func
img_convert <- function(img){ # add raster img as chord labels
  imgr <- img %>% magick::image_read() %>% as.raster() # convert img to raster layer
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