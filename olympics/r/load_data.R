# @knitr load_data

elist <- here::here("data","medals_event.Rda") %>% readRDS() # load tally by event 
clist <- here::here("data","medals_country.Rda") %>% readRDS() # load tally by country
flags_df <- here::here("data","flags_df.Rda") %>% readRDS() # hi-res country flags
colv_label <- c("Gold","Silver","Bronze") # col labels
colv_pal <- c("#C09F68","#C5C3C3","#AA7C64") # col hex
colv_df <- tibble("label" = colv_label,"col" = colv_pal)