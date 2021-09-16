# @knitr data2

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
