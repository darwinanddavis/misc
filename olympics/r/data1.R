# @knitr data1

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

