# @knitr data3

# hi-res flags
flags_df <- tibble("name" = get_flag() %>% html_nodes("span") %>% html_text() %>% str_remove_all(" flag icon") %>% str_replace_all(" ","-") %>% str_to_lower(),
                   "flag" = get_flag() %>% html_nodes("img") %>% html_attr("src"))
saveRDS(flags_df,here::here("r","flags_df.Rda"))
