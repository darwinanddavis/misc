# tokyo olympics 2020 dataviz 
# matt malishev
# @darwinanddavis

# pcks ----------------------------------------------------------
pacman::p_load(rvest,xml2,dplyr,here)

# get country flags ---------------------------------------------
flag_url <- "https://www.countryflags.com/icons-overview/"
get_flag <- function() flag_url %>% read_html() %>% html_nodes(".thumb") 
flags_df <- tibble("name" = get_flag() %>% html_nodes("span") %>% html_text() %>% str_remove_all(" flag icon") %>% str_replace_all(" ","-") %>% str_to_lower(),
                   "flag" = get_flag() %>% html_nodes("img") %>% html_attr("src"))

# check country name
flags_df %>%
  filter(name %in% str_subset(name,"syr")) %>% 
  pull(name)

# add missing countries -----------------------------------------
clist_name <- c(
  "czech-republic,-the,",
  "dominican-republic",
  "iran",
  "côte-d’-ivoire",
  "south-korea",
  "moldova",
  "philippines,-the,",
  "russia",
  "syria",
  "united-kingdom",
  "hong-kong") 
clist_rename <- c(
  "czech-republic", # rename fid list to match clist
  "dominican-rep.",
  "islamic-rep.-of-iran",
  "côte-d'ivoire",
  "republic-of-korea",
  "rep.-of-moldova",
  "philippines",
  "roc",
  "syrian-arab-rep.",
  "great-britain",
  "hong-kong,-china") 
flags_df[flags_df$name %in% clist_name,"name"] <- clist_rename
flags_df <- flags_df %>% 
  add_row("name" = c("chinese-taipei","bermuda"),
          "flag" = c("https://cdn.countryflags.com/thumbs/china/flag-square-250.png",
                     "https://cdn.countryflags.com/thumbs/united-kingdom/flag-square-250.png")
  )
flags_df <- flags_df %>% filter(name %in% names(clist)) # pull final olympic names
saveRDS(flags_df,here::here("r","flags_df.Rda")) # save
