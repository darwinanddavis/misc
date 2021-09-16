# @knitr setup 

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
