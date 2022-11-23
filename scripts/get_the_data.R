library(tidyverse)
library(WDI)

library(httr)
library(jsonlite)

# World Bank data --------------------------------------------------------------
wb_data <- WDI(indicator = c("hom_rate" = "VC.IHR.PSRC.P5",
                             "edu_expend" = "SE.XPD.TOTL.GD.ZS",
                             "pop_urban" = "SP.URB.TOTL.IN.ZS",
                             "unemployment" = "SL.UEM.TOTL.ZS",
                             "pop_slums" = "EN.POP.SLUM.UR.ZS",
                             "statistical_capacity" = "IQ.SCI.OVRL"),
                   start = 1960, end = 2018)

# UCDP data --------------------------------------------------------------------
# https://ucdp.uu.se/apidocs/
ged_urls <- function(i) {
    urls <- list()
    page_num <- i - 1 # UCDP API begins index at 0, R begins at 1
    urls[[i]] <- paste0("https://ucdpapi.pcr.uu.se/api/gedevents/",
                        "21.1?pagesize=1000&page=", page_num)
}


get_ged_json <- function(i) {
    ged_pages <- list()
    ged_pages[[i]] <- httr::GET(url_list[[i]])
}


convert_json <- function(i) {
    data_list <- list()
    data_list <- jsonlite::fromJSON(rawToChar(ged_json[[i]]$content))$Result
}


pages_to_dataframe <- function() {
    new_df <- rbind(ged_data_list[[1]], ged_data_list[[2]])
    for (i in 3:262) {  # 1st 2 pages used to create base dataframe, start at 3
        new_df <- rbind(new_df, ged_data_list[[i]])
    }
    return(new_df)
}


num_pages <- seq(from = 1, to = 262, by = 1)    # download all 262 pages of JSON
url_list <- lapply(num_pages, ged_urls)
ged_json <- lapply(num_pages, get_ged_json)
ged_data_list <- lapply(num_pages, convert_json)
ged_data <- pages_to_dataframe()

# Rebel Contraband data --------------------------------------------------------
rcd_url <- paste0("https://dataverse.harvard.edu/api/access/datafile/",
                  ":persistentId?persistentId=doi:10.7910/DVN/COQ65B/LGZXWC")

rcd_raw <- GET(rcd_url)
rcd_data <- read_tsv(rawToChar(rcd_raw$content))
