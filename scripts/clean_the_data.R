library(tidyverse)
library(merge.stats)
library(UCDP.codes)
library(countrycode)


# get a df with all unique conflict and dyad ids
conflicts_and_dyads <- ged_data %>%
    select(conflict_new_id, dyad_new_id, dyad_name) %>%
    distinct


# convert the old UCDP dyad ids used in the RCD to the new ids in a new column
rcd_data$dyad_new_id <- NA
rcd_data$dyad_new_id <- ucdp_ids(rcd_data$dyadid, "old_id", "new_id", "dyad_id")


# merge the RCD with the conflict/dyad id df to merge based on conflict id
rcd_dyad <- join_stats(rcd_data, conflicts_and_dyads,
                       by = c("dyad_new_id"), join = "left")


# extract the needed columns from RCD, replace missing values with 0
rcd_binary <- rcd_dyad %>%
    select(conflict_new_id,
           grep("extortion$", colnames(rcd_dyad), value = T),
           grep("theft$", colnames(rcd_dyad), value = T),
           grep("smuggling$", colnames(rcd_dyad), value = T),
           grep("smugglinghuman$", colnames(rcd_dyad), value = T),
           grep("smugglingother$", colnames(rcd_dyad), value = T),
           grep("bf$", colnames(rcd_dyad), value = T),
           grep("kidnap$", colnames(rcd_dyad), value = T),
           grep("kidnapintl$", colnames(rcd_dyad), value = T),
           grep("other$", colnames(rcd_dyad), value = T),
           grep("piracy$", colnames(rcd_dyad), value = T),
           grep("humanitarianaid$", colnames(rcd_dyad), value = T)) %>%
    replace(is.na(.), 0) #%>%

rcd_binary$illicit_resources <- 0 # initialise at 0 to avoid NAs


# if any illicit resource is 1 for a row, make illicit_resources 1
for (i in seq_len(nrow(rcd_binary))) {
    for (j in seq_along(rcd_binary)) {
        if (rcd_binary[i, j] == 1) {
            rcd_binary$illicit_resources[i] <- 1
        }
    }
}


# collapse RCD to conflict level, only keep conflict_id and illicit_resources
# if any resources are used in conflict, illicit_resources = 1
rcd_illicit <- rcd_binary %>%
    select(conflict_new_id, illicit_resources) %>%
    group_by(conflict_new_id) %>%
    summarise(illicit_resources = max(illicit_resources)) %>%
    ungroup


# collapse GED to conflict-year level
ged_conflict1 <- ged_data %>%
    group_by(conflict_new_id, year) %>%
    summarise(deaths_a = sum(deaths_a),
              deaths_b = sum(deaths_b),
              deaths_civilians = sum(deaths_civilians)) %>%
    select(conflict_new_id, year, deaths_a, deaths_b, deaths_civilians) %>%
    ungroup

ged_country_ids <- ged_data %>%
    select(conflict_new_id, country_id) %>%
    distinct

ged_conflict <- join_stats(ged_conflict1, ged_country_ids,
                           by = "conflict_new_id", join = "left")
ged_conflict$merge <- NULL

# create conflict-years for postconflict periods
conflict_years <- ged_conflict %>%
    select(conflict_new_id, year) %>%
    group_by(conflict_new_id) %>%
    filter(year == min(year)) %>%
    rowwise %>%
    mutate(year = list(seq(from = year, to = 2021))) %>%
    unnest(cols = c(year)) %>%
    ungroup

# 6,260 obs from GED, 17,124 obs added, 23,384 obs total
ged_expanded <- join_stats(conflict_years, ged_conflict,
                           by = c("conflict_new_id", "year"), join = "full")
ged_expanded$merge <- NULL
print("line 92")
rm(conflict_years, ged_conflict)


# merge cleaned RCD and GED datasets
ged_rcd <- join_stats(ged_expanded, rcd_illicit, by = "conflict_new_id",
                      join = "full")

print("line 99")

# creating conflict thresholds
ged_rcd <- ged_rcd %>%
    mutate(deaths_a = ifelse(is.na(deaths_a), 0, deaths_a),
           deaths_b = ifelse(is.na(deaths_b), 0, deaths_b),
           deaths_civilians = ifelse(is.na(deaths_civilians), 0,
                                     deaths_civilians),
           over_25_brds = ifelse(deaths_a + deaths_b >= 25, 1, 0),
           over_100_brds = ifelse(deaths_a + deaths_b >= 100, 1, 0),
           over_1000_brds = ifelse(deaths_a + deaths_b >= 1000, 1, 0)
    )
ged_rcd$merge <- NULL
print("line 112")
rm(ged_expanded)


# Generating conflict lags for calculating postconflict periods
conflict_lags <- ged_rcd %>%
    select(conflict_new_id, year, over_25_brds, over_100_brds,
           over_1000_brds) %>%
    mutate(year = year + 1) %>%
    rename_with(~ gsub("brds", "brds_t1", .x))
print("line 121")

ged_rcd_lags <- dplyr::left_join(ged_rcd, conflict_lags,
                           by = c("conflict_new_id", "year")) %>%
                filter(year < 2022)
print("line 123")
rm(ged_rcd, conflict_lags)

# Generating postconflict variables
ged_rcd_postcon <- ged_rcd_lags %>%
    mutate(post_25_brds = ifelse(over_25_brds == 0 &
                                 over_25_brds_t1 == 1, 1, 0),
           post_100_brds = ifelse(over_100_brds == 0 &
                                  over_100_brds_t1 == 1, 1, 0),
           post_1000_brds = ifelse(over_1000_brds == 0 &
                                   over_1000_brds_t1 == 1, 1, 0)
    )
print("line 139")
rm(ged_rcd_lags)

ged_rcd_postcon2 <- ged_rcd_postcon %>%
    mutate(post_25_brds = replace_na(post_25_brds, 0),
           post_100_brds = replace_na(post_100_brds, 0),
           post_1000_brds = replace_na(post_1000_brds, 0)
    )


print("line 148")

ged_rcd_postcon3 <- ged_rcd_postcon2 %>%
    group_by(conflict_new_id, post_25_episode) %>%
    mutate(post_25_duration =
               ifelse(over_25_brds == 0 & post_25_episode > 0,
                      seq(from = 1, to = n), 0)) %>%
    ungroup %>%
    group_by(conflict_new_id, post_100_episode) %>%
    mutate(post_100_duration =
               ifelse(over_100_brds == 0 & post_100_episode > 0,
                      seq(from = 1, to = n), 0)) %>%
    ungroup %>%
    group_by(conflict_new_id, post_1000_episode) %>%
    mutate(post_1000_duration =
               ifelse(over_1000_brds == 0 & post_1000_episode > 0,
                      seq(from = 1, to = n), 0)) %>%
    ungroup

ged_rcd_postcon3$merge <- NULL
print("line 161")
rm(ged_rcd_postcon, ged_rcd_postcon2)

# Adding in World Bank data
wb_data$country_id <- countrycode(wb_data$iso2c, origin = "iso2c",
                                  destination = "gwn")

main_df <- join_stats(ged_rcd_postcon3, wb_data, by = c("country_id", "year"),
                      join = "left")
rm(ged_postcon3, wb_data)
