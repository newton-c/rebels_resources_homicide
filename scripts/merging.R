library(haven)
library(tidyverse)
library(readxl)
library(countrycode)
library(UCDP.codes)
library(merge.stats)


# import dyadic dataset ------------------------------------------------------
dyadic <- read_csv("data/ucdp-dyadic-201.csv")

# creating an empty dyadic dataset -------------------------------------------
dyadic <- filter(dyadic, type_of_conflict != 2) # no interstate conflicts

# find all dyads with their start years
dyad <- dyadic %>%
    select(dyad_id, conflict_id, side_a, side_a_id,
        side_b, side_b_id, year, gwno_a) %>%
    group_by(dyad_id, conflict_id, side_a, side_a_id,
        side_b, side_b_id, gwno_a) %>%
    summarise(start_year = min(year))

# expand dyads for yearly obsevations from 1946-2019
dyad <- dyad %>%
    rowwise() %>%
    mutate(year = list(seq(1946, 2019))) %>%
    unnest(cols = c(year)) %>%
    arrange(dyad_id, year) %>%
    select(dyad_id, conflict_id, side_a, side_a_id,
        side_b, side_b_id, year, start_year, gwno_a) %>%
    distinct(dyad_id, conflict_id, side_a, side_a_id,
        side_b, side_b_id, year, start_year, gwno_a)

# delete obvervations that occur before a dyad starts
dyad <- dyad %>% filter(year >= start_year)
dat <- join_stats(dyadic, dyad,
                  by = c("dyad_id", "conflict_id", "side_a", "side_a_id",
                         "side_b", "side_b_id", "year", "gwno_a"),
                  join = "right")
rm("dyad", "dyadic")
dat$merge <- NULL
dat$side_b_id <- as.integer(dat$side_b_id)
dat$side_a_id <- as.integer(dat$side_a_id)
dat$gwno_a <- as.integer(dat$gwno_a)

# OSV ------------------------------------------------------------------------
osv <- read.csv("data/ucdp-onesided-201.csv")
osv <- osv[c(3, 8:12)]

osv_nsa <- osv %>%
    filter(is_government_actor == 0) %>%
    summarise(actor_id = actor_id,
              year = year,
              osv_nsa_best = best_fatality_estimate,
              osv_nsa_low = low_fatality_estimate,
              osv_nsa_high = high_fatality_estimate)

osv_gov <- osv %>%
    filter(is_government_actor == 1) %>%
    summarise(actor_id = actor_id,
              year = year,
              osv_gov_best = best_fatality_estimate,
              osv_gov_low = low_fatality_estimate,
              osv_gov_high = high_fatality_estimate)

dat <- join_stats(dat, osv_nsa, by = c("side_b_id" = "actor_id", "year"),
                  join = "left")
dat$merge <- NULL

dat <- join_stats(dat, osv_gov, by = c("side_a_id" = "actor_id", "year"),
                  join = "left")
dat$merge <- NULL
rm("osv", "osv_gov", "osv_nsa")

# BRDs -----------------------------------------------------------------------
brd <- read_csv("data/ucdp-brd-dyadic-201.csv")
brd <- brd[c(1:2, 12:15)]

dat <- join_stats(dat, brd, by = c("conflict_id", "dyad_id", "year"),
                  join = "left")
dat$merge <- NULL
rm(brd)

# Homicides ------------------------------------------------------------------
hom <- read_dta("data/undoc_hom.dta")


dat <- join_stats(dat, hom, by = c("gwno_a" = "ccode", "year"), join = "left")
dat$merge <- NULL
rm(hom)

# Economic data --------------------------------------------------------------
gdp <- read.table("data/gdpv6.txt", header = TRUE)
gdp <- gdp[-c(2, 7:8)]

dat <- join_stats(dat, gdp, by = c("gwno_a" = "statenum", "year"),
                  join = "left")
dat$merge <- NULL
rm(gdp)

# Regime data ----------------------------------------------------------------
dem <- read.csv("data/polity5.csv")
dem <- dem %>%
    filter(year > 1945)

dat <- join_stats(dat, dem, by = c("gwno_a" = "ccode", "year"), join = "left")
dat$merge <- NULL
rm(dem)

# Rebel Contraband data -------------------------------------------------------
rcd <- read_csv("data/rcd_1990-2015.csv")
row_numbers <- seq(7, 917, by = 7)

rcd_subset <- rcd[c(row_numbers)]
cols <- colnames(rcd_subset)

rcd_ids <- rcd[c(1:6)]

illicit_resources <- array()
for (i in 1:ncol(rcd_subset)) {
    for (j in 1:nrow(rcd_subset)) {
        if (rcd_subset[j, i] == 1) {
            illicit_resources[j] <- 1
        }
    }
}

rcd_sub <- cbind(rcd_ids, illicit_resources)
rcd_sub$illicit_resources <- ifelse(is.na(rcd_sub$illicit_resources), 0,
                                    rcd_sub$illicit_resources)
rcd_sub <- rcd_sub %>%
    group_by(sidebid) %>%
    summarise(illicit_resources = max(illicit_resources)) %>%
    select(sidebid, illicit_resources)

rm(rcd, rcd_ids, rcd_subset, cols, i, illicit_resources, j, row_numbers)

rcd_sub$side_b_id <- ucdp_ids(rcd_sub$sidebid, "old_id", "new_id", "actor_id")
dat <- join_stats(dat, rcd_sub, by = ("side_b_id"), join = "left")
#dat <- join_stats(dat, rcd_sub, by = ("side_b_id"), join = "inner")
dat$merge <- NULL
rm(rcd_sub)


# Conflict and peace variables -----------------------------------------------
dat$conflict <- ifelse(is.na(dat$intensity_level), 0, 1)
dat$no_conflict <- ifelse(dat$conflict == 1, 0, 1)
dat$war <- ifelse(is.na(dat$intensity_level), 0,
                  ifelse(dat$intensity_level == 1, 0, 1))
dat$no_war <- ifelse(dat$war == 1, 0, 1)
#write_csv(dat, "data/dyadic14Oct2020.csv")

# Adding conflict variables to post conflict ---------------------------------
# Finding the last year of conflict for each dyad
dat.last_conflict <- dat %>% group_by(dyad_id) %>%
    arrange(year) %>%
    filter(conflict == 1) %>%
    mutate(last_year_conflict = max(year)) %>%
    select(dyad_id, last_year_conflict) %>%
    unique()

# finding the last year of war for each dyad
dat.last_war <- dat %>% group_by(dyad_id) %>%
    arrange(year) %>%
    filter(war == 1) %>%
    mutate(last_year_war = max(year)) %>%
    select(dyad_id, last_year_war) %>%
    unique()

dat.pc <- left_join(dat, dat.last_war, by = c("dyad_id"))
dat.pc <- left_join(dat.pc, dat.last_conflict, by = c("dyad_id"))
rm(dat.last_war, dat)

# accounting for all periods without conflict


# Filling in NAs with 0 for relevant variables
dat.pc <- dat.pc %>%
    replace_na(list("osv_nsa_best" = 0,
                    "osv_nsa_low" = 0,
                    "osv_nsa_high" = 0,
                    "osv_gov_best" = 0,
                    "osv_gov_low" = 0,
                    "osv_gov_high" = 0,
                    "bd_best" = 0,
                    "bd_low" = 0,
                    "bd_high" = 0))


# sums of conflict variables for postconflict analysis
dat.vars <- dat.pc %>% group_by(dyad_id) %>%
    arrange(year) %>%
    mutate(total_osv_nsa = sum(osv_nsa_best),
           total_osv_gov = sum(osv_gov_best),
           total_osv = (total_osv_nsa + total_osv_gov),
           total_brds = sum(bd_best),
           total_violence = (total_osv + total_brds)) %>%
    filter(year > last_year_conflict) %>%
    select(dyad_id, year, total_osv_nsa, total_osv_gov, total_osv, total_brds,
           total_violence)

dat.pc_vars <- left_join(dat.pc, dat.vars, by = c("dyad_id", "year"))
rm(dat.vars, dat.pc, dat.last_conflict)

# relative political capacity variables ---------------------------------------
rpe <- read_xlsx("data/RPC2015.xlsx")
rpe <- rpe[-c(1:2, 4:5)]

dat.rpe <- join_stats(dat.pc_vars, rpe, by = c("gwno_a" = "cowcode", "year"),
                      join = "left")
dat.rpe$merge <- NULL
rm(dat.pc_vars, rpe)

# inequality ------------------------------------------------------------------
#https://data.worldbank.org/indicator/SI.DST.10TH.10

inequality <- read.csv("data/API_SI.DST.10TH.10_DS2_en_csv_v2_2060766.csv")
inequality <- inequality %>%
    gather(key = "year", value = "inequality", X1960:X2020)
inequality$year <- as.numeric(gsub( "X", "", as.character(inequality$year)))

inequality$gwno_a <- countrycode(inequality$Country.Code, "wb", "gwn")
inequality <- inequality %>%
    filter(!is.na(gwno_a)) %>%
    select(gwno_a, year, inequality)

dat.ineq <- join_stats(dat.rpe, inequality,
                       by = c("gwno_a", "year"), join = "left")

dat <- dat.ineq
save(dat, file = "data/data25Mar2020.Rdata")
