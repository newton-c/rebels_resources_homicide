library(countrycode)
library(tidyverse)
library(Amelia)
library(haven)
library(readxl)
library(rstanarm)
library(WDI)

select <- dplyr::select

set.seed(977634)

load(unzip("new_data/ged211-RData.zip"))


ged <- GEDEvent_v21_1 %>%
    filter(type_of_violence != 1) %>%
    select(year, active_year, conflict_new_id, conflict_name, dyad_new_id,
           dyad_name, side_a, side_b, country, region, best) %>%
    group_by(year, active_year, conflict_new_id, conflict_name, dyad_new_id,
             dyad_name, side_a, side_b, country, region) %>%
    summarize(deaths = sum(best)) %>%
    mutate(cumulative_deaths = cumsum(deaths),
           total_active_years = cumsum( active_year))

# Child sodlier
#cs <- read_dta("data/child_soldiers.dta")
load("new_data/Replication_Forced.RData")
cs <- table
rm("table")

dyad_years <- cs %>%
    select(dyadid, endyear, ccodecow, Csdum, Csindex, nr_anystrategy,
           duration, bdeaths) %>%
    group_by(dyadid, endyear, ccodecow) %>%
    summarise(Csdum = max(Csdum), Csindex = max(Csindex),
              resources = max(nr_anystrategy),
              duration = max(duration),
              battle_deaths = cumsum(bdeaths)) %>%
    rowwise() %>%
    mutate(year = list(seq(1990, 2020, by = 1))) %>%
    unnest(cols = c(year)) %>%
    ungroup() 

# Homicide data
homc <- read_xlsx("new_data/homicide_country_download.xlsx") %>%
    filter(Gender == "Total (all ages)",
           Indicator == "Homicide: # of victims",
           Unit == "Count") %>%
    rename(year = Year, hom_count = Value) %>%
    select(country, year, hom_count)

homr <- read_xlsx("new_data/homicide_country_download.xlsx") %>%
    filter(Gender == "Total (all ages)",
           Indicator == "Homicide: # of victims",
           Unit != "Count") %>%
    rename(year = Year, hom_rate = Value) %>%
    select(country, year, hom_rate)

hom <- inner_join(homr, homc)
hom$ccode <- countrycode(sourcevar = hom$country, origin = "country.name",
                         destination = "cown")
hom <- filter(hom, !is.na(ccode))

dat <- left_join(dyad_years, hom, by = c("ccodecow" = "ccode", "year" = "year"))
dat <- dat %>%
    mutate(hom_count = as.numeric(hom_count),
           hom_rate = as.numeric(hom_rate),
           postcon = ifelse(year >= endyear, 1, 0))

#wbi <- WDI(indicator = c("SE.XPD.TOTL.GD.ZS", "EN.POP.SLUM.UR.ZS",
#                         "SP.POP.TOTL"), start = 1990, end = 2022)
wbi2 <- WDI(indicator = c("SE.XPD.TOTL.GD.ZS", "SP.POP.TOTL", "EN.POP.SLUM.UR.ZS"),
           start = 1990, end = 2022)
wbi2$ccodecow <- countrycode(sourcevar = wbi2$country, origin = "country.name",
                         destination = "cown")
#wbi <- filter(wbi, !is.na(ccodecow)) %>%
#       mutate(pop_slums = EN.POP.SLUM.UR.ZS,
#              education = SE.XPD.TOTL.GD.ZS) %>%
#       select(year, ccodecow, pop_slums, education)

#wbi2 <- filter(wbi2, !is.na(ccodecow)) %>%
#  mutate(education = SE.XPD.TOTL.GD.ZS,
#         log_pop = log(SP.POP.TOTL),
#         pop_slums = EN.POP.SLUM.UR.ZS) %>%
#  select(year, ccodecow, education, log_pop, pop_slums)

#csdat <- left_join(dat, wbi2)

#a.out <- amelia(data.frame(csdat), m = 5, idvars = c("dyadid", "endyear", "hom_rate",
#                                         "hom_count", "country", "postcon"), 
#                ts = "year", cs = "ccodecow")

wbi2 <- filter(wbi2, !is.na(ccodecow)) %>%
  mutate(education = SE.XPD.TOTL.GD.ZS,
         log_pop = log(SP.POP.TOTL),
         pop_slums = EN.POP.SLUM.UR.ZS) %>%
  select(year, ccodecow, education, log_pop, pop_slums)

csdat <- left_join(dat, wbi2)

pko <- read_xlsx("new_data/mission-month_12-2019.xlsx") %>%
  filter(missionccode >= 0 & troop >= 0 & police >= 0) %>%
  group_by(missionccode, year) %>%
  summarise(troop = sum(troop, na.rm = TRUE),
            police = sum(police, na.rm = TRUE))

rrh_data <- left_join(csdat, pko,
                      by = c('ccodecow' = 'missionccode', 'year' = 'year')) 
rrh_data <- rrh_data %>%
  mutate(troop = ifelse(rrh_data$year >= min(pko$year) &
                          rrh_data$year <= max(pko$year) &
                          is.na(rrh_data$troop), 0, rrh_data$troop),
         police = ifelse(rrh_data$year >= min(pko$year) &
                          rrh_data$year <= max(pko$year) &
                          is.na(rrh_data$police), 0, rrh_data$police))

a.out <- amelia(data.frame(rrh_data), m = 5,
                idvars = c("dyadid", "endyear", "hom_rate", "hom_count",
                           "country", "postcon", "troop", "police"), 
                ts = "year", cs = "ccodecow")

theme_set(theme_bw())
#overdisp <- ggplot(data = subset(csdat2, postcon == 1)) +
overdisp <- ggplot(data = subset(csdat, postcon == 1)) +
  geom_histogram(aes(x = hom_count)) +
  labs(title = 'Post-Conflict Homicides', subtitle = '1990 — 2020') +
  ylab('Count') +
  xlab('') +
  theme(title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"))

#ggsave(plot = overdisp, filename = 'figures/oversidpersion.png',
#       height = 4, width = 8)