library(Amelia)
library(tidyverse)
library(modelsummary)
library(Zelig)
library(rethinking)

map <- rethinking::map

load("data/data25Mar2020.Rdata")
dat <- dat %>%
    filter(year > last_year_conflict & !is.na(illicit_resources)) %>%
    mutate(time = year - last_year_conflict,
        time2 = time**2,
        time3 = time**3,
        gwno_a = as.factor(gwno_a),
         #  year = as.ordered(year),
        rgdppc_s = (rgdppc - mean(rgdppc, na.rm = TRUE)) /
            sd(rgdppc, na.rm = TRUE),
        polity2_s = (polity2 - mean(polity2, na.rm = TRUE)) /
            sd(polity2, na.rm = TRUE),
        total_violence_s = (total_violence - mean(total_violence)) /
            sd(total_violence),
        pop_s = (pop - mean(pop, na.rm = TRUE)) /  sd(pop, na.rm = TRUE),
        rpe_gdp_s = (rpe_gdp - mean(rpe_gdp, na.rm = TRUE)) /
            sd(rpe_gdp, na.rm = TRUE),
        inequality_s = (inequality - mean(inequality, na.rm = TRUE)) /
            sd(inequality, na.rm = TRUE),
        hom_count_s = (hom_count - mean(hom_count, na.rm = TRUE)) /
            sd(hom_count, na.rm = TRUE),
        hom_rate_s = (hom_rate - mean(hom_rate, na.rm = TRUE)) /
            sd(hom_rate, na.rm = TRUE)
        ) %>%
    select(year, gwno_a, conflict_id, hom_rate, hom_count, rgdppc_s,
        polity2_s, total_violence_s, pop_s, illicit_resources, rpe_gdp_s, time,
        time2, time3, side_a, side_b, inequality_s, hom_rate_s) %>%
    replace_na(list(total_violence = 0))

dat.df <- as.data.frame(dat)


a.out <- amelia(dat.df, m = 5, ts = "year", cs = "gwno_a",
    idvars = c("side_a", "side_b", "hom_rate", "hom_count"))

a_1 <- a.out$imputations$imp1
a_1 <- filter(a_1, !is.na(hom_rate))


a_1b <- transform(a_1, id=as.numeric(factor(gwno_a)))
a_1b$hom_count_log <- ifelse(a_1b$hom_count == 0, 0, log(a_1b$hom_count))
a_1b$irxtime <- a_1b$illicit_resources * a_1b$time
a_1c <- filter(a_1, time <= 10)
a_1c <- transform(a_1c, id=as.numeric(factor(gwno_a)))

m1 <- map2stan(
    alist(
        hom_rate_s ~ dnorm(mu, sigma),
        mu <- a_society[id] + bir * illicit_resources + bpre * rpe_gdp_s +
            btv * total_violence_s,
        a ~ dnorm(0, 10),
        bir ~ dnorm(0, 10),
        bpre ~ dnorm(0, 10),
        btv ~ dnorm(0, 10),
        a_society[id] ~ dnorm(0, sigma_society),
        sigma ~ dcauchy(0, 2),
        sigma_society ~ dcauchy(0,2)
    ), data=a_1c ,iter = 2000 , chains = 2, cores = 2 )
precis(m1)
