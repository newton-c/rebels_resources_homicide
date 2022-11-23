library(Amelia)
library(tidyverse)
library(modelsummary)
library(Zelig)

#load("data/data25Mar2020.Rdata")
dat <- main_data
dat <- dat %>%
    filter(year > last_year_conflict & !is.na(illicit_resources)) %>%
    mutate(time = year - last_year_conflict,
           time2 = time**2,
           time3 = time**3,
           gwno_a = as.factor(gwno_a),
         #  year = as.ordered(year),
           rgdppc.s = (rgdppc - mean(rgdppc, na.rm = TRUE)) /
                       sd(rgdppc, na.rm = TRUE),
           polity2.s = (polity2 - mean(polity2, na.rm = TRUE)) /
                        sd(polity2, na.rm = TRUE),
           total_violence.s = (total_violence - mean(total_violence)) /
                              sd(total_violence),
           pop.log = log(pop),
           pop.s = (pop - mean(pop, na.rm = TRUE)) /  sd(pop, na.rm = TRUE),
           rpe_gdp.s = (rpe_gdp - mean(rpe_gdp, na.rm = TRUE)) /
                        sd(rpe_gdp, na.rm = TRUE),
           inequality.s = (inequality - mean(inequality, na.rm = TRUE)) /
                          sd(inequality, na.rm = TRUE)
         ) %>%
    select(year, gwno_a, conflict_id, hom_rate, hom_count, rgdppc.s, polity2.s,
           total_violence.s, pop.s, pop.log, illicit_resources, rpe_gdp.s, time, time2, time3,
           side_a, side_b, inequality.s) %>%
    replace_na(list(total_violence = 0))

dat.df <- as.data.frame(dat)

bounds <- matrix(c(5, 0, 60000), nrow = 1, ncol = 3)
a.out <- amelia(dat.df, m = 5, ts = "year", cs = "gwno_a",
                idvars = c("side_a", "side_b", "hom_rate"), bounds = bounds)
#a.out <- amelia(dat.df, m = 5, ts = "year", cs = "gwno_a",
#   idvars = c("side_a", "side_b", "hom_rate", "hom_count"))



cn = c(
       'illicit_resources' = 'Illicit Funding',
       'total_violence.s' = 'Total Violence',
       'pop.s' = 'Population',
       "pop.log" = "Population (log)",
       'rpe_gdp.s' = 'Relative Political Extraction',
       'inequality.s' = 'Inequality',
       'polity2.s' = 'Polity',
       'time' = 'Time',
       'time2' = 'Time^2',
       'time3' = 'Time^3',
       '(Intercept)' = 'Constant')



       models = list(
                             `Amelia 1` = lm(hom_rate ~ illicit_resources +
                                                  total_violence.s + rpe_gdp.s + polity2.s +
                                                  inequality.s + time + time2 + time3,
                                                  data = a.out$imputations$imp1),
                             `Amelia 2` = lm(hom_rate ~ illicit_resources +
                                                  total_violence.s + rpe_gdp.s + polity2.s +
                                                  inequality.s + time + time2 + time3,
                                                data = a.out$imputations$imp2),
                             `Amelia 3` = lm(hom_rate ~ illicit_resources +
                                                  total_violence.s + rpe_gdp.s + polity2.s +
                                                  inequality.s + time + time2 + time3,
                                                data = a.out$imputations$imp3),
                             `Amelia 4` = lm(hom_rate ~ illicit_resources +
                            models2 = list(
    `Amelia 1` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp1, model = "negbin", cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 2` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp2, model = "negbin", cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 3` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 +
        time3, data = a.out$imputations$imp3, model = "negbin",
        cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 4` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp4, model = "negbin", cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 5` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp5, model = "negbin", cite = FALSE) %>%
        from_zelig_model()
)

modelsummary(models = models2, stars = TRUE, coef_omit = "year|gwno",
    coef_map = cn, vcov = ~gwno_a)

modelplot(models = models2, coef_omit = "year|Con|gwno", coef_map = cn,
    vcov = ~gwno_a) +
    geom_vline(xintercept = 0)
                      total_violence.s + rpe_gdp.s + polity2.s +
                                                  inequality.s + time + time2 + time3,
                                                data = a.out$imputations$imp4),
                             `Amelia 5` = lm(hom_rate ~ illicit_resources +
                                                  total_violence.s + rpe_gdp.s + polity2.s +
                                                  inequality.s + time + time2 + time3,
                                                data = a.out$imputations$imp5)
       )

modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno',
                    coef_map = cn)

modelplot(models = models, coef_omit = "year|Con|gwno", coef_map = cn) +
           geom_vline(xintercept = 0)



models2 = list(
    `Amelia 1` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp1, model = "negbin", cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 2` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp2, model = "negbin", cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 3` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 +
        time3, data = a.out$imputations$imp3, model = "negbin",
        cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 4` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp4, model = "negbin", cite = FALSE) %>%
        from_zelig_model(),

    `Amelia 5` = zelig(hom_count ~ illicit_resources + total_violence.s +
        pop.log + rpe_gdp.s + polity2.s + inequality.s + time + time2 + time3,
        data = a.out$imputations$imp5, model = "negbin", cite = FALSE) %>%
        from_zelig_model()
)

modelsummary(models = models2, stars = TRUE, coef_omit = "year|gwno",
    coef_map = cn, vcov = ~gwno_a)

modelplot(models = models2, coef_omit = "year|Con|gwno", coef_map = cn,
    vcov = ~gwno_a) +
    geom_vline(xintercept = 0)
