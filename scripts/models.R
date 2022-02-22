library(Amelia)
library(tidyverse)
library(modelsummary)
library(Zelig)

load("data/data25Mar2020.Rdata")
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
           pop.s = (pop - mean(pop, na.rm = TRUE)) /  sd(pop, na.rm = TRUE),
           rpe_gdp.s = (rpe_gdp - mean(rpe_gdp, na.rm = TRUE)) /
                        sd(rpe_gdp, na.rm = TRUE),
           inequality.s = (inequality - mean(inequality, na.rm = TRUE)) /
                          sd(inequality, na.rm = TRUE)
         ) %>%
    select(year, gwno_a, conflict_id, hom_rate, hom_count, rgdppc.s, polity2.s,
           total_violence.s, pop.s, illicit_resources, rpe_gdp.s, time, time2, time3,
           side_a, side_b, inequality.s) %>%
    replace_na(list(total_violence = 0))

dat.df <- as.data.frame(dat)


a.out <- amelia(dat.df, m = 5, ts = "year", cs = "gwno_a",
                idvars = c("side_a", "side_b", "hom_rate", "hom_count"))
#missmap(a.out)

#boxplot(hom_count~illicit_resources,
#        data=dat,
#        main="Homicide for Groups with and without Illicit Funding",
#        xlab="Illicit Funding",
#        ylab="Homicides",
#        col="orange",
#        border="brown"
#)

ggplot() +
  geom_density(data = subset(dat, illicit_resources == 0),
               aes(x = hom_count), color = 'blue', fill = 'blue', alpha = .3) +
  geom_density(data = subset(dat, illicit_resources == 1),
               aes(x = hom_count), color = 'red', fill = 'red', alpha = .3) +
  theme_bw()

#dat$polity2_sqrd <- dat$polity2 ^ 2

models <- list(`Homicide Count` = zelig(hom_count ~ illicit_resources + illicit_resources * time +
                                              total_violence + pop + rpe_gdp +
                                              polity2 +
                                              time, data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Count` = zelig(hom_count ~ illicit_resources + illicit_resources * time +
                                                total_violence + pop + rpe_gdp +
                                                polity2 +
                                                time + time2 + time3,
                                            data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Count` = zelig(hom_count ~ illicit_resources + illicit_resources * time +
                                                total_violence + rpe_gdp +
                                                polity2 +
                                            gwno_a + year, data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Rate` = lm(hom_rate ~ illicit_resources + illicit_resources * time +
                                                total_violence  + rpe_gdp +
                                                polity2 +
                                            gwno_a + year,
                                        data = dat)
)

cn = c(
       'illicit_resources' = 'Illicit Funding',
       'total_violence' = 'Total Violence',
       'pop' = 'Population',
       'rpe_gdp' = 'Relative Political Extraction',
       'polity2' = 'Polity',
       'time' = 'Time',
       'time2' = 'Time^2',
       'time3' = 'Time^3',
       '(Intercept)' = 'Constant')
models = list(
  zelig(hom_count~illicit_resources, data = dat, model = 'negbin') %>%
            from_zelig_model(),
  zelig(hom_count~illicit_resources + log(pop + .0001), data = dat, model = 'negbin') %>%
    from_zelig_model()
  )

modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno',
             coef_map = cn, vcov = 'robust')
modelplot(models = models, coef_omit = 'year|Int', vcov = 'robust') +
    geom_vline(xintercept = 0)

models <- list(`Homicide Count` = zelig(hom_count ~ illicit_resources +
                                             total_violence + pop + rpe_gdp +
                                             polity2 + inequality +
                                             time, data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Count` = zelig(hom_count ~ illicit_resources +
                                             total_violence + pop + rpe_gdp +
                                             polity2 + inequality +
                                             time + time2 + time3,
                                         data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Count` = zelig(hom_count ~ illicit_resources +
                                             total_violence + rpe_gdp +
                                             polity2 + inequality +
                                             gwno_a + year, data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Rate` = lm(hom_rate ~ illicit_resources +
                                        total_violence  + rpe_gdp +
                                        polity2 + inequality +
                                        gwno_a + year,
                                    data = dat)
)

cn = c(
    'illicit_resources' = 'Illicit Funding',
    'total_violence' = 'Total Violence',
    'pop' = 'Population',
    'rpe_gdp' = 'Relative Political Extraction',
    'rgdppc' = 'Real GDP per Capita',
    'inequality' = 'Inequality',
    'polity2' = 'Polity',
    'time' = 'Time',
    'time2' = 'Time^2',
    'time3' = 'Time^3',
    '(Intercept)' = 'Constant')

modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno',
             coef_map = cn, vcov = 'robust')
modelplot(models = models, coef_omit = 'year|Int|gwno') +
    geom_vline(xintercept = 0)

# Best models

models <- list(`Homicide Count` = zelig(hom_count ~ illicit_resources +
                                            total_violence + pop + rpe_gdp +
                                            polity2 + inequality +
                                            time, data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Count` = zelig(hom_count ~ illicit_resources +
                                            total_violence + pop + rpe_gdp +
                                            polity2 + inequality +
                                            time + time2 + time3,
                                        data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Count` = zelig(hom_count ~ illicit_resources +
                                            total_violence + pop + rpe_gdp +
                                            polity2 + inequality + rgdppc +
                                            time, data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Count` = zelig(hom_count ~ illicit_resources +
                                            total_violence + pop + rpe_gdp +
                                            polity2 + inequality + rgdppc +
                                            time + time2 + time3,
                                        data = dat,
                                        model = 'negbin', cite = FALSE) %>%
                   from_zelig_model())

modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno',
             coef_map = cn, vcov = 'robust')
modelplot(models = models, coef_omit = 'year|Con|gwno', coef_map = cn,
          vcov = 'robust') +
    geom_vline(xintercept = 0)


# Clustered s.e.
modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno',
             coef_map = cn, vcov = ~gwno_a)
modelplot(models = models, coef_omit = 'year|Con|gwno', coef_map = cn,
          vcov = ~gwno_a) +
    geom_vline(xintercept = 0)


# OLS rate
models <- list(`Homicide Rate` = zelig(hom_rate ~ illicit_resources +
                                            total_violence + rpe_gdp +
                                            polity2 + inequality +
                                            time, data = dat,
                                        model = 'ls', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Rate` = zelig(hom_rate ~ illicit_resources +
                                            total_violence + rpe_gdp +
                                            polity2 + inequality +
                                            time + time2 + time3,
                                        data = dat,
                                        model = 'ls', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Rate` = zelig(hom_rate ~ illicit_resources +
                                            total_violence + rpe_gdp +
                                            polity2 + inequality + rgdppc +
                                            time, data = dat,
                                        model = 'ls', cite = FALSE) %>%
                   from_zelig_model(),
               `Homicide Rate` = zelig(hom_rate ~ illicit_resources +
                                            total_violence + rpe_gdp +
                                            polity2 + inequality + rgdppc +
                                            time + time2 + time3,
                                        data = dat,
                                        model = 'ls', cite = FALSE) %>%
                   from_zelig_model())

# Robust s.e.
modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno',
             coef_map = cn, vcov = 'robust')
modelplot(models = models, coef_omit = 'year|Con|gwno', coef_map = cn,
          vcov = 'robust') +
    geom_vline(xintercept = 0)

# Clustered s.e.
modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno',
             coef_map = cn, vcov = ~gwno_a)

modelplot(models = models, coef_omit = 'year|Con|gwno', coef_map = cn,
          vcov = ~gwno_a) +
    geom_vline(xintercept = 0)


m.out <- zelig(hom_count ~ illicit_resources +
          total_violence + rpe_gdp +
          polity2 + inequality +
          time, data = dat,
      model = 'negbin', cite = FALSE) %>%
    setx(illicit_resources = c(0, 1), polity2 = 0) %>%
    sim() %>%
    zelig_qi_to_df()

# simulated values
sims.slimmed <- qi_slimmer(m.out)
ggplot(sims.slimmed, aes(illicit_resources, qi_ci_median)) +
  geom_point() +
  geom_pointrange(aes(ymin = qi_ci_min, ymax = qi_ci_max)) +
    labs(title = 'Homicides') +
    ylab('Homicide Count') + xlab('Illicit Resources') +
  theme_bw() +
  geom_text(mapping = aes(label = '6861\nNo Resources', x = 0.125, y = 6861)) +
  geom_text(mapping = aes(label = '10705\nIllicit Funding', x = 0.875, y = 10705)) +
  scale_x_discrete(breaks = c(0, 1))


m.out <- zelig(hom_count ~ illicit_resources +
           total_violence + rpe_gdp +
           polity2 + inequality +
           time, data = dat,
         model = 'negbin', cite = FALSE)
x.out0 <- setx(m.out, illicit_resources = 0)
x.out1 <- setx(m.out, illicit_resources = 1)
s.out <- sim(x.out, x = x.out0, x1 = x.out1) %>%
  zelig_qi_to_df()

sims.slimmed <- qi_slimmer(s.out)
ggplot(sims.slimmed, aes(illicit_resources, qi_ci_median)) +
  geom_point() +
  geom_pointrange(aes(ymin = qi_ci_min, ymax = qi_ci_max)) +
  geom_hline(yintercept = (sims.slimmed$qi_ci_median[1] +
                             sims.slimmed$qi_ci_median[2]) / 2) +
  labs(title = 'Homicides') +
  ylab('Homicide Count') + xlab('Illicit Resources') +
  theme_bw()

# Imputed data
models = list(
               `Amelia 1` = zelig(hom_count ~ illicit_resources +
                                    total_violence.s + pop.s + rpe_gdp.s + polity2.s +
                                    inequality.s + time + time2 + time3,
                                  data = a.out$imputations$imp1,
                                  model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Amelia 2` = zelig(hom_count ~ illicit_resources +
                                    total_violence.s + pop.s + rpe_gdp.s + polity2.s +
                                    inequality.s + time + time2 + time3,
                                  data = a.out$imputations$imp2,
                                  model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Amelia 3` = zelig(hom_count ~ illicit_resources +
                                    total_violence.s + pop.s + rpe_gdp.s + polity2.s +
                                    inequality.s + time + time2 + time3,
                                  data = a.out$imputations$imp3,
                                  model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Amelia 4` = zelig(hom_count ~ illicit_resources +
                                    total_violence.s + pop.s + rpe_gdp.s + polity2.s +
                                    inequality.s + time + time2 + time3,
                                  data = a.out$imputations$imp4,
                                  model = 'negbin', cite = FALSE) %>%
                   from_zelig_model(),
               `Amelia 5` = zelig(hom_count ~ illicit_resources +
                                    total_violence.s + pop.s + rpe_gdp.s + polity2.s +
                                    inequality.s + time + time2 + time3,
                                  data = a.out$imputations$imp5,
                                  model = 'negbin', cite = FALSE) %>%
                   from_zelig_model()
)
