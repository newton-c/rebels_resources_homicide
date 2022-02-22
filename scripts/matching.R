library(MASS)
library(MatchIt)
library(modelsummary)
library(tidyverse)

library(DMwR)

select <- dplyr::select # both dplyr and MASS have select, I want to use dplyr

load(file = "data/data25Mar2020.Rdata")

matching_data <- dat %>% 
    filter(year > last_year_conflict & !is.na(illicit_resources)) %>% 
    mutate(time = year - last_year_conflict,
           time2 = time**2,
           time3 = time**3) %>% 
    select(year, gwno_a, conflict_id, hom_rate, hom_count, rgdppc, polity2, total_violence, pop,
           illicit_resources, rpe_gdp, time, time2, time3, side_a 
           ) %>% 
    replace_na(list(total_violence = 0)) %>% 
    drop_na()


ggplot() + 
    geom_density(data = matching_data, aes(hom_rate), fill = 'red') + 
    theme_bw()
ggplot() + 
    geom_density(data = matching_data, aes(hom_count), fill = 'red') +
    theme_bw()
#matching_data <- as.data.frame(matching_data)
#knn_impute <- knnImputation(data = matching_data, k = 15, scale = F, meth = "weighAve")

ggplot(matching_data, aes(x = as.factor(illicit_resources), y = hom_count)) + 
    geom_boxplot() +
    theme_classic()


matching_data <- dat %>% drop_na()
cem_data <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                        pop + polity2 + rgdppc, data = a.out$imputations$imp1,
                    method = 'cem')

summary(cem_data)
#plot(cem_data)

cem_data <- match.data(cem_data)

genetic_data <- matchit(illicit_resources ~ total_violence + year + rpe_gdp +
                            pop + polity2 + rgdppc, data = matching_data,
                        method = 'genetic')

summary(genetic_data)
#plot(genetic_data)


genetic_data <- match.data(genetic_data)

models <- list(`CEM Homicide Rate` = lm(hom_rate ~ illicit_resources, 
                                        data = cem_data, weights = weights),
               `CEM Homicide Count` = glm.nb(hom_count ~ illicit_resources, 
                                             data = cem_data, weights = weights),
               `Genetic Homicide Rate` = lm(hom_rate ~ illicit_resources, 
                                        data = genetic_data, weights = weights),
               `Genetic Homicide Count` = glm.nb(hom_count ~ illicit_resources, 
                                             data = genetic_data, weights = weights)
               )
modelsummary(models = models, stars = TRUE)
modelplot(models = models, coef_omit = 'Int') +
    geom_vline(xintercept = 0)

matching_data$polity2_sqrd <- matching_data$polity2 ^ 2

models <- list(`Homicide Count` = glm.nb(hom_count ~ illicit_resources + 
                                              total_violence + pop + rpe_gdp +
                                              polity2 + 
                                              time, data = matching_data),
               `Homicide Count` = glm.nb(hom_count ~ illicit_resources + 
                                                total_violence + pop + rpe_gdp +
                                                polity2 + 
                                                time + time2 + time3,
                                            data = matching_data),
               `Homicide Count` = glm.nb(hom_count ~ illicit_resources + 
                                                total_violence + rpe_gdp +
                                                polity2 + 
                                            as.factor(gwno_a) + as.factor(year), data = matching_data),
               `Homicide Rate` = lm(hom_rate ~ illicit_resources + 
                                                total_violence  + rpe_gdp +
                                                polity2 + 
                                            as.factor(gwno_a) + as.factor(year),
                                        data = matching_data)
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

modelsummary(models = models, stars = TRUE, coef_omit = 'year|gwno', 
             coef_map = cn, vcov = 'robust')
modelplot(models = models, coef_omit = 'year|Int') +
    geom_vline(xintercept = 0)


# CEM imputed models
cem_data1 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp1,
                     method = 'cem')
cem_data2 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp2,
                     method = 'cem')
cem_data3 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp3,
                     method = 'cem')
cem_data4 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp4,
                     method = 'cem')
cem_data5 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp5,
                     method = 'cem')

models = list(
    `Amelia 1` = zelig(hom_count ~ illicit_resources, 
                       data = match.data(cem_data1), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 2` = zelig(hom_count ~ illicit_resources, 
                       data = match.data(cem_data2), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 3` = zelig(hom_count ~ illicit_resources, 
                       data = match.data(cem_data3), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 4` = zelig(hom_count ~ illicit_resources, 
                       data = match.data(cem_data4), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 5` = zelig(hom_count ~ illicit_resources, 
                       data = match.data(cem_data5), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model()
    
)
modelplot(models = models, coef_omit = 'Int') + 
    geom_vline(xintercept = 0) +
    labs(title = "CEM")

# Genetic imputed data
models = list(
    `Amelia 1` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data1), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 2` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data2), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 3` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data3), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 4` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data4), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 5` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data5), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model()
    
)
modelplot(models = models, coef_omit = 'Int') + 
    geom_vline(xintercept = 0) +
    labs(title = "Genetic")

# CEM imputed models (rate)
models = list(
    `Amelia 1` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(cem_data1), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 2` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(cem_data2), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 3` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(cem_data3), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 4` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(cem_data4), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 5` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(cem_data5), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model()
    
)
modelplot(models = models, coef_omit = 'Int') + 
    geom_vline(xintercept = 0) +
    labs(title = "CEM")

# Genetic imputed data (rate)
gen_data1 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp1,
                     method = 'genetic')
gen_data2 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp2,
                     method = 'genetic')
gen_data3 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp3,
                     method = 'genetic')
gen_data4 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp4,
                     method = 'genetic')
gen_data5 <- matchit(illicit_resources ~ total_violence + rpe_gdp +
                         pop + polity2 + rgdppc, data = a.out$imputations$imp5,
                     method = 'genetic')

models = list(
    `Amelia 1` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data1), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 2` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data2), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 3` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data3), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 4` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data4), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model(),
    `Amelia 5` = zelig(hom_rate ~ illicit_resources, 
                       data = match.data(gen_data5), model = 'negbin', 
                       cite = FALSE) %>% 
        from_zelig_model()
    
)
modelplot(models = models, coef_omit = 'Int') + 
    geom_vline(xintercept = 0) +
    labs(title = "Genetic")
