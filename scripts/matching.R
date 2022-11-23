library(lmtest)
library(MatchIt)
library(purrr)
library(tidyverse)
library(sandwich)
library(Zelig)
library(patchwork)

theme_set(theme_bw)

datasets <- list()

datasets <- map(seq_len(5), function(i) {
    file_path <- paste0("data/imputed_data_", i, ".csv")
    datasets[[i]] <- read_csv(file_path)
})

datasets[[6]] <- read_csv("data/unimputed_data.csv") %>%
    drop_na()

match_formula <- illicit_resources ~ pop_slums + gini + corruption +
                 pko_troops + pko_police + total_deaths

match_algos <- list("genetic", "cem", "nearest", "optimal")

matched_data <- list()
for (k in seq_len(24)) {
for (i in seq_along(datasets)) {
    for (j in seq_along(match_algos)) {
    matched <- matchit(formula = match_formula, data = datasets[[i]],
                                 method = match_algos[[j]], replace = TRUE)
        }
    matched_data[[k]] <- matched
    }
}


# vignette("estimating-effects")
gens <- array(NA, c(6, 2))
for (i in seq_len(6)) {
    dat <- match.data(matched_data[[i]])
    gen <- zelig(hom_count ~ illicit_resources, data = dat,
                 weights = dat$weights, model = "negbin", cite = FALSE) %>%
           from_zelig_model()
    gens[i, 1] <- coeftest(gen, vcov. = vcovHC)[[2]]
    gens[i, 2] <- coeftest(gen, vcov. = vcovHC)[[4]]
}

gen_p <- ggplot() +
    geom_point(aes(x = seq_len(6), y = gens[, 1])) +
    geom_segment(aes(x = seq_len(6), xend = seq_len(6),
                     y = gens[, 1] - 1.96 * gens[, 2],
                     yend = gens[, 1] + 1.96 * gens[, 2])) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    xlab("Amelia II Dataset #") +
    ylab("Treatment Effect") +
    labs(title = "Genetic Matching")

cems <- array(NA, c(6, 2))
for (i in seq_len(6)) {
    j <- i + 5
    dat <- match.data(matched_data[[j]])
    cem <- zelig(hom_count ~ illicit_resources, data = dat,
                 weights = dat$weights, model = "negbin", cite = FALSE) %>%
           from_zelig_model()
    cems[i, 1] <- coeftest(cem, vcov. = vcovHC)[[2]]
    cems[i, 2] <- coeftest(cem, vcov. = vcovHC)[[4]]
}

cem_p <- ggplot() +
    geom_point(aes(x = seq_len(6), y = gens[, 1])) +
    geom_segment(aes(x = seq_len(6), xend = seq_len(6),
                     y = gens[, 1] - 1.96 * gens[, 2],
                     yend = gens[, 1] + 1.96 * gens[, 2])) +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    xlab("Amelia II Dataset #") +
    ylab("Treatment Effect") +
    labs(title = "Coarsened-Exact Matching")
