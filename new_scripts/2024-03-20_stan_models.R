library(rstanarm)
library(furrr)
library(tidyverse)
library(patchwork)
library(gridExtra)
library(bayesplot)

set.seed(987645)
options(mc.cores = parallel::detectCores())
options(scipen=999)

bnb <- stan_glm.nb(hom_count ~ resources + pop_slums + battle_deaths +
                     duration + troop + police,
                   data = subset(a.out$imputations[[1]], postcon == 1))

mcmc_areas(bnb, prob = .95, prob_outer = .95, pars = vars(-"(Intercept)"))

bnbs <- array(NA, c(8, 2, 5))
for (i in seq_len(5)) {
    bnb <- stan_glm.nb(hom_count ~ resources + pop_slums + 
                        total_deaths,
                      data = subset(a.out$imputations[[1]]), postcon == 1)
    bnbs[, , i] <- cbind(coef(bnb), se(bnb))
}
coef_names <- c("Intercept", "Illicit Resources", "Population\nLiving in Slums",
               
                "Total Deaths")

bnbs_plots <- lapply(seq_along(coef_names), function(.x) {
    ggplot() +
        geom_point(aes(x = 1:5, y = bnbs[.x, 1, ])) +
        geom_segment(aes(x = 1:5, xend = 1:5,
                         y = bnbs[.x, 1, ] - 1.96 * bnbs[.x, 2, ],
                         yend = bnbs[.x, 1, ] + 1.96 * bnbs[.x, 2, ]
                         )) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        xlab("") +
        ylab(ifelse(.x %in% c(2, 6), "Post-conflict Homicides", "")) +
        labs(title = coef_names[[.x]]) +
        theme(axis.text.y = element_text(angle = 90))
           }
)

layout_grid <- rbind(c(2, 3, 4, 5),
                     c(6, 7, 8, 1))

nb_grid <- arrangeGrob(grobs = bnbs_plots, nrow = 2, ncol = 4,
                       layout_matrix = layout_grid)
ggsave("figs/bnbs.png", nb_grid, height = 8, width = 12)
