library(rstanarm)
library(furrr)
library(tidyverse)
library(patchwork)
library(gridExtra)

set.seed(987645)
options(mc.cores = parallel::detectCores())

datasets <- map(seq_len(5), function(i) {
    file_path <- paste0("data/imputed_data_", i, ".csv")
    datasets[[i]] <- read_csv(file_path)
})

datasets[[6]] <- read_csv("data/unimputed_data.csv") %>%
    drop_na()

bm1 <- stan_glm.nb(hom_count ~ illicit_resources + pop_slums + gini +
                   corruption + pko_troops + pko_police + total_deaths,
                   data = datasets[[1]])

bnbs <- array(NA, c(8, 2, 5))
for (i in seq_len(5)) {
    bnb <- stan_glm.nb(hom_count ~ illicit_resources + pop_slums + gini +
                        corruption + pko_troops + pko_police + total_deaths,
                      data = datasets[[i]])
    bnbs[, , i] <- cbind(coef(bnb), se(bnb))
}
coef_names <- c("Intercept", "Illicit Resources", "Population\nLiving in Slums",
                "Inequality (Gini)", "Corruption", "PKO Troops", "PKO Police",
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
