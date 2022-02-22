library(tidyverse)

load("data/data25Mar2020.Rdata")


p <- dat %>%
    group_by(conflict_id) %>%
    filter(hom_count > 0,
           !is.na(illicit_resources)) %>%
    ggplot(aes(x = year, y = hom_rate,
               color = as.factor(illicit_resources))) +
    geom_line() +
    facet_wrap(~side_a) +
    scale_y_continuous(limits = c(0, 150)) +
    labs(color = "Illicit Resources") +
    theme_bw()

p
