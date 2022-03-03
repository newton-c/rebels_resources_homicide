library(tidyverse)
library(purrr)

datasets <- list()

datasets <- map(seq_len(5), function(i) {
    file_path <- paste0("data/imputed_data_", i, ".csv")
    datasets[[i]] <- read_csv(file_path)
})

datasets[[6]] <- read_csv("data/unimputed_data.csv")

map(seq_len(6), function(i) summary(datasets[[i]]))
