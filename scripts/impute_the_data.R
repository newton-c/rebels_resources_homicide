library(Amelia)
library(tidyverse)

set.seed(977634)

unimputed_data <- read_csv("data/unimputed_data.csv")

a.out <- amelia(data.frame(unimputed_data), m = 5, ts = "year", cs = "country_id",
                empri = .01 * nrow(unimputed_data))

# save each dataset separately
lapply(seq_len(5), function(i) {
	dataset <- a.out$imputations[[i]]
	file_name <- paste0("data/imputed_data_", i, ".csv")
	write_csv(dataset, file = file_name)
})