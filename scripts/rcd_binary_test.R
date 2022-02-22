library(tidyverse)
library(merge.stats)
library(UCDP.codes)
library(countrycode)


# get a df with all unique conflict and dyad ids
conflicts_and_dyads <- ged_data %>%
    select(conflict_new_id, dyad_new_id, dyad_name) %>%
    distinct


# convert the old UCDP dyad ids used in the RCD to the new ids in a new column
rcd_data$dyad_new_id <- NA
rcd_data$dyad_new_id <- ucdp_ids(rcd_data$dyadid, "old_id", "new_id", "dyad_id")


# merge the RCD with the conflict/dyad id df to merge based on conflict id
rcd_dyad <- join_stats(rcd_data, conflicts_and_dyads,
                       by = c("dyad_new_id"), join = "left")


# extract the needed columns from RCD, replace missing values with 0
rcd_binary <- rcd_dyad %>%
    select(conflict_new_id,
           grep("extortion$", colnames(rcd_dyad), value = T),
           grep("theft$", colnames(rcd_dyad), value = T),
           grep("smuggling$", colnames(rcd_dyad), value = T),
           grep("smugglinghuman$", colnames(rcd_dyad), value = T),
           grep("smugglingother$", colnames(rcd_dyad), value = T),
           grep("bf$", colnames(rcd_dyad), value = T),
           grep("kidnap$", colnames(rcd_dyad), value = T),
           grep("kidnapintl$", colnames(rcd_dyad), value = T),
           grep("other$", colnames(rcd_dyad), value = T),
           grep("piracy$", colnames(rcd_dyad), value = T),
           grep("humanitarianaid$", colnames(rcd_dyad), value = T)) %>%
    replace(is.na(.), 0) #%>%

rcd_binary$illicit_resources <- 0 # initialise at 0 to avoid NAs
rcd_binary_iter <- rcd_binary

for_loop_start <- Sys.time()
# if any illicit resource is 1 for a row, make illicit_resources 1
for (i in seq_len(nrow(rcd_binary))) {
    for (j in seq_along(rcd_binary)) {
        if (rcd_binary[i, j] == 1) {
            rcd_binary$illicit_resources[i] <- 1
        }
    }
}
for_loop_end <- Sys.time()

iter_start <- Sys.time()
rcd_iter <- function(i, j) {
    if (i < 1625 & j < 132) {
        if (rcd_binary_iter[i, j] == 1) {
            rcd_binary_iter$illicit_resources[i] <- 1
            j <- j + 1
            rcd_iter(i, j)
            print("initial if statement working")
        } else {
            j <- j + 1
            rcd_iter(i, j)
        }
    } else if (i < 1625 & j == 132) {
               i <- i + 1
               j <- 1
               rcd_iter(i, j)
    } else {
    return(rcd_binary_iter)
    }
}


rcd_iter <- function(i, j) {
    if (i >= 1625 & j >= 132) {
        return(rcd_binary_iter)
    } else {
        if (rcd_binary_iter[i, j] == 1) {
            rcd_binary_iter$illicit_resources[i] <- 1
            j <- j + 1
            rcd_iter(i, j)
            print("initial if statement working")
        } else {
            j <- j + 1
            rcd_iter(i, j)
        }
    }
    } else if (i < 1625 & j == 132) {
               i <- i + 1
               j <- 1
               rcd_iter(i, j)
    } else {
    return(rcd_binary_iter)
    }
}

    } else if (j > ncol(rcd_binary_iter)) {
               i <- i + 1
               j <- 1
               rcd_iter(i, j)
    } else {
        return(rcd_binary_iter)
    }
}

rcd_test <- rcd_iter(i = 1, j = 1)
iter_end <- Sys.time()

print(paste("for-loop time: ", for_loop_end - for_loop_start))
print(paste("iter time: ", iter_end - iter_start))
