library(countrycode)
library(mapdata)
library(patchwork)
library(tidyverse)
library(readxl)


theme_set(theme_classic())

ged2015 <- ged_data %>%
    filter(year == 2015) %>%
    group_by(country_id, year) %>%
    summarise(best = sum(best)) %>%
    select(year, country_id, best)

world_map <- map_data("worldHires")
world_map$country_id <- countrycode(world_map$region, "country.name", "gwn")
map_data_ged <- right_join(ged2015, world_map, by = "country_id") #%>%
    #mutate(best = (replace_na(best, 0)))

# GED deaths
p_ged <- ggplot() +
    geom_polygon(data = map_data_ged, mapping = aes(x = long, y = lat,
                                      fill = best, group = group),
                 color = "black", size = 0.1) +
    coord_fixed(1.3) +
    scale_fill_gradient2(low = "green", mid = "white", high = "red") +
    theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

# UNODC homicides
hom2015 <- read_xls("data/homicide_total_rate_and_count.xls") %>%
    filter(Year == 2015, Indicator == "Homicide count") %>%
    select(Year, Indicator, Territory, Value)

hom2015$country_id <- countrycode(hom2015$Territory, "country.name", "gwn")
map_data_undoc <- right_join(hom2015, world_map, by = "country_id") %>%
    mutate(Value = as.numeric(Value))
           #Value = (replace_na(Value, 0)))

p_undoc <- ggplot() +
    geom_polygon(data = map_data_undoc, mapping = aes(x = long, y = lat,
                                                      fill = Value,
                                                      group = group),
                 color = "black", size = 0.1) +
    coord_fixed(1.3) +
    scale_fill_gradient2(low = "green", mid = "white", high = "red", name = "Deaths") +
        theme(text = element_text(family = "serif"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


p_ged + p_undoc +
    plot_annotation(title = "Political and Criminal Violence",
                    subtitle = "2015") +
                    theme(title = element_text(family = "serif",
                                                    hjust = 0.5))
