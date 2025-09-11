library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggview)

tuesdata <- tidytuesdayR::tt_load(2025, week = 35)
frogID <- tuesdata$frogID_data
frognames <- tuesdata$frog_names

frogID <- frogID |>
  left_join(frognames |> 
    select(scientificName, subfamily,tribe) |>
    mutate(scientificName = word(scientificName, 1, 2)) |>
    distinct(), by = "scientificName") |> 
  mutate(night=case_when(hour(eventTime) < 6 ~ "night",
                         hour(eventTime) >= 18 ~ "night",
                         TRUE ~ "day"))

# pull the map of australia
australia <- ne_states(country = "australia", returnclass = "sf")

## map the frogs over australia by density

australia |> 
  ggplot() +
  geom_sf(fill = "lightgrey") +
  geom_point(data = frogID |> 
    filter(!is.na(tribe)), 
            aes(x = decimalLongitude, y = decimalLatitude, color=tribe), size = 0.5, alpha = 0.7) +
 # geom_density_2d(data = frogID, aes(x = decimalLongitude, y = decimalLatitude), alpha = 0.6, contour_var = "count") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Frog Species in Australia",
       subtitle = "Locations of various frog species across Australia",
       caption = "Tidy Tuesday (2025, Week 35)",
      color="") +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10))

## look at distribution of identifications by hour of day

frogID |>
  filter(!is.na(hour(eventTime)), 
          !is.na(tribe)) |>
  ggplot(aes(x = hour(eventTime), fill = tribe)) +
  geom_histogram(binwidth = 1, position = "stack", color = "black") +
  theme_minimal() +
  labs(title = "Frog Identifications by Hour of Day",
       x = "Hour of Day",
       y = "Number of Identifications",
       fill = "Tribe",
       caption = "Tidy Tuesday (2025, Week 35)") +
  scale_x_continuous(breaks = 0:23) +
  theme(legend.position = "bottom")
