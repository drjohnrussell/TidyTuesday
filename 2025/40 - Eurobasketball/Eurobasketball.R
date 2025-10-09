library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 40)

basketball <- tuesdata$euroleague_basketball

## separate out the years the team won
basketballwinners <- basketball |> 
  tidyr::separate_longer_delim(Years_of_Titles_Won, delim = ", ") |> 
  filter(!is.na(as.integer(Years_of_Titles_Won)))

library(rnaturalearth)
library(gganimate)

europe <- ne_countries(continent = "Europe", returnclass = "sf", scale = 50)
middleeast <- ne_countries(continent = "Asia", returnclass = "sf")
africa <- ne_countries(continent = "Africa", returnclass = "sf")

map <- rbind(europe, middleeast,africa) |> 
  full_join(basketball |> 
              select(Country) |> 
              mutate(HasTeam = TRUE) |> 
              distinct(), by = c("name" = "Country"))

basketballwinners <- basketballwinners |> 
  left_join(map |> 
            select(name, geometry), by = c("Country" = "name"))

animation <- map |> 
  ggplot() +
  geom_sf(aes(fill = HasTeam), color = "white", show.legend=FALSE) +
  geom_sf(data = basketballwinners, aes(geometry = geometry), fill = "gold", size = 3) +
  transition_states(Years_of_Titles_Won, state_length = 1, transition_length = 0) +
  labs(title = 'Countries with Euroleague Basketball Teams Winning Titles',
    subtitle = 'Year: {closest_state}')+
  scale_y_continuous(limits=c(15, 75)) +
  scale_x_continuous(limits=c(-25, 60)) +
  theme_minimal()

anim_save(animation = animation, filename = "2025/40 - Eurobasketball/euroleague_basketball.gif")

animation
