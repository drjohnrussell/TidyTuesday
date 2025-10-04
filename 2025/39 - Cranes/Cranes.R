library(tidyverse)
library(ggmap)

tuesdata <- tidytuesdayR::tt_load(2025, week = 39)

cranes <- tuesdata$cranes

# pull a base map of Lake Hornborgasjön

lake_map <- get_map(location = c(lon = 13.55, lat = 58.32), zoom = 12, maptype = "terrain")
map <- ggmap(lake_map)

map <- map + theme_void()

# plot the crane data as a scatter plot, just pulling the month and day from the date
library(viridis)
plot <- cranes |> 
  mutate(spring=ifelse(yday(date)<160, "Spring", "Fall"),
         spring=factor(spring, levels=c("Spring","Fall"))) |>
  ggplot(aes(x=yday(date), y=observations, color=year(date))) +
  geom_point(alpha=0.5) +
  geom_smooth(aes(group=paste0(year(date), spring)), method="loess", se=FALSE) +
  ## have an x axis that shows month labels
  scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                     labels = month.abb) +
  scale_color_viridis() +
  scale_y_continuous(limits=c(0, 28000), breaks=seq(0, 28000, by=4000)) +
  theme_minimal() +
  theme(legend.position = "bottom",
## make the legend wider
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(.2, "cm"),
      panel.border=element_rect(linewidth=.1,fill=NA)) +
  labs(title="Crane observations at Lake Hornborgasjön",
       subtitle="Daily crane observations from 1994 to 2024",
       x="Month",
       y="Number of cranes observed",
       color="",
       caption="Tidy Tuesday (2025, Week 39)") +
  facet_wrap(~spring, scales="free_x")

plot

library(ggview)
library(patchwork)
library(cowplot)

## put map as in inset of the plot

final_plot <- plot + inset_element(map, left = 0.88, bottom = 0.65, right = 1, top = 1, align_to = 'full')
final_plot

final_plot + canvas(bg="white", width=14, height=5, units="in")

ggsave("2025/39 - cranes/cranes.png", width=14, height=5, units="in", dpi=300)
