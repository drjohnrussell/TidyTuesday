library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 38)

august <- tuesdata$fide_ratings_august
september <- tuesdata$fide_ratings_september

view(august)

p <- september |> 
  group_by(bday,sex) |> 
  slice_max(order_by = rating, n = 20) |>
  summarise(rating=mean(rating, na.rm = TRUE)) |>
  ungroup() |> 
  ggplot(aes(x=2025-bday, y=rating, color=sex)) +
  geom_smooth(method = "loess", se=FALSE) +
  theme_minimal() +
  labs(title="Average Rating of Top 20 Players at Each Age",
       subtitle="Data from FIDE Ratings, September 2025",
       x="Age",
       y="Average Rating",
       color="Gender",
       caption="Tidy Tuesday (2025, Week 37)") +
  scale_color_manual(values=c("blue", "red")) +
  theme(legend.position = "top")
library(ggview)

p + canvas(width = 6, height = 4, units = "in", dpi = 300)

ggsave(plot=p, filename = "2025/38 - Chess/chess.png", width = 6, height = 4, units = "in", dpi = 300, bg="white")
