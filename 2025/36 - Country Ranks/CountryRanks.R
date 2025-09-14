library(tidyverse)
library(ggflags) #pulls flags

tuesdata <- tidytuesdayR::tt_load(2025, week = 36)
countrylist <- tuesdata$country_lists
ranks <- tuesdata$rank_by_year

view(ranks)

##let's only include countries that have been in the top 10 every year
top10 <- ranks |> 
  group_by(country) |>
  mutate(n = sum(rank <= 10)) |> 
  ungroup() |> 
  filter(n > 0) |> 
  select(-n)

top10 <- top10 |> 
  group_by(year, rank) |> 
# add a unique number to each country in the top 10 for each year
  mutate(n = row_number()) |> 
  ungroup() |> 
# jitter the years so that flags don't overlap
  mutate(year = case_when(n-1 == 0 ~ year,
                           n %% 2 == 0 ~ year + (n-1)*0.05,
                           TRUE ~ year - (n-1)*0.05))

p <- top10 |> 
  filter(year > 2013.5) |> 
  ggplot(aes(x = year, y = rank, group = country)) +
  geom_line(aes(color=region)) +
  geom_flag(aes(country = tolower(code))) +
  scale_y_reverse(breaks = seq(1, 10, by = 1),
                  limits = c(10, 1)) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 1),
                      minor_breaks = seq(2013.5, 2024.5, by=1)) +
  theme_minimal() +
  ## place vertical lines between years
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "grey80", linetype = "dashed"),
        legend.position = "bottom") +
  labs(x="",
       y="",
       title="Top 10 Easiest Passports to Travel With (2015-2025)",
       subtitle="Countries that have consistently ranked in the top 10 countries for ease of travel",
       caption= "Tidy Tuesday Week 36 2025 | Data: Henley Passport Index")

library(ggview)

p + canvas(width = 12, height = 4, units = "in", dpi = 300)

ggsave("2025/36 - Country Ranks/Henley Passport.png", width = 12, height = 4, units = "in", dpi = 300, bg="white")
