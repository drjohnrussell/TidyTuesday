library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 34)

billboard <- tuesdata$billboard

billboard |> 
  ggplot(aes(x=date,y=weeks_at_number_one, color=as.factor(eurovision_entry))) +
  geom_point()

billboard |> 
  group_by(Year=year(date)) |> 
  summarise(`Max weeks at Number One`=max(weeks_at_number_one)) |> 
  ggplot(aes(x=Year, y=`Max weeks at Number One`)) +
  geom_point() +
  geom_smooth(method="gam", color="blue", se=FALSE) +
  geom_smooth(method="lm", color="red", se=FALSE) +
  geom_smooth(method="loess", color="green", se=FALSE) +
  theme_light() +
  labs(y="Number of Weeks",
       title="Longest Time for a Hit at Number One by Year",
       caption="Tidy Tuesday (2025, week 34)") +
  scale_x_continuous(breaks = seq(1950, 2025, by=10))
