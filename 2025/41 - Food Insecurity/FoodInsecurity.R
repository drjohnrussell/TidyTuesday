library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 41)

food <- tuesdata$food_security
colnames(food)
unique(food$Item)

food2 <- food |> 
  filter(!str_detect(Item, "male|female|Number"))

library(plotly)

{ food2 |> 
  group_by(Item) |> 
  mutate(minyear=min(Year_Start)) |> 
  ungroup() |> 
  filter(minyear < 2010) |> 
  group_by(Year_Start, Item) |> 
  summarise(yearlymean = mean(Value, na.rm=TRUE)) |> 
  ggplot(aes(x=Year_Start, y=yearlymean, color=Item)) +
  geom_line() } |> 
  ggplotly()
