options(scipen=999)
library(tidyverse)
library(tidytuesdayR)
library(statebins)
library(colorspace)

tuesdata <- tt_load("2022-01-11")
colony <- tuesdata$colony
stressor <- tuesdata$stressor

colony |> 
    ggplot(aes(x=as.factor(year), y=colony_n)) +
    geom_boxplot() +coord_flip() +
    labs(x="Year", y="Number of colonies", title="Lots of outliers") +
  facet_wrap(~months)

## filter down to 2015 October for a calculation
colonycompare <- colony |> 
  filter(months=="October-December",
         !(state %in% c("United States", "Other States")),
         year==2015) |> 
        rename(beginning=colony_n) |> 
        select(state, beginning)

colonyplot <- colony |> 
  filter(months=="October-December",
         !(state %in% c("United States", "Other States"))) |> 
  left_join(colonycompare,
              by="state") |> 
  mutate(change=((colony_n-beginning)/beginning)*100) |> 
  select(year, state, colony_n, beginning, change)

## set an appropriate palette

hcl_palettes("Diverging", n=3, plot=TRUE)
diverging_hcl(3, palette = "Blue-Red")
## initial plot
plot <- colonyplot |> 
  ggplot(aes(state=state, fill=change)) +
  geom_statebins() +
  coord_equal() +
  scale_fill_gradient2(low= "#8E063B", mid="#E2E2E2", high="#023FA5", midpoint=0,
    labels = scales::label_percent(scale=1)) +
  facet_wrap(~year) +
  ## label legend for percent change
  theme_void() +
  theme(legend.position="bottom",
  panel.background=element_rect(linewidth=1, color="black"),
  plot.title.position="plot") +
  guides(
    fill = guide_legend(title.position = "bottom") # Places the title above the horizontal legend keys
  ) +
  labs(title="Percent Change of Bee Colonies since 2015 by State",
    subtitle= "While some large bee colony states (CA), were affected, it seems like some states actually improved \nthe number of bee colonies over time",
    caption = "Source: USDA, TidyTuesday 2021 Week 2",
    fill = "Percent Change since 2015")

    

library(ggview)

plot + canvas(width=17, height=9)

ggsave(plot=plot, filename="2022/2 - Bee Colony/stateview.png", width=17, height=9, bg="white")
