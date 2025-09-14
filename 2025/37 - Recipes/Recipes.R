library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 37)

all_recipes <- tuesdata$all_recipes
cuisines <- tuesdata$cuisines

all_recipes2 <- all_recipes |> 
  mutate(ingredients = str_split(ingredients, ", ")) |> 
  unnest(ingredients)

## find the most common ingredients to check

common <- all_recipes2 |> 
  count(ingredients, sort = TRUE) |> 
  slice_head(n = 40)
view(common)
## notice the following ingredients to filter out: 
## chopped, divided, minced, softened, diced, sliced, drained, melted,
## beaten, finely chopped, thinly sliced, and anything starting with "or"

filtered <- all_recipes2 |> 
  filter(!ingredients %in% c("chopped", "divided", "minced", "softened", 
                             "diced", "sliced", "drained", "melted", 
                             "beaten", "finely chopped", "thinly sliced",
                            "thawed","juiced","cubed", "crushed",
                          "at room temperature", "peeled")) |>
  filter(!str_starts(ingredients, "or "))
## now find the most common ingredients again
common2 <- filtered |> 
  count(ingredients, sort = TRUE) |> 
  slice_head(n = 50)
## looks better for ingredient analysis