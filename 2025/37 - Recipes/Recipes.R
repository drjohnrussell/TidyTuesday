library(tidyverse)
library(tidytext)

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
                          "at room temperature", "peeled","halved","shredded",
                          "quartered","lightly beaten","rinsed and drained")) |>
  filter(!str_starts(ingredients, "or "))
## now find the most common ingredients again, and take out common pantry items

common2 <- filtered |> 
  filter(!str_detect(ingredients,"salt|pepper|water|oil|sugar|flour|butter|egg|cooking spray")) |>
  count(ingredients, sort = TRUE) |> 
  slice_head(n = 25)
## looks better for ingredient analysis

p <- common2 |> 
  ggplot(aes(x = reorder(ingredients, n), y = n)) +
           geom_col(fill = "lightblue") +
           coord_flip() +
           labs(title = "Most Common Ingredients in Recipes",
                x = "Ingredient",
                y = "Count",
                caption = "Tidy Tuesday Week 37 2025") +
           theme_minimal()

library(ggview)
p + canvas(width = 8, height = 6, units = "in", dpi = 300)
ggsave("2025/37 - Recipes/recipes.png", plot = p, width = 8, height = 6, dpi = 300, bg="white")
