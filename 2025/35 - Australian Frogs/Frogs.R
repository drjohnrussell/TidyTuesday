library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2025, week = 35)
frogID <- tuesdata$frogID_data
frognames <- tuesdata$frog_names
