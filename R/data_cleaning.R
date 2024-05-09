library(tidyverse)

data = read.csv(file = "data/diet_data.csv", header = TRUE, sep = ",")

View(data)
glimpse(data)

data = data |>
  replace(is.na(data), 1) |>
  mutate(body_fat = (weight - muscle_mass - bone_mass),
         body_fat_pct = (body_fat/weight),
         muscle_mass_pct = (muscle_mass/weight),
         bone_mass_pct = (bone_mass/weight),
         unsat_fat = (total_fat - sat_fat),
         fat_pct = ((total_fat*9)/caloric_intake),
         carb_pct = ((total_carb*4)/caloric_intake),
         protein_pct = ((protein*4)/caloric_intake))

View(data)

