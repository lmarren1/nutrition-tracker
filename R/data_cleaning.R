library(tidyverse)

data = read.csv(file = "data/diet_data.csv", header = TRUE, sep = ",")

View(data)
glimpse(data)

################################################################################
## Data wrangling
################################################################################

## Add analysis variables
data = data |>
  replace(is.na(data), 1) |>
  mutate(body_fat = (weight - muscle_mass - bone_mass),
         body_fat_pct = (body_fat/weight),
         muscle_mass_pct = (muscle_mass/weight),
         bone_mass_pct = (bone_mass/weight),
         unsat_fat = (total_fat - sat_fat),
         fat_pct = ((total_fat*9)/caloric_intake),
         carb_pct = ((total_carb*4)/caloric_intake),
         protein_pct = ((protein*4)/caloric_intake),
         non_fiber_complex_carb = (total_carb - fiber - sugar))

## Isolate regressors
regressors_data = data |>
  select(weight, muscle_mass, bone_mass, body_fat, unsat_fat, sat_fat,
         non_fiber_complex_carb, fiber, sugar, protein, plyo, weightlift_times,
         weightlift_type, basketball, swimming, core)

ols_model = lm(weight ~ muscle_mass + bone_mass + body_fat + unsat_fat + sat_fat +
   non_fiber_complex_carb + fiber + sugar + protein + plyo + weightlift_times +
   weightlift_type + basketball + swimming + core, data)

summary(ols_model)
