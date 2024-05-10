library(tidyverse)
library(ggplot2)

data = read.csv(file = "data/diet_data.csv", header = TRUE, sep = ",")

View(data)
glimpse(data)

data = data.frame(weight = sample(176:189, replace = TRUE, 30),
                     muscle_mass = sample(150:154.5, replace = TRUE, 30),
                     bone_mass = sample(7.8:8, replace = TRUE, 30),
                     body_fat = sample(20:24, replace = TRUE, 30),
                     unsat_fat = sample(15:70, replace = TRUE, 30),
                     sat_fat = sample(15:30, replace = TRUE, 30),
                     non_fiber_complex_carb = sample(100:200, replace = TRUE, 30),
                     fiber = sample(15:50, replace = TRUE, 30),
                     sugar = sample(10:60, replace = TRUE, 30),
                     protein = sample(150:210, replace = TRUE, 30),
                     plyo = sample(0:1, replace = TRUE, 30),
                     weightlift_times = sample(1:2, replace = TRUE, 30),
                     weightlift_type = sample(1:3, replace = TRUE, 30),
                     basketball = sample(0:1, replace = TRUE, 30),
                     swimming = sample(0:1, replace = TRUE, 30),
                     core = sample(0:1, replace = TRUE, 30))


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

ols_formula = data$weight ~ data$muscle_mass + data$bone_mass + data$body_fat + data$unsat_fat + data$sat_fat +
  data$non_fiber_complex_carb + data$fiber + data$sugar + data$protein + data$plyo + data$weightlift_times +
  data$weightlift_type + data$basketball + data$swimming + data$core

ols_model = lm(ols_formula)
ols_predictions = predict(ols_model, data)
summary(ols_model)
?summary
summary.data.frame(data)

ggplot() +
  geom_point(mapping = aes(x = 1:30, y = data$weight)) +
  geom_line(mapping = aes(x = 1:30, y = ols_predictions))


