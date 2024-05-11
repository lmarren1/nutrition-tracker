library(tidyverse)
library(ggplot2)

biometric_data = read.csv(file = "data/health_proj_biometric_data.csv",
                          header = TRUE, sep = ",")
nutrition_data = read.csv(file = "data/health_proj_nutrition_data.csv",
                          header = TRUE, sep = ",")
exercise_data = read.csv(file = "data/health_proj_exercise_data.csv",
                         header = TRUE, sep = ",")
performance_data = read.csv(file = "data/health_proj_performance_data.csv",
                            header = TRUE, sep = ",")

biometric_data = data.frame(date = seq.Date(from = as.Date("2024/5/14"), by = 1, length.out = 30),
                            weight_lbs = sample(178:191, replace = TRUE, 30),
                            muscle_mass_lbs = sample(140:160, replace = TRUE, 30),
                            bone_mass_lbs = sample(7:9, replace = TRUE, 30),
                            sleep_hrs = sample(3:12, replace = TRUE, 30))
View(biometric_data)

nutrition_data = data.frame(date = seq.Date(from = as.Date("2024/5/14"), by = 1, length.out = 30),
                            caloric_intake_kcal = sample(2400:3700, replace = TRUE, 30),
                            total_fat_g = sample(20:140, replace = TRUE, 30),
                            sat_fat_g = sample(0:40, replace = TRUE, 30),
                            total_carb_g = sample(250:450, replace = TRUE, 30),
                            fiber_g = sample(10:60, replace = TRUE, 30),
                            sugar_g = sample(30:130, replace = TRUE, 30),
                            protein_g = sample(160:210, replace = TRUE, 30))
View(nutrition_data)

exercise_data = data.frame(date = seq.Date(from = as.Date("2024/5/14"), by = 1, length.out = 30),
                           plyo = sample(0:1, replace = TRUE, 30),
                           weightlift_type = sample(1:3, replace = TRUE, 30),
                           weightlift_volume_lbs = sample(3000:5500, replace = TRUE, 30),
                           basketball_mins = sample(0:480, replace = TRUE, 30),
                           swimming_mins = sample(0:80, replace = TRUE, 30),
                           core = sample(0:1, replace = TRUE, 30))
View(exercise_data)

performance_data = data.frame(date = seq.Date(from = as.Date("2024/5/14"), by = 1, length.out = 30),
                              standing_vertical_inches = sample(24:39, replace = TRUE, 30),
                              max_vertical_inches = sample(25:40, replace = TRUE, 30),
                              three_fourths_sprint_secs = sample(2.8:4.8, replace = TRUE, 30),
                              lane_agility_secs = sample(10:18, replace = TRUE, 30))
View(performance_data)








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


