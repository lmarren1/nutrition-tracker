################################################################################
## Import libraries
################################################################################

library(tidyverse)
library(ggplot2)

################################################################################
## Process CSVs
################################################################################

biometric_data = read.csv(file = "data/health_proj_biometric_data.csv",
                          header = TRUE, sep = ",")
nutrition_data = read.csv(file = "data/health_proj_nutrition_data.csv",
                          header = TRUE, sep = ",")
exercise_data = read.csv(file = "data/health_proj_exercise_data.csv",
                         header = TRUE, sep = ",")
performance_data = read.csv(file = "data/health_proj_performance_data.csv",
                            header = TRUE, sep = ",")

################################################################################
## Create Fake Data (Alternative to CSVs)
################################################################################

biometric_data = data.frame(date = seq.Date(from = as.Date("2024/5/14"), by = 1, length.out = 30),
                            weight_lbs = seq(from = 180, to = 190, length.out = 30),
                            muscle_mass_lbs = seq(from = 145, to = 170, length.out = 30),
                            bone_mass_lbs = seq(from = 7, to = 9, length.out = 30),
                            sleep_hrs = sample(6:10, replace = TRUE, 30))
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

################################################################################
## Helper Functions
################################################################################

## moving average
ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}

################################################################################
## Create analysis dfs
################################################################################

biometric_data = biometric_data |>
  mutate(body_fat_lbs = (weight_lbs - muscle_mass_lbs - bone_mass_lbs),
         body_fat_pct = (body_fat_lbs/weight_lbs),
         muscle_mass_pct = (muscle_mass_lbs/weight_lbs),
         bone_mass_pct = (bone_mass_lbs/weight_lbs))
View(biometric_data)

biometric_analysis_data = biometric_data |>
  mutate(dly_chg_body_fat_lbs = body_fat_lbs - lag(body_fat_lbs, default = first(body_fat_lbs)),
         wkly_chg_body_fat_lbs = body_fat_lbs - lag(body_fat_lbs, n = 7, default = first(body_fat_lbs)),
         cume_chg_body_fat_lbs = cumsum(dly_chg_body_fat_lbs),
         wkly_avg_body_fat_lbs = ma(body_fat_lbs, n = 7)
         )


  mutate(dly_chg_body_fat_pct = body_fat_pct - lag(body_fat_pct, default = first(body_fat_pct)),
         wkly_chg_body_fat_pct = body_fat_lbs - lag(body_fat_lbs, n = 7, default = first(body_fat_lbs)),
         cume_chg_body_fat_pct,
         wkly_avg_body_fat_pct)

View(biometric_analysis_data)
         wkly_chg_body_fat_lbs,
         cume_chg_body_fat_lbs,
         wkly_avg_body_fat_lbs) |>
  mutate(dly_chg_body_fat_pct,
         wkly_chg_body_fat_pct,
         cume_chg_body_fat_pct,
         wkly_avg_body_fat_pct) |>
  mutate(dly_chg_muscle_mass_lbs,
         wkly_chg_muscle_mass_lbs,
         cume_chg_muscle_mass_lbs,
         wkly_avg_muscle_mass_lbs) |>
  mutate(dly_chg_muscle_mass_pct,
         wkly_chg_muscle_mass_pct,
         cume_chg_muscle_mass_pct) |>
  mutate(dly_chg_sleep_hrs,
         wkly_chg_sleep_hrs,
         wkly_avg_sleep_hrs,
         cume_avg_sleep_hrs)
  )


nutrition_data = nutrition_data |>
  mutate(unsat_fat_g = (total_fat_g - sat_fat_g),
         fat_pct = ((total_fat_g*9)/caloric_intake_kcal),
         carb_pct = ((total_carb_g*4)/caloric_intake_kcal),
         protein_pct = ((protein_g*4)/caloric_intake_kcal),
         non_fiber_complex_carb_g = (total_carb_g - fiber_g - sugar_g))
View(nutrition_data)

################################################################################
## Combine datasets
################################################################################

full_dataset = biometric_data |>
  full_join(nutrition_data, by = join_by(date)) |>
  full_join(exercise_data, by = join_by(date)) |>
  full_join(performance_data, by = join_by(date))
View(full_dataset)

################################################################################
## Simple Graphical Analysis
################################################################################

## biometric_data

  ## Weight Over Time
ggplot(data = biometric_data) +
  geom_point(mapping = aes(x = date, y = weight_lbs),
             color = "darkblue",
             size = 2) +
  labs(title = "Weight Over Time",
       x = "Date (yyyy-mm-dd)",
       y = "Weight (lbs)") +
  scale_x_date(date_breaks = "3 days") +
  scale_y_continuous(n.breaks = 7) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(angle = 30))

  ## Body Fat Over Time
ggplot(data = biometric_data) +
  geom_point(mapping = aes(x = date, y = body_fat_lbs),
             color = "darkred",
             size = 2) +
  labs(title = "Body Fat Over Time",
       x = "Date (yyyy-mm-dd)",
       y = "Body Fat (lbs)",
       ) +
  scale_x_date(date_breaks = "3 days") +
  scale_y_continuous(n.breaks = 7) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(angle = 30))

  ## Muscle Mass Over Time
ggplot(data = biometric_data) +
  geom_point(mapping = aes(x = date, y = muscle_mass_lbs),
             color = "darkgreen",
             size = 2) +
  labs(title = "Muscle Mass Over Time",
       x = "Date (yyyy-mm-dd)",
       y = "Muscle Mass (lbs)",
  ) +
  scale_x_date(date_breaks = "3 days") +
  scale_y_continuous(n.breaks = 7) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(angle = 30))

  ## Bone Mass Over Time
ggplot(data = biometric_data) +
  geom_point(mapping = aes(x = date, y = bone_mass_lbs),
             color = "darkred",
             size = 2) +
  labs(title = "Bone Mass Over Time",
       x = "Date (yyyy-mm-dd)",
       y = "Bone Mass (lbs)",
  ) +
  scale_x_date(date_breaks = "3 days") +
  scale_y_continuous(n.breaks = 7) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(angle = 30))

  ## Sleep Over Time
ggplot(data = biometric_data) +
  geom_point(mapping = aes(x = date, y = sleep_hrs),
             color = "darkred",
             size = 2) +
  labs(title = "Sleep Over Time",
       x = "Date (yyyy-mm-dd)",
       y = "Sleep (hrs)",
  ) +
  scale_x_date(date_breaks = "3 days") +
  scale_y_continuous(n.breaks = 7) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5),
        axis.text.x = element_text(angle = 30))

################################################################################
## Regression Analysis
################################################################################

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


