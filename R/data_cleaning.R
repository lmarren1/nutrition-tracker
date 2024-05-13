################################################################################
## Import libraries
################################################################################

library(tidyverse)
library(ggplot2)
library(plotly)

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
                            day = 1:30,
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

## graph over time
time_series_graph = function(data = biometric_data, y = biometric_data$weight_lbs,
                      title = "Weight Over Time", y_label = "Weight (lbs)") {
  ggplot(data = data) +
    geom_point(mapping = aes(x = date, y = y),
               color = "darkblue",
               size = 2) +
    labs(title = title,
         x = "Date (yyyy-mm-dd)",
         y = y_label) +
    scale_x_date(date_breaks = "3 days") +
    scale_y_continuous(n.breaks = 7) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "italic", hjust = 0.5),
          axis.text.x = element_text(angle = 30))
}

## Change Over Time
chg_over_time = function(x, days = 1) {
  return(x - lag(x, n = days, default = first(x)))
}

## Average Over Time
avg_over_time = function(x, days = 1) {
  for (i in seq(from = 1, to = days)) {
    y = y + lag(x, n = i, default = first(x))
  }
  return(y/7)
}

################################################################################
## Create analysis dfs
################################################################################

  ## add body_fat & percentages
biometric_data = biometric_data |>
  mutate(body_fat_lbs = (weight_lbs - muscle_mass_lbs - bone_mass_lbs),
         body_fat_pct = (body_fat_lbs/weight_lbs),
         muscle_mass_pct = (muscle_mass_lbs/weight_lbs),
         bone_mass_pct = (bone_mass_lbs/weight_lbs))
View(biometric_data)

  ## **NEW DF** <- add dly_chg, wkly_chg, cum_chg, wkly_avg
biometric_analysis_data = biometric_data |>
  ## weight
  mutate(dly_chg_weight_lbs = chg_over_time(weight_lbs),
         wkly_chg_weight_lbs = chg_over_time(weight_lbs, days = 7),
         cume_chg_weight_lbs = cumsum(weight_lbs),
         wkly_avg_weight_lbs = avg_over_time(weight_lbs, days = 7)) |>
  ## body_fat_lbs
  mutate(dly_chg_body_fat_lbs = chg_over_time(body_fat_lbs),
         wkly_chg_body_fat_lbs = chg_over_time(body_fat_lbs, days = 7),
         cume_chg_body_fat_lbs = cumsum(dly_chg_body_fat_lbs),
         wkly_avg_body_fat_lbs = avg_over_time(body_fat_lbs, days = 7)) |>
  ## body_fat_pct
  mutate(dly_chg_body_fat_pct = chg_over_time(body_fat_pct),
         wkly_chg_body_fat_pct = chg_over_time(body_fat_pct, days = 7),
         cume_chg_body_fat_pct = cumsum(dly_chg_body_fat_pct),
         wkly_avg_body_fat_pct = avg_over_time(body_fat_pct, days = 7)) |>
  ## muscle_mass_lbs
  mutate(dly_chg_muscle_mass_lbs = chg_over_time(muscle_mass_lbs),
         wkly_chg_muscle_mass_lbs = chg_over_time(muscle_mass_lbs, days = 7),
         cume_chg_muscle_mass_lbs = cumsum(muscle_mass_lbs),
         wkly_avg_muscle_mass_lbs = avg_over_time(muscle_mass_lbs, days = 7)) |>
  ## muscle_mass_pct
  mutate(dly_chg_muscle_mass_pct = chg_over_time(muscle_mass_pct),
         wkly_chg_muscle_mass_pct = chg_over_time(muscle_mass_pct, days = 7),
         cume_chg_muscle_mass_pct = cumsum(muscle_mass_pct),
         wkly_avg_muscle_mass_pct = avg_over_time(muscle_mass_pct, days = 7)) |>
  ## sleep_hrs
  mutate(dly_chg_sleep_hrs = chg_over_time(sleep_hrs),
         wkly_chg_sleep_hrs = chg_over_time(sleep_hrs, days = 7),
         cume_avg_sleep_hrs = cumsum(dly_chg_sleep_hrs),
         wkly_avg_sleep_hrs = avg_over_time(sleep_hrs, days = 7))
View(biometric_analysis_data)

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
time_series_graph() |>
  ggplotly()

  ## Body Fat Over Time
time_series_graph(y = biometric_data$body_fat_lbs,
           title = "Body Fat Over Time", y_label = "Body Fat (lbs)") |>
  ggplotly()

  ## Muscle Mass Over Time
time_series_graph(y = biometric_data$muscle_mass_lbs,
           title = "Muscle Mass Over Time", y_label = "Muscle Mass (lbs)") |>
  ggplotly()

  ## Bone Mass Over Time
time_series_graph(y = biometric_data$bone_mass_lbs,
           title = "Bone Mass Over Time", y_label = "Bone Mass (lbs)") |>
  ggplotly()

  ## Sleep Over Time
time_series_graph(y = biometric_data$sleep_hrs,
           title = "Sleep Over Time", y_label = "Sleep (hrs)") |>
  ggplotly()

graph = time_series_graph(data = biometric_analysis_data, y = biometric_analysis_data$dly_chg_weight_lbs) +
  geom_point(mapping = aes(x = date, y = dly_chg_body_fat_lbs))
  ggplotly(graph)



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


