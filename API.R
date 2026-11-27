################################################################################
# Title:       API.R
# Purpose:     Setting up the API I will use in the Docker image to allow 
#              my model to be called through.
# Author:      Michael Costello
# Date:        26 November 2026
################################################################################

# 0. Grab packages and get ready to run.
library(plumber)
library(tidyverse)
library(tidymodels)
library(ranger)


# 1. Starting by pulling in my dataset, NHANES. 
NHANES <- read.csv("data/NHANES_EnergyDrinks_BP_ages18-29.csv", na.strings = c("", "NA")) |>
  select(-Cycle, -RecallDays) |>
  tidyr::drop_na(Pulse)

#NHANES |> summarise(across(everything(), \(x) sum(is.na(x))))


# 2. Set defaults for each value. These will be means/modes for numeric/categorical vars, respectively.
get_mode <- function(dsvar) {
  unqvar <- unique(na.omit(dsvar))
  unqvar[which.max(tabulate(match(dsvar, unqvar)))]
}
default_age         <- mean(NHANES$Age)
default_caffeine    <- mean(NHANES$Caffeine_mg)
default_diastolic   <- mean(NHANES$Diastolic_BP)
default_pulse       <- mean(NHANES$Pulse)

default_sex         <- get_mode(NHANES$Sex)
default_energydrink <- get_mode(NHANES$EnergyDrink)


# 3. Set up the model I'm passing through from my Project3.qmd file investigation
final_rec <- recipe(
  Systolic_BP ~ Diastolic_BP + Pulse + Age + Sex + EnergyDrink + Caffeine_mg,
  data = NHANES) |>
  step_dummy(all_nominal_predictors())


# 4. Tinker with model paramters to match the best model I created in the QMD file.:
final_spec <- rand_forest(mtry = 4, min_n = 25, trees = 500) |>
  set_engine("ranger") |>
  set_mode("regression")

  # Final Workflow
final_fit <- workflow() |>
  add_recipe(final_rec) |>
  add_model(final_spec) |>
  fit(data = NHANES)


# 5. Set up the main API function
#* @apiTitle Michael's ST 558 Landing Page
#* @apiDescription Project 3 API Assignment Page

#* Predict systolic BP
#* @param age Patient's age
#* @param sex Patient's sex
#* @param energydrink Patient took energy drink
#* @param caffeine Mgs of caffeine taken by patient
#* @param diastolic Patient's diastolic BP
#* @param pulse Patient's pulse rate

#* @get /pred
function(
    #Inupt default values
    age = default_age,
    sex = default_sex,
    energydrink = default_energydrink,
    caffeine = default_caffeine,
    diastolic = default_diastolic,
    pulse = default_pulse
) {
  #Create values that I can feed into the model
  prediction_NHANES <- tibble(
    Age = as.numeric(age),
    Caffeine_mg = as.numeric(caffeine),
    Diastolic_BP = as.numeric(diastolic),
    Pulse = as.numeric(pulse),
    Sex = factor(sex, levels = levels(NHANES$Sex)),
    EnergyDrink = factor(energydrink, levels = levels(NHANES$EnergyDrink))
  )
  
  pred <- predict(final_fit, new_data = prediction_NHANES)
  
  list(
    predicted_systolic_bp = round(pred$.pred, 2),
    inputs_used = prediction_NHANES
  )
}
#http://localhost:8000/pred?--parameter=num1&parameter=num2--
#http://127.0.0.1:15331/pred?age=23&sex=Female&energydrink=No&caffeine=75&diastolic=69&pulse=77


# 6. Set up the who-and-where API
#* @get /info
function(){
  list(
    name = "Michael Costello",
    website = "https://michael833.github.io/ST558Projects/"
  )
}
#http://localhost:8000/info
#http://127.0.0.1:14306/info

# 7. Start the Plumber API server
pr("API.R") |>
  pr_run(host = "0.0.0.0", port = 8000)