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


# 1. Starting by pulling in my dataset, CensusSchool. 
CensusSchool <- read_csv("data/CensusAtSchool2026.csv") |>
  tidyr::drop_na(
    text_messages_sent_yesterday, doing_homework_hours, classgrade, gender, ageyears,
    travel_time_to_school, favorite_school_subject, home_occupants)


# 2. Set defaults for each value. These will be means/modes for numeric/categorical vars, respectively.
get_mode <- function(dsvar) {
  unqvar <- unique(na.omit(dsvar))
  unqvar[which.max(tabulate(match(dsvar, unqvar)))]
}
default_ageyears    <- mean(CensusSchool$ageyears)
default_travel_time <- mean(CensusSchool$travel_time_to_school)
default_occupants   <- mean(CensusSchool$home_occupants)

default_classgrade  <- get_mode(CensusSchool$classgrade)
default_gender      <- get_mode(CensusSchool$gender)
default_subject     <- get_mode(CensusSchool$favorite_school_subject)
default_role_model  <- get_mode(CensusSchool$role_model_type)


# 3. Set up the model I'm passing through from my Project3.qmd file investigation
final_rec <- recipe(
  text_messages_sent_yesterday ~ classgrade + gender + ageyears + 
    travel_time_to_school + favorite_school_subject + home_occupants + 
    role_model_type, data = CensusSchool) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())


# 4. Tinker with model paramters to match the best model I created in the QMD file.:
final_spec <- rand_forest(mtry = 3, min_n = 10, trees = 500) |>
  set_engine("ranger") |>
  set_mode("regression")
  # Final Workflow
final_fit <- workflow() |>
  add_recipe(final_rec) |>
  add_model(final_spec) |>
  fit(data = CensusSchool)


# 5. Set up the main API function
#* @apiTitle Michael's ST 558 Landing Page
#* @apiDescription Project 4 API Assignment Page

#* Predict text messages sent yesterday
#* @param classgrade Student grade level
#* @param gender Student gender
#* @param ageyears Student age in years
#* @param travel_time_to_school Travel time to school in minutes
#* @param favorite_school_subject Favorite school subject
#* @param home_occupants Number of people living in home
#* @param role_model_type Type of role model
#* @get /pred
function(
    #Inupt default values
    ageyears = default_ageyears,
    travel_time_to_school = default_travel_time,
    home_occupants = default_occupants,
    classgrade = default_classgrade,
    gender = default_gender,
    favorite_school_subject = default_subject,
    role_model_type = default_role_model
) {
  #Create values that I can feed into the model
  prediction_CensusSchool <- tibble(
    ageyears = as.numeric(ageyears),
    travel_time_to_school = as.numeric(travel_time_to_school),
    home_occupants = as.numeric(home_occupants),
    classgrade = factor(classgrade, levels = levels(CensusSchool$classgrade)),
    gender = factor(gender, levels = levels(CensusSchool$gender)),
    favorite_school_subject = factor(favorite_school_subject, levels = levels(CensusSchool$favorite_school_subject)),
    role_model_type = factor(role_model_type, levels = levels(CensusSchool$role_model_type))
  )
  
  pred <- predict(final_fit, new_data = prediction_CensusSchool)
  
  list(
    predicted_text_messages = round(pred$.pred, 2),
    inputs_used = prediction_CensusSchool
  )
}
#http://localhost:8000/pred?--parameter=num1&parameter=num2--
#http://127.0.0.1:14306/pred?classgrade=12&gender=Male&ageyears=16&travel_time_to_school=14&favorite_school_subject=Mathematics%20and%20statistics&home_occupants=4&role_model_type=Relative


# 6. Set up the who-and-where API
#* @param role_model_type Type of role model
#* @get /info
function(){
  list(
    name = "Michael Costello",
    website = "https://michael833.github.io/ST558Projects/"
  )
}
#http://localhost:8000/info
#http://127.0.0.1:14306/info