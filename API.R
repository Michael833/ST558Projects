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


# 1. Starting by pulling in my dataset, CensusSchool. 
CensusSchool <- read_csv("data/CensusAtSchool2026.csv") |>
  tidyr::drop_na(
    text_messages_sent_yesterday, doing_homework_hours, classgrade, gender, ageyears,
    travel_time_to_school, favorite_school_subject, home_occupants
  )

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
    travel_time_to_school + favorite_school_subject +
    home_occupants + role_model_type,
  data = CensusSchool
) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

# 4. Set u the API function




# 5. Create the API endpoints





# 6. 
