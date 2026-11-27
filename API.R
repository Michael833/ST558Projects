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
CensusSchool <- read_csv("CensusSchool.csv") |>
  filter(!is.na(text_messages_sent_yesterday)) |>
  mutate(
    across(c(classgrade, gender, favorite_school_subject, role_model_type), as.factor)
  )

# 2. Set defaults for each value. These will be means/modes for numeric/categorical vars, respectively.






# 3. Set up the model I'm passing through from my Project3.qmd file investigation





# 4. Set u the API function




# 5. Create the API endpoints





# 6. 
