
library(janitor)
library(tidyverse)

#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')

my_data <- read.csv("data/CensusAtSchool2026.csv", na.strings = c("", "NA"))


#Set up some quick tabulations
table(my_data$state, useNA = "always")
table(my_data$gender, useNA = "always")
table(my_data$handed, useNA = "always")
table(my_data$planned_education_level, useNA = "always")
table(my_data$gender, my_data$handed)
table(my_data$planned_education_level, my_data$gender)
  #3-way categorical table
ftable(my_data$planned_education_level, my_data$gender, my_data$handed)


#
my_data |>
  summarize(
    mean_grade   = mean(classgrade, na.rm = TRUE),
    median_grade = median(classgrade, na.rm = TRUE),
    sd_grade     = sd(classgrade, na.rm = TRUE),
    mean_langs   = mean(languages_spoken, na.rm = TRUE),
    median_langs = median(languages_spoken, na.rm = TRUE),
    sd_langs     = sd(languages_spoken, na.rm = TRUE),
  )

my_data |>
  group_by(gender, na.rm = FALSE) |>
  summarize(
    total_obs    = n(),
    mean_grade   = mean(classgrade, na.rm = TRUE),
    median_grade = median(classgrade, na.rm = TRUE),
    sd_grade     = sd(classgrade, na.rm = TRUE),
    valid_grade  = sum(!is.na(classgrade)),
    mean_langs   = mean(languages_spoken, na.rm = TRUE),
    median_langs = median(languages_spoken, na.rm = TRUE),
    sd_langs     = sd(languages_spoken, na.rm = TRUE),
    valid_langs  = sum(!is.na(languages_spoken))
  )

#Plotting Graphs: 1-way plots
ggplot(data = subset(my_data, !is.na(favorite_season)), aes(x = favorite_season)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Favorite Season", x = "Season", y = "Count") +
  theme_minimal()

ggplot(my_data, aes(x = travel_time_to_school)) +
  geom_histogram(binwidth = 5, fill = "darkred", color = "white") +
  labs(title = "Distribution of School Travel Time", x = "Travel Time (minutes)", y = "Count") +
  theme_minimal()


#Plotting Graphs: 2-way plots
ggplot(data = subset(my_data, !is.na(gender)), aes(x = classgrade, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Class Grade Breakdown by Gender", x = "Class Grade", y = "Count") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

ggplot(data = subset(my_data, !is.na(gender)), aes(x = gender, y = travel_time_to_school, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Travel Time to School by Gender", x = "Gender", y = "Travel Time (minutes)") +
  theme_minimal()


ggplot(my_data, aes(x = travel_time_to_school, y = text_messages_sent_yesterday)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE, linetype = "dashed") +
  labs(title = "Travel Time to School vs. Texts Sent Yesterday", 
       x = "Travel Time (minutes)",
       y = "Text Messages Sent Yesterday") +
  theme_minimal()


ggplot(data = subset(my_data, !is.na(favorite_school_subject)), aes(x = travel_time_to_school, y = text_messages_sent_yesterday)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "green", se = FALSE, linetype = "solid") +
  facet_wrap(~ favorite_school_subject) + 
  labs(
    title = "Travel Time to School vs. TExts Sent",
    subtitle = "Faceted by Student's Favorite School Subject",
    x = "Travel Time (minutes)",
    y = "Text Messages Sent Yesterday"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA), # Soft background for panel labels
    strip.text = element_text(face = "bold")                    # Bold headers for each subject
  )
