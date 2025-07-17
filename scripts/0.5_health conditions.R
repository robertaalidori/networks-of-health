## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Healthh conditions
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load data.
load("./data/egor_socnet.rda")

library(ggplot2)
library(stringr)
library(dplyr)
library(forcats)

unique(egor_socnet$ego$G1)

# Convert G1 to numeric for regression analysis
egor_socnet$ego <- egor_socnet$ego |>
  mutate(G1 = as.character(G1))  # convert from factor to character

# Trim white spaces
egor_socnet$ego <- egor_socnet$ego |>
  mutate(G1 = str_trim(G1))

# Convert G1 to numeric values

egor_socnet$ego <- egor_socnet$ego |>
  mutate(
    g1_numeric = case_when(
      str_detect(G1, "Excellent") ~ 1,
      str_detect(G1, "Very Good") ~ 2,
      str_detect(G1, "Good") ~ 3,
      str_detect(G1, "Fair") ~ 4,
      str_detect(G1, "Poor") ~ 5,
      TRUE ~ NA_real_
    )
  )

# Check the conversion
egor_socnet$ego |>
  count(G1, g1_numeric)

# Visualise across race
race_health_plot <-egor_socnet$ego |>
  group_by(race) |>
  summarise(
    mean_health = mean(g1_numeric, na.rm = TRUE),
    se = sd(g1_numeric, na.rm = TRUE) / sqrt(n())
  ) |>
  ggplot(aes(x = race, y = mean_health, fill = race)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_health - se, ymax = mean_health + se), width = 0.2) +
  labs(
    title = "Average Health by Race",
    y = "Mean Health (1 = Excellent, 5 = Poor)",
    x = "Race"
  ) +
  theme_minimal()

# Save the plot
ggsave("outputs/average_health_by_race.png", plot = race_health_plot, width = 7, height = 5, dpi = 300)

# Chronic conditions (G7S1 - G7S6, G7S9)
# Select and recode G7 variables as numeric (0 = No, 1 = Yes), merge it with race
g7_vars <- egor_socnet$ego |>
  select(.egoID, race, starts_with("G7S")) |>
  mutate(across(starts_with("G7S"),
                ~ case_when(
                  str_detect(., "1 Yes") ~ 1,
                  str_detect(., "0 No") ~ 0,
                  TRUE ~ NA_real_
                )
  ))

# Check the conversion
g7_vars |>
  pivot_longer(cols = starts_with("G7S"), names_to = "var", values_to = "numeric_value") |>
  count(var, numeric_value)

# Save chronic conditions variable
saveRDS(g7_vars, file = "data/g7_vars.rds")

# Change label and compute the proportion of each race group reporting each condition
g7_race <- g7_vars |>
  pivot_longer(cols = starts_with("G7S"), names_to = "condition", values_to = "has_condition") |>
  mutate(condition_label = case_when(
    condition == "G7S1" ~ "High Blood Pressure",
    condition == "G7S2" ~ "Diabetes",
    condition == "G7S3" ~ "Heart Disease",
    condition == "G7S4" ~ "Asthma",
    condition == "G7S5" ~ "Arthritis",
    condition == "G7S6" ~ "Depression",
    condition == "G7S9" ~ "None of the Above",
    TRUE ~ condition  # fallback
  )) |>
  group_by(race, condition_label) |>
  summarise(
    proportion = mean(has_condition, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Check the transformation
count(g7_race, condition_label, sort = TRUE)

# Make sure all combinations of race and condition appear, even if no ego in that race group reported that condition
g7_race_complete <- g7_race |>
  complete(race, condition_label, fill = list(proportion = 0))

# Plot the Proportions
ggplot(g7_race_complete, aes(x = race, y = proportion, fill = race)) +
  geom_col(position = "dodge") +
  facet_wrap(~ condition_label) +
  labs(
    title = "Proportion of Health Conditions by Race",
    y = "Proportion with Condition",
    x = "Race"
  ) +
  theme_minimal()

# Save the plot
ggsave("outputs/g7_health_conditions_by_race.pdf", plot = g7_plot, width = 10, height = 6, dpi = 300)





