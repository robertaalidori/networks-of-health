## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Regression
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load data
load("./data/egor_socnet.rda")
g7_vars <- readRDS("data/g7_vars.rds")
ego_tie_props <- readRDS("data/ego_tie_props.rds")

# Load packages
library(broom)
library(ggplot2)
library(stringr)
library(dplyr)
library(forcats)
library(readr)
library(tidyr)


# Count chronic conditions and ensure race is included
ego_health <- g7_vars |>
  mutate(n_conditions = rowSums(across(starts_with("G7S")), na.rm = TRUE)) |>
  left_join(egor_socnet$ego |> select(.egoID, race), by = ".egoID")

# Check the count of conditions
ego_health |>
  count(n_conditions)

# Merge with tie proportions
ego_health_net <- ego_health |>
  left_join(ego_tie_props, by = ".egoID")

# Select and clean control variables
levels(egor_socnet$ego$GENDER)
levels(egor_socnet$ego$K21B)
levels(egor_socnet$ego$K1)
levels(egor_socnet$ego$AGEGROUP)

ego_controls <- egor_socnet$ego |>
  select(
    .egoID,
    AGEGROUP,
    K21B,      # include for income recoding
    K1,        # include for education recoding
    GENDER     # include for gender recoding
  ) |>
  mutate(
    gender = fct_recode(GENDER,
                        "Male" = "(1)  1 Male ",
                        "Female" = "(2)  2 Female"),
    
    family_income = fct_recode(K21B,
                               "Lower" = "(1)  1 Lower ",
                               "Higher" = "(2)  2 Higher ",
                               "Around_80k" = "(3)  3 About $80,000"),
    
    education_level = case_when(
      K1 %in% c("(01) 1 Less than 9th grade", "(02) 2 9th grade to 12th grade, but did not graduate") ~ "Low",
      K1 %in% c("(03) 3 High school graduate","(04) 4 GED or equivalent", "(05) 5 Some college") ~ "Medium",
      K1 %in% c("(06) 6 Associate degree", "(07) 7 Bachelors degree", "(08) 8 Masters degree", 
                "(09) 9 Higher professional degree (like MD, JD, or PhD) ") ~ "High",
      K1 == "(10) 10 Other: ~k1_other" ~ NA_character_),
    
    age_group = fct_recode(AGEGROUP, 
                               "21-30" = "(0)  21-30 ",
                               "50-70" = "(1)  50-70"),
    
    education_level = factor(education_level, levels = c("Low", "Medium", "High"))
  )

# Join ego_health_net with ego_controls
ego_health_net_controls <- ego_health_net |>
  left_join(ego_controls, by = ".egoID")


# Set reference categories
ego_controls <- ego_controls |>
  mutate(
    gender = relevel(factor(gender), ref = "Male"),
    education_level = relevel(factor(education_level), ref = "Medium"),
    family_income = relevel(factor(family_income), ref = "Around_80k"),
    age_group = relevel(factor(age_group), ref = "50-70")  
  )

# Drop Missing values
ego_health_net_controls_clean <- ego_health_net_controls |>
  drop_na(n_conditions, race, p_positive, p_negative,
          gender, age_group, education_level, family_income)

# Run the regression model with multiplex ties
model_multiplex <- lm(n_conditions ~ race + p_positive + p_negative +
                                          gender + age_group + education_level + family_income,
                                        data = ego_health_net_controls)
summary(model_multiplex)

# Check for missing values
ego_health_net_controls |>
  summarise(across(c(n_conditions, race, p_positive, p_negative, gender, age_group, education_level, family_income), ~ sum(is.na(.))))

# Drop family_income
ego_health_net_controls_clean <- ego_health_net_controls |>
  drop_na(n_conditions, race, p_positive, p_negative, gender, age_group, education_level)

# Clean data
ego_health_net_controls_clean <- ego_health_net_controls |>
  drop_na(n_conditions, race, p_positive, p_negative,
          gender, age_group, education_level)

# Run the regression model
model_multiplex <- lm(n_conditions ~ race + p_positive + p_negative +
                        gender + age_group + education_level,
                      data = ego_health_net_controls_clean)

summary(model_multiplex)

# Set white as reference category
ego_health_net_controls <- ego_health_net_controls |>
  mutate(
    race = relevel(factor(race), ref = "White")
  )

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Multiplex ties
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Run the regression model with multiplex ties
ego_health_net_controls_clean <- ego_health_net_controls |>
  drop_na(n_conditions, race, p_positive, p_negative,
          gender, age_group, education_level)

model_multiplex <- lm(
  n_conditions ~ race * p_positive + race * p_negative +
    gender + age_group + education_level,
  data = ego_health_net_controls
)
summary(model_multiplex)

# Improve visualization of results
tidy(model_multiplex)

View(tidy(model_multiplex))

# Tidy the model into a data frame
model_multiplex_tidy <- tidy(model_multiplex) |>
  mutate(across(where(is.numeric), round, 4))

# Save the tidy model to CSV
write_csv(model_multiplex_tidy, "outputs/model_multiplex_raw.csv")

# Filter key interaction terms and results
multiplex_interaction <- model_multiplex_tidy |>
  filter(term %in% c(
    "raceBlack:p_negative", "raceMixed:p_negative", "raceAsian:p_negative",
    "raceBlack:p_positive", "raceMixed:p_positive", "raceAsian:p_positive"
  )) |>
  select(term, estimate, statistic, p.value) |>
  rename(Estimate = estimate,
         `t value` = statistic,
         `p-value` = p.value)


print(multiplex_interaction)



# Clean labels for readability
multiplex_interaction <- multiplex_interaction |>
  mutate(term_label = recode(term,
                             "raceBlack:p_negative" = "Black × Negative Ties",
                             "raceMixed:p_negative" = "Mixed × Negative Ties",
                             "raceAsian:p_negative" = "Asian × Negative Ties",
                             "raceBlack:p_positive" = "Black × Positive Ties",
                             "raceMixed:p_positive" = "Mixed × Positive Ties",
                             "raceAsian:p_positive" = "Asian × Positive Ties"
  ))

#Save important results
write_csv(multiplex_interaction, "outputs/model_multiplex_clean.csv")

# Plot
ggplot(multiplex_interaction, aes(x = term_label, y = Estimate, fill = term_label)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0("β = ", round(Estimate, 2), 
                               "\nt = ", round(`t value`, 2), 
                               "\np = ", round(`p-value`, 3))),
            vjust = ifelse(multiplex_interaction$Estimate > 0, -0.8, 1.8),
            size = 4) +
  labs(
    title = "Race × Tie Type Interactions on Chronic Conditions",
    x = NULL,
    y = "Effect Estimate (β)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Save the plot as a PDF
ggsave("outputs/multiplex_plot.pdf", width = 9, height = 5)

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Themed ties
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Drop missing data
ego_health_net_controls_clean <- ego_health_net_controls |>
  drop_na(n_conditions, race, p_ambivalent, p_demanding, p_difficult, p_reciprocal,
          gender, age_group, education_level)

# Run the model with race × themed ties
model_themes <- lm(
  n_conditions ~ race * p_ambivalent + race * p_demanding +
    race * p_difficult + race * p_reciprocal +
    gender + age_group + education_level,
  data = ego_health_net_controls_clean
)
summary(model_themes)

# Tidy results
model_themes_tidy <- tidy(model_themes) |>
  mutate(across(where(is.numeric), round, 4))

view(model_themes_tidy)

# Save the raw model to CSV
write_csv(model_themes_tidy, "outputs/model_themes_raw.csv")

# Filter key interaction terms and results
theme_interaction <- model_themes_tidy |>
  filter(str_detect(term, "^race.*:p_")) |>
  select(term, estimate, statistic, p.value) |>
  rename(Estimate = estimate,
         `t value` = statistic,
         `p-value` = p.value)

# Clean labels for readability
theme_interaction <- theme_interaction |>
  mutate(term_label = recode(term,
                             "raceBlack:p_ambivalent" = "Black × Ambivalent",
                             "raceMixed:p_ambivalent" = "Mixed × Ambivalent",
                             "raceAsian:p_ambivalent" = "Asian × Ambivalent",
                             "raceBlack:p_demanding" = "Black × Demanding",
                             "raceMixed:p_demanding" = "Mixed × Demanding",
                             "raceAsian:p_demanding" = "Asian × Demanding",
                             "raceBlack:p_difficult" = "Black × Difficult",
                             "raceMixed:p_difficult" = "Mixed × Difficult",
                             "raceAsian:p_difficult" = "Asian × Difficult",
                             "raceBlack:p_reciprocal" = "Black × Reciprocal",
                             "raceMixed:p_reciprocal" = "Mixed × Reciprocal",
                             "raceAsian:p_reciprocal" = "Asian × Reciprocal"
  ))

View(theme_interaction)

# Subset significant terms
theme_significant <- theme_interaction |>
  filter(term %in% c("raceMixed:p_ambivalent", "raceBlack:p_reciprocal"))

# Custom readable labels
theme_significant <- theme_significant |>
  mutate(term_label = recode(term,
                             "raceMixed:p_ambivalent" = "Mixed × Ambivalent",
                             "raceBlack:p_reciprocal" = "Black × Reciprocal"
  ))

View(theme_significant)

# Save
write_csv(theme_significant, "outputs/model_themes_clean.csv")

# Compute confidence intervals
model_themes_tidy <- tidy(model_themes, conf.int = TRUE) |>
  mutate(across(where(is.numeric), round, 4))

# Define terms to highlight
highlight_terms <- c("raceMixed:p_ambivalent", "raceBlack:p_reciprocal")

theme_highlight <- model_themes_tidy |>
  mutate(
    is_highlight = term %in% highlight_terms,
    term_label = case_when(
      term == "raceMixed:p_ambivalent" ~ "Mixed × Ambivalent",
      term == "raceBlack:p_reciprocal" ~ "Black × Reciprocal",
      TRUE ~ term
    )
  )
ggplot(theme_highlight, aes(x = estimate, y = reorder(term_label, estimate))) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = is_highlight),
                 height = 0.2) +
  geom_point(aes(color = is_highlight), size = 3) +
  scale_color_manual(values = c("gray60", "#D55E00")) +  # highlighted terms in orange
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Regression Coefficients for Race × Themed Tie Interactions",
    x = "Estimate (β) with 95% CI", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Save the plot
ggsave("outputs/theme_highlight_plot.pdf", plot = theme_plot, width = 9, height = 5)
