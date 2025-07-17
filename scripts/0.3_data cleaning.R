## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Cleaning data
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load packages.
library(tidyverse)
library(skimr)
library(janitor)
library(egor)

# Load data.
load("./data/egor_obj.rda")


view(egor_obj$ego$RACECATS1)

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Race categories
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Summary of race category.
egor_obj$ego |> 
  count(RACECATS1) |> 
  mutate(percent = round(100 * n / sum(n), 1))

# Group some of race categories into a simplified variable: 
## Black: black, American Indian, Hawaiian Pac Islander
## White: white
## Asian: Asian
## Mixed: mixed race
## Unknown: Unknown
egor_obj$ego <- egor_obj$ego |>
  mutate(race = case_when(
    RACECATS1 %in% c("(2) black", "(3) American Indian", "(7) Hawaiian Pac Islander") ~ "Black",
    RACECATS1 == "(1) white"        ~ "White",
    RACECATS1 == "(4) Asian"        ~ "Asian",
    RACECATS1 == "(9) mixed race"   ~ "Mixed",
    RACECATS1 == "(6) Unknown"      ~ "Unknown",
    TRUE                            ~ NA_character_
  ))

# Check the transformation
egor_obj$ego |> 
  count(race) |> 
  mutate(percent = round(100 * n / sum(n), 1))

# Remove egos with "Unknown" race
ego_filtered <- egor_obj$ego |> 
  filter(race != "Unknown")


# Filter alters and ties based on the filtered egos
alter_filtered <- egor_obj$alter |>
  semi_join(ego_filtered, by = c(".egoID"))


ties_filtered <- egor_obj$aatie |>
  semi_join(ego_filtered, by = c(".egoID"))


# Create a new egor object with filtered data
egor_filtered <- egor::egor(
  egos = ego_filtered,
  alters = alter_filtered,
  aaties = ties_filtered,
  ID.vars = list(
    ego = ".egoID",
    alter = ".altID",    
    source = ".srcID",     
    target = ".tgtID"
  )
)

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Type of support
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Create a new egor object with additional variables for social support and strain typology (If B6B... is not NA, then the alter does provide practical support. 
# Don’t care if the value is 1, 2, ..., 6 as it just indicates rank/order, not absence of support. For D1L  values are 1 (definitely would to 4 definitely would not)
# So D1L codified as 1 or 2 → Yes (demands help) 3 or 4 → No. Something else → Missing/invalid

egor_support <- egor(
  egos = ego_filtered,
  alters = alter_filtered |>
    mutate(
      practical_support = if_else(!is.na(B6B), 1, 0),
      emergency_support = if_else(!is.na(B7C), 1, 0),
      emotional_support = if_else(!is.na(B4A), 1, 0),
      
      appraisal_support = if_else(!is.na(B5A), 1, 0),
      conflict_tie      = if_else(!is.na(B9A), 1, 0),
      burdensome_tie    = if_else(!is.na(B8A), 1, 0),
      demands_help      = case_when(
        D1L %in% c(1, 2) ~ 1,
        D1L %in% c(3, 4) ~ 0,
        TRUE ~ NA_real_
      )
    ),
  aaties = ties_filtered,
  ID.vars = list(
    ego = ".egoID",
    alter = ".altID",
    source = ".srcID",
    target = ".tgtID"
  )
)

# Check the structure of the new egor object
egor_support$alter |>
  select(practical_support, demands_help) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  count(variable, value)

egor_support$alter |>
  count(B6B_is_na = is.na(B6B), practical_support)

egor_support$alter |>
  count(D1L, demands_help)


# Save the new egor object 
save(egor_support, file = "./data/egor_support.rda")

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Type of relationship
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Check values in category with type of relationship (values of C1A_variables)
egor_support$alter |>
  select(starts_with("C1A_")) |>
  summarise(across(everything(), ~ toString(unique(.))))

egor_support$alter |>
  select(starts_with("C1A_")) |>
  summarise(across(everything(), ~ list(unique(.))))

# Convert C1A_ variables to numeric values
egor_support$alter <- egor_support$alter |>
mutate(across(starts_with("C1A_"),
              ~ case_when(
                str_trim(.) == "(1) yes" ~ 1,
                str_trim(.) == "(0) no" ~ 0,
                str_trim(.) == "(9) 9" ~ NA_real_,
                TRUE ~ NA_real_
              )
))

# Check conversion (the class of C1A_ variables)
egor_support$alter |>
  select(starts_with("C1A_")) |>
  summarise(across(everything(), class))

view(egor_support$alter$C1A_1)

# Check for overlaps between categories in variables C1A_
egor_support$alter |>
  filter(C1A_3 == 1 & C1A_23 == 1) |>
  count()

egor_support$alter |>
  filter(C1A_4 == 1 & C1A_24 == 1) |>
  count()

egor_support$alter |>
  filter(C1A_5 == 1 & C1A_26 == 1) |>
  count()

egor_support$alter |>
  filter(C1A_10 == 1 & C1A_20 == 1) |>
  count()

egor_support$alter |>
  filter(C1A_10 == 1 & C1A_21 == 1) |>
  count()

# Create a new variable for type of relationship 
# immediate family: (step)parents, (step)children, (step)siblings; 
# extended family: other relatives;
# friends: only those categorized as friends
# acquaintances: all the others

egor_support$alter <- egor_support$alter |>
  mutate(
    rel_immediate = rowSums(across(c(C1A_1, C1A_2, C1A_3, C1A_4, C1A_5, C1A_23, C1A_24, C1A_25, C1A_26)), na.rm = TRUE),
    rel_extended  = C1A_6,
    rel_friends   = C1A_20,
    
    relation_category = case_when(
      rel_immediate > 0 ~ "Immediate Family",
      rel_extended == 1 ~ "Extended Family",
      rel_friends == 1 ~ "Friends",
      TRUE ~ "Acquaintances"
    )
  )

# Check the distribution of the new variable
egor_supnet <- egor_support$alter |>
  count(relation_category) |>
  mutate(percent = round(100 * n / sum(n), 1))


