## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Measures of network compositions
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

# Create a new egor object with additional variables for social support and strain typology using name generators and R
egor_support <- egor(
  egos = ego_filtered,
  alters = alter_filtered |>
    mutate(
      practical_support = case_when(B6B == "1" ~ 1, TRUE ~ 0),
      emergency_support = case_when(B7C == "1" ~ 1, TRUE ~ 0),
      emotional_support = case_when(B4A == "1" ~ 1, TRUE ~ 0),
      appraisal_support = case_when(B5A == "1" ~ 1, TRUE ~ 0),
      conflict_tie      = case_when(B9A == "1" ~ 1, TRUE ~ 0),
      burdensome_tie    = case_when(B8A == "1" ~ 1, TRUE ~ 0),
      demands_help = case_when(
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

# Save the new egor object with support and strain variables
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

egor_support$alter <- egor_support$alter |>
mutate(across(starts_with("C1A_"),
              ~ case_when(
                str_trim(.) == "(1) yes" ~ 1,
                str_trim(.) == "(0) no" ~ 0,
                str_trim(.) == "(9) 9" ~ NA_real_,
                TRUE ~ NA_real_
              )
))


# Check the class of C1A_ variables
egor_support$alter |>
  select(starts_with("C1A_")) |>
  summarise(across(everything(), class))

view(egor_support$alter$C1A_1)

# Check for overlaps between categories 
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

# Create a new variable for type of relationship (Immediate family, extended family, friends, acquaintances, other)
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
egor_support$alter |>
  count(relation_category) |>
  mutate(percent = round(100 * n / sum(n), 1))


# Save the new egor object with type of relationship variable
save(egor_support, file = "./data/egor_socnet.rda")
