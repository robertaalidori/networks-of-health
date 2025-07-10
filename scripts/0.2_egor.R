# Load packages
library(tidyverse)
library(igraph)
library(egor)
library(janitor)

# Import data frames.

# Ego attributes
load("./data/ego_df.rda")
# Alter attributes
load("./data/alter_df.rda")

# Convert all character variables to factor
# Using dplyr's across(), this code takes all ego.df variables that are character
# (where(is.character)) and applies the as.factor() function to each, converting 
# it to a factor variable. The resulting data frame is re-assigned to ego.df.

ego.df <- ego_df |> 
  mutate(across(where(is.character), as.factor))

alter.df <- alter_df |> 
  mutate(across(where(is.character), as.factor))

# Create edge list 
tie_vars <- c("N1_N2", "N1_N3", "N1_N4", "N1_N5",
              "N2_N3", "N2_N4", "N2_N5",
              "N3_N4", "N3_N5", "N4_N5")

# Check the classes of the tie variables in alter.df
alter.df |> 
  select(all_of(tie_vars)) |> 
  summarise(across(everything(), ~class(.)))

# Convert the tie variables to numeric, where values are:1 = yes, they know each other; 0 = no, they don’t; NA = we don’t know
alter.df <- alter.df |>
  mutate(across(all_of(tie_vars), ~ case_when(
    . == "(1) very well"      ~ 1,
    . == "(2) know a little"  ~ 1,
    . == "(3) do not know"    ~ 0,
    TRUE                      ~ NA_real_
  )))

# Convert tie columns to long format
alter.ties <- alter.df |>
  select(PRIM_KEY, all_of(tie_vars)) |>
  pivot_longer(
    cols = all_of(tie_vars),
    names_to = "tie",
    values_to = "present"
  ) |>
  filter(!is.na(present) & present == 1) |>  # Only keep known connections
  separate(tie, into = c("source", "target"), sep = "_") |>
  mutate(
    source = as.numeric(sub("N", "", source)),
    target = as.numeric(sub("N", "", target))
  ) |>
  select(PRIM_KEY, source, target)

# Rename alter ID for clarity
alter.df <- alter.df |> 
  rename(alter_id = NAME_NMBR2)


# Create a lookup of valid alter IDs per ego
valid_alters <- alter.df |>
  select(PRIM_KEY, alter_id)

# Filter alter.ties — keep only rows where both source and target exist for that ego
alter.ties.cleaned <- alter.ties |>
  semi_join(valid_alters, by = c("PRIM_KEY", "source" = "alter_id")) |>
  semi_join(valid_alters, by = c("PRIM_KEY", "target" = "alter_id"))

# Check for missing alter IDs
sum(is.na(alter.df$alter_id))

# Regenerate proper alter_ids so each alter is uniquely numbered within their ego’s group.
alter.df <- alter.df |>
  group_by(PRIM_KEY) |>
  mutate(alter_id = row_number()) |>
  ungroup()

# Rebuild alter.ties
alter.ties <- alter.df |>
  select(PRIM_KEY, all_of(tie_vars)) |>
  pivot_longer(
    cols = all_of(tie_vars),
    names_to = "tie",
    values_to = "present"
  ) |>
  filter(!is.na(present) & present == 1) |>  # Keep only real ties
  separate(tie, into = c("source", "target"), sep = "_") |>
  mutate(
    source = as.numeric(sub("N", "", source)),
    target = as.numeric(sub("N", "", target))
  ) |>
  select(PRIM_KEY, source, target)

# Create a lookup of valid alter IDs per ego
valid_alters <- alter.df |>
  select(PRIM_KEY, alter_id)

# Filter alter.ties — keep only rows where both source and target exist for that ego
alter.ties.cleaned <- alter.ties |>
  semi_join(valid_alters, by = c("PRIM_KEY", "source" = "alter_id")) |>
  semi_join(valid_alters, by = c("PRIM_KEY", "target" = "alter_id"))

# Create egor object
egor_obj <- egor(
  alters = alter.df,
  egos = ego.df,
  aaties = alter.ties.cleaned,
  ID.vars = list(
    ego = "PRIM_KEY",
    alter = "alter_id",
    source = "source",
    target = "target"
  )
)

# Check the structure of the egor object
summary(egor_obj)
plot_egor(egor_obj, ego = 1)

# Save egor object
save(egor_obj, file = "./data/egor_obj.rda")
