## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Dimensions of social support
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load the egor object
load("./data/egor_support.rda")

# Join alters with ego race
alter_with_race <- egor_support$alter |>
  left_join(egor_support$ego |> select(.egoID, race), by = ".egoID")

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Average number of alters providing each type of support across egos, grouped by race
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Count number of alters providing each type of support (per-ego summary of their social network composition)
ego_support_counts <- alter_with_race |>
  group_by(.egoID, race) |>
  summarise(
    n_practical  = sum(practical_support == 1, na.rm = TRUE),
    n_emotional  = sum(emotional_support == 1, na.rm = TRUE),
    n_emergency  = sum(emergency_support == 1, na.rm = TRUE),
    n_appraisal  = sum(appraisal_support == 1, na.rm = TRUE),
    n_burdensome = sum(burdensome_tie == 1, na.rm = TRUE),
    n_conflict   = sum(conflict_tie == 1, na.rm = TRUE),
    n_demands    = sum(demands_help == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Average of alters across egos by race 

ego_support_avg <-  ego_support_counts |>
  group_by(race) |>
  summarise(
    avg_practical  = mean(n_practical),
    avg_emotional  = mean(n_emotional),
    avg_emergency  = mean(n_emergency),
    avg_appraisal  = mean(n_appraisal),
    avg_burdensome = mean(n_burdensome),
    avg_conflict   = mean(n_conflict),
    avg_demands    = mean(n_demands),
    .groups = "drop"
  )

# Save table
write_csv(ego_support_avg, "./outputs/ego_support_avg.csv")

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
##  Proportion of alters per ego (within each race group) who provide each support type.
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Count and proportion of support per ego
ego_support_prop <- alter_with_race |>
  group_by(.egoID, race) |>
  summarise(
    n_alters       = n(),
    p_practical    = mean(practical_support == 1, na.rm = TRUE),
    p_emotional    = mean(emotional_support == 1, na.rm = TRUE),
    p_emergency    = mean(emergency_support == 1, na.rm = TRUE),
    p_appraisal    = mean(appraisal_support == 1, na.rm = TRUE),
    p_burdensome   = mean(burdensome_tie == 1, na.rm = TRUE),
    p_conflict     = mean(conflict_tie == 1, na.rm = TRUE),
    p_demands      = mean(demands_help == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Average proportion per support type across egos, grouped by race (aggregate proportions by race to compare group differences)

support_prop_by_race <- ego_support_prop |>
  group_by(race) |>
  summarise(
    n_egos = n(),
    avg_p_practical  = mean(p_practical, na.rm = TRUE),
    avg_p_emotional  = mean(p_emotional, na.rm = TRUE),
    avg_p_emergency  = mean(p_emergency, na.rm = TRUE),
    avg_p_appraisal  = mean(p_appraisal, na.rm = TRUE),
    avg_p_burdensome = mean(p_burdensome, na.rm = TRUE),
    avg_p_conflict   = mean(p_conflict, na.rm = TRUE),
    avg_p_demands    = mean(p_demands, na.rm = TRUE),
    .groups = "drop"
  )

# Save table
write_csv(support_prop_by_race, "./outputs/support_proportions_by_race.csv")

# Visualize results

# Covert table to long format for plotting
support_prop_long <- support_prop_by_race |>
  pivot_longer(
    cols = starts_with("avg_p_"),
    names_to = "support_type",
    names_prefix = "avg_p_",
    values_to = "avg_proportion"
  )

# Reorder support type
support_prop_long <- support_prop_long |>
  mutate(support_type = factor(
    support_type,
    levels = c("practical", "emotional", "emergency", "appraisal", "burdensome", "conflict", "demands")
  ))

support_plot <- ggplot(support_prop_long, aes(x = support_type, y = avg_proportion, fill = race)) +
  geom_col(position = "dodge") +
  labs(title = "Average Proportion of Alters Providing Each Type of Support/Strain by Race",
       x = "Support / Strain Type", y = "Average Proportion",
       fill = "Race of Ego") +
  theme_minimal()

# Save the plot
ggsave(
  filename = "./outputs/support_plot.pdf",
  plot = support_plot,
  width = 10, height = 6, dpi = 300)

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Create multiplex ties
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Create create composite scores (multiplexity):
egor_support$alter <- egor_support$alter |>
  mutate(
    multiplex_positive = practical_support + emergency_support + emotional_support + appraisal_support,
    multiplex_negative = conflict_tie + burdensome_tie + demands_help
  ) |>
  mutate(
    ambivalent = case_when(multiplex_positive > 0 & conflict_tie == 1 ~ 1, TRUE ~ 0),
    reciprocal = case_when(demands_help == 1 & multiplex_positive > 0 ~ 1, TRUE ~ 0),
    difficult  = case_when(conflict_tie == 1 & multiplex_positive == 0 ~ 1, TRUE ~ 0),
    demanding  = case_when(demands_help == 1 & multiplex_positive == 0 ~ 1, TRUE ~ 0)
  )

## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Create multiplex ties across race groups
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Count multiplex ties per ego
ego_tie_counts <- egor_support$alter |>
  group_by(.egoID) |>
  summarise(
    n_positive     = sum(multiplex_positive > 0, na.rm = TRUE),
    n_negative     = sum(multiplex_negative > 0, na.rm = TRUE),
    n_ambivalent   = sum(ambivalent, na.rm = TRUE),
    n_reciprocal   = sum(reciprocal, na.rm = TRUE),
    n_difficult    = sum(difficult, na.rm = TRUE),
    n_demanding = sum(demanding, na.rm = TRUE)
  )

# Add Network Size and Race to Each Ego
ego_size_race <- egor_support$ego |>
  select(.egoID, race) |>
  left_join(
    egor_support$alter |>
      count(.egoID, name = "network_size"),
    by = ".egoID"
  )

# Merge tie counts (n_positive, n_ambivalent, etc.) with network size and race and normalize for proportion
ego_tie_props <- ego_tie_counts |>
  left_join(ego_size_race, by = ".egoID") |>
  mutate(
    p_positive   = n_positive / network_size,
    p_negative   = n_negative / network_size,
    p_ambivalent = n_ambivalent / network_size,
    p_reciprocal = n_reciprocal / network_size,
    p_difficult  = n_difficult / network_size,
    p_demanding  = n_demanding / network_size
  )

saveRDS(ego_tie_props, file = "data/ego_tie_props.rds")


# Group by Race and Average the Proportions
tie_prop_by_race <- ego_tie_props |>
  group_by(race) |>
  summarise(
    avg_p_positive   = mean(p_positive, na.rm = TRUE),
    avg_p_negative   = mean(p_negative, na.rm = TRUE),
    avg_p_ambivalent = mean(p_ambivalent, na.rm = TRUE),
    avg_p_reciprocal = mean(p_reciprocal, na.rm = TRUE),
    avg_p_difficult  = mean(p_difficult, na.rm = TRUE),
    avg_p_demanding  = mean(p_demanding, na.rm = TRUE),
    .groups = "drop"
  )
# Save the multiplex ties data
write_csv(tie_prop_by_race, "./outputs/tie_prop_by_race.csv")

# Prepare data for plotting
tie_prop_long <- tie_prop_by_race |>
  pivot_longer(
    cols = starts_with("avg_p_"),
    names_to = "tie_type",
    names_prefix = "avg_p_",
    values_to = "avg_proportion"
  ) |>
  mutate(tie_type = factor(tie_type, levels = c("positive", "ambivalent", "reciprocal", "difficult", "demanding", "negative")))

# Plot
tie_plot <- ggplot(tie_prop_long, aes(x = tie_type, y = avg_proportion, fill = race)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Proportion of Tie Types per Ego by Race",
    x = "Tie Type", y = "Average Proportion",
    fill = "Race"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # <-- this centers the title
  )

# Save the plot
ggsave(
  filename = "./outputs/tie_plot.pdf",
  plot = tie_plot,
  width = 10, height = 6, dpi = 300
)

# Save the egor object with race, support and relationship variables
egor_socnet <- egor(
  egos = egor_support$ego,     # already contains 'race'
  alters = egor_support$alter, # contains relationship + tie type variables
  aaties = egor_support$aatie,
  ID.vars = list(
    ego = ".egoID",
    alter = ".altID",
    source = ".srcID",
    target = ".tgtID"
  )
)

# Save it
save(egor_socnet, file = "./data/egor_socnet.rda")



