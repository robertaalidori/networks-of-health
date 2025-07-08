## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
## Import and clean all data.
## = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load & Rename Ego Data (da36975.0005.rda)

load("./data/da36975.0005.rda")  # Loads object da36975.0005

ego_df <- get("da36975.0005")    # Rename to ego_df
rm("da36975.0005")               # Clean up old name

save(ego_df, file = "./data/ego_df.rda")  # Save clean version

# Load & Rename Alter Data (da36975.0001.rda)
load("./data/da36975.0001.rda")  # Loads object da36975.0001

alter_df <- get("da36975.0001")  # Rename to alter_df

rm("da36975.0001")               # Clean up old name

save(alter_df, file = "./data/alter_df.rda")  # Save clean version

# Check the structure of the ego_df
ego_df
str(ego_df)
