# ==============================================================================
# SCRIPT OVERVIEW & DICTIONARY
# ==============================================================================
# This script processes network and survey data from Waves 1, 3, and 4 to prepare 
# a combined dataset of female respondents for mixed‐effects logistic regression 
# modeling of postpartum depression (f1500r01 == 1).
#
# Key steps:
#
#   1. Load required packages:
#        • Matrix        — advanced matrix operations
#        • lme4          — mixed‐effects modeling (glmer)
#        • dplyr, tidyr  — data manipulation and reshaping
#        • MuMIn         — Nakagawa & Schielzeth R²
#        • performance   — pseudo‐R² and discrimination metrics
#
#   2. Per‐wave data merging & cleaning (Waves 1, 3, 4):
#        a. Join network attribute tables (dataframe_attributes_fr_*) with edge lists 
#           (all_networks_df_*) and health/financial networks where available.
#        b. Merge individual‐level survey data (individuals_wave*_fr) by respondent ID.
#        c. Recode postpartum indicator (f1500r01): 1 if “Younger than 6 months”, else 0.
#        d. Filter to postpartum cases only (f1500r01 == 1).
#        e. Drop unneeded village‐level columns (travel times, clinic access).
#        f. Compute household‐level depression averages (avg_household) excluding self.
#        g. Merge village wealth index from CSV (wave‐specific).
#
#   3. Harmonize & label variables:
#        • Assign key survey variables from “.x” sources: age, marital_status, gender, FI.
#        • Scale or recode:
#            – age (numeric)
#            – education ← b0100.x
#            – religion ← b0600.x (factor)
#            – indigenous_status ← indigenous.binary.x
#            – network metrics: percentage_depressed, friends,
#              avg_neighbors_degree, intransitivity_values,
#              Adversaries, network_size, network_density, Adv_density
#            – avg_household, number_household
#
#   4. Retain only female respondents (gender == 0) and merge household IDs.
#
#   5. Combine Waves 1, 3, and 4 datasets (rbind), ensure unique column names,
#      convert “depressed” to factor, and clean names.
#
#   6. Fit mixed‐effects logistic regression with glmer():
#        depressed ~ age + marital_status + percentage_depressed + friends +
#                    avg_neighbors_degree + intransitivity_values + Adversaries +
#                    access_routes + education + religion +
#                    avg_household + number_household + indigenous_status + FI +
#                    network_size + network_density + Adv_density + wave +
#                    (1 | name) + (1 | Household) + (1 | village)
#
#   7. Extract and format model outputs:
#        • Fixed‐effect coefficients, SEs, z‐values, p‐values
#        • Odds ratios & 95% confidence intervals
#        • Likelihood ratio test vs. null model
#        • Marginal/conditional R² (Nakagawa & Schielzeth)
#        • Tjur’s R² and other pseudo‐R² (performance package)
#
#   8. Save model objects and formatted tables for reporting.
# ==============================================================================

# ==============================================================================
# BEGIN SCRIPT
# ==============================================================================

# Number WOMEN:: 1245, groups:  Household, 1199; name, 1197; village, 169, along 3 waves (wave1, 3 and 4)


# ------------------------------------------------------------------------------
# Load required packages for modeling
# ------------------------------------------------------------------------------
library(Matrix)            # For advanced matrix operations (if needed later)
library(lme4)              # For fitting mixed-effects models (glmer function)
library(dplyr)             # For data manipulation and piping (%>%)

# ------------------------------------------------------------------------------
# OVERVIEW:
# This code processes data from Waves 1, 3, and 4. For each wave we:
#   - Merge network attribute data with individual-level data.
#   - Clean and recode variables (e.g., recoding f1500r01).
#   - Compute household-level averages (e.g., average depression among household members).
#   - Merge additional information such as village wealth.
#   - Scale selected variables and immediately assign them intuitive names.
# Finally, we combine all waves into one final dataset, fit a mixed-effects 
# logistic regression model, and format the output with user-friendly variable labels.
# ------------------------------------------------------------------------------

#load("Depression/Code/Results/network_dfs_fr.RData")
#load("Depression/Code/Results/network_dfs_adv.RData")
#load("Depression/Code/Results/network_dfs_financial.RData")
#load("Depression/Code/Results/network_dfs_health.RData")

# ------------------------------------------------------------------------------
# STANDARDIZE WEALTH VARIABLES ACROSS WAVES
# ------------------------------------------------------------------------------

# Copy the raw wealth values into a standardized column called 'Wealth'
dataframe_attributes_fr_1$Wealth <- dataframe_attributes_fr_1$Wealth1              # Wave 1: Use Wealth1
dataframe_attributes_fr_3$Wealth <- dataframe_attributes_fr_3$Wealth3              # Wave 3: Use Wealth3
dataframe_attributes_fr_4$Wealth <- dataframe_attributes_fr_4$Wealth4              # Wave 4: Use Wealth4

# ------------------------------------------------------------------------------
# WAVE 1: MERGING AND PROCESSING
# ------------------------------------------------------------------------------
# Merge network attribute data for Wave 1 with additional network datasets.
joined_df_1 <- dataframe_attributes_fr_1 %>%                                # Start with Wave 1 network attributes
  left_join(all_networks_df_1, by = c("name" = "Node")) %>%                    # Merge primary network data using "name" from attributes and "Node" from network
  left_join(all_networks_df_health_1, by = c("name" = "Node")) %>%               # Merge health-related network data
  left_join(all_networks_df_financial_1, by = c("name" = "Node"))                # Merge financial network data

# Merge the joined network data with individual-level data for Wave 1 respondents.
final_df <- joined_df_1 %>% 
  left_join(individuals_wave1_fr, by = c("name" = "Respondent1"))               # Match on respondent IDs

# Clean up f1500r01:
final_df$f1500r01[is.na(final_df$f1500r01)] <- 0                               # Replace any NA in f1500r01 with 0
final_df$f1500r01 <- ifelse(final_df$f1500r01 == "Younger than 6 months", 1, 0)  # Recode: if "Younger than 6 months", then 1; otherwise, 0

# Remove unwanted village-level columns that are not needed for the analysis.
final_df <- final_df %>% 
  dplyr::select(-time_to_main_road, -health_center, -time_to_health_ctr, -maternal_clinic, -time_to_mat_clinic)

# Compute household-level depression averages (excluding the individual’s own value)
final_df <- final_df %>%
  group_by(Household1) %>%                                                    # Group by household identifier
  mutate(
    avg_household = (sum(depressed_w1.x) - depressed_w1.x) / (n() - 1),         # Compute the average depression level of other household members
    avg_household = replace_na(avg_household, 0),                             # Replace NA values with 0
    number_household = n()                                                    # Count the number of individuals in the household
  ) %>% 
  ungroup()                                                                  # Remove grouping

# Load village wealth information for Wave 1 from a CSV file and select necessary columns.
village_wealth_1 <- read.csv("honduras_villages_WAVE1_v8.csv") %>% 
  dplyr::select(village_code, village_wealth_index_median_w1)
# Merge village wealth info with the main dataset by matching village codes.
final_df <- final_df %>% 
  dplyr::left_join(village_wealth_1, by = c("village" = "village_code"))

# Harmonize key variables by using the .x versions (from the individual data join).
final_df$age <- final_df$age.x                                              # Set age from age.x
final_df$marital_status <- final_df$marital_status.x                        # Set marital status from marital_status.x
final_df$gender <- final_df$gender.x                                        # Set gender from gender.x
final_df$FI <- final_df$FI.x                                                # Set food insecurity (FI) from FI.x


# ---------------------------
# SELECTED VARIABLES & ASSIGN INTUITIVE NAMES (WAVE 1)
# ---------------------------
final_df$avg_neighbors_degree <- final_df$avg_neighbors_degree 

# Convert age to numeric and then keep as is.
final_df$age <- as.numeric(final_df$age)                              # Ensure age is numeric
final_df$age <- final_df$age                                           # Age remains unchanged

# ---------------------------
# CREATE NETWORK-DERIVED VARIABLES (WAVE 1)
# ---------------------------
# Calculate the total number of neighbors (sum of depressed and not depressed neighbors).
total_neigh <- final_df$depressed_w1_neigh + final_df$not_depressed_w1_neigh
# Create a variable for the percentage of depressed neighbors.
final_df$percentage_depressed <- ifelse(
  total_neigh == 0,                        # If there are no neighbors...
  0,                                       # assign 0,
  final_df$depressed_w1_neigh / total_neigh  # otherwise, calculate the proportion
)
# Create a variable for the total number of friends (neighbors).
final_df$friends <- ifelse(
  total_neigh == 0,                                                         # If there are no neighbors...
  0,                                                                        # assign 0,
  final_df$depressed_w1_neigh + final_df$not_depressed_w1_neigh               # otherwise, calculate the total count
)

final_df$avg_neighbors_degree <- final_df$avg_neighbors_degree 

# ---------------------------
# ADDITIONAL VARIABLES & RENAME INTUITIVELY (WAVE 1)
# ---------------------------
# Wealth and renamed columns.
final_df$household_wealth <- final_df$Wealth                         # Wealth becomes household_wealth
final_df$individual_wealth <- final_df$d0700.x                         # d0700.x becomes individual_wealth
final_df$education <- final_df$b0100.x                                 # b0100.x becomes education
final_df$religion <- as.factor(final_df$b0600.x)                                  # b0600.x becomes religion
final_df$indigenous_status <- final_df$indigenous.binary.x             # indigenous.binary.x becomes indigenous_status

# Remaining network variables.
final_df$Adversaries <- final_df$Adversaries                           # Adversaries remains unchanged
final_df$network_size <- final_df$network_size                         # network_size remains unchanged
final_df$network_density <- scale(final_df$network_density)                   # network_density remains unchanged
final_df$Adv_density <- scale(final_df$Adv_density)                           # Adv_density remains unchanged
final_df$avg_household <- final_df$avg_household                       # Average household depression remains unchanged
final_df$number_household <- final_df$number_household                 # Household size remains unchanged

# Filter to include only female respondents (assuming gender == 0 indicates female).
final_df <- final_df %>% filter(gender == 0)

# Merge household information from the individuals_wave1 dataset.
ind1_tmp <- individuals_wave1[, c("Respondent1", "Household1")]              # Select respondent and household columns
names(ind1_tmp) <- c("name", "Household")                                    # Rename columns to "name" and "Household"
final_df <- merge(final_df, ind1_tmp, by = "name", all.x = TRUE)              # Merge on "name", keeping all rows

# Save the processed Wave 1 dataset.
final_df_1 <- final_df                                                     # Save current final_df as final_df_1
final_df_1$wave <- 1                                                        # Add a wave indicator

# ------------------------------------------------------------------------------
# WAVE 3 MERGING AND PROCESSING
# ------------------------------------------------------------------------------
# Merge network attribute data for Wave 3 with additional network datasets.
joined_df_3 <- dataframe_attributes_fr_3 %>%
  left_join(all_networks_df_3, by = c("name" = "Node"))                 # Primary network data for Wave 3

# Merge the joined network data with individual-level data for Wave 3.
final_df <- joined_df_3 %>%
  left_join(individuals_wave3_fr, by = c("name" = "Respondent3"))              # Merge using respondent IDs

# Create the IHV (Interpersonal Household Violence) variable.
final_df$IHV <- ifelse(
  final_df$i0300 == "Yes" | final_df$i0400 == "Yes" | 
    final_df$i0500 == "Yes" | final_df$i0600 == "Yes" | 
    final_df$i0700 == "Yes", 1, 0                                              # If any violence indicator is "Yes", assign 1; else 0
)

# Remove unwanted columns that are not needed.
#columns_to_remove <- c("d0100", "d0200", "e0300", "c1100", "c1200", "c1300", 
#                       "i0300", "i0400", "i0500", "i0600", "i0700", "cohabitating", "IPV_hist")
#final_df <- final_df %>% select(-all_of(columns_to_remove))                  # Drop the unwanted columns

# Recode f1500r01 for Wave 3.
final_df$f1500r01[is.na(final_df$f1500r01)] <- 0                               # Replace NA with 0
final_df$f1500r01 <- ifelse(final_df$f1500r01 == "Younger than 6 months", 1, 0)  # Recode as before

# Remove unwanted village-level columns.
final_df <- final_df %>% 
  select(-time_to_main_road, -health_center, -time_to_health_ctr, -maternal_clinic, -time_to_mat_clinic)

# Compute household-level averages for depressed_w3.x.
final_df <- final_df %>%
  group_by(Household3) %>%                                                    # Group by Household3
  mutate(
    avg_household = (sum(depressed_w3.x) - depressed_w3.x) / (n() - 1),         # Compute average excluding self
    avg_household = replace_na(avg_household, 0),                             # Replace NA with 0
    number_household = n()                                                    # Count household members
  ) %>% ungroup()                                                              # Remove grouping

# Merge village wealth information for Wave 3.
village_wealth_1 <- read.csv("honduras_villages_WAVE3_v3.csv") %>% 
  select(village_code, village_wealth_index_median_w3)                      # Select relevant columns from wealth data
final_df <- final_df %>% left_join(village_wealth_1, by = c("village" = "village_code"))  # Merge by village code

# ---------------------------
# SCALE SELECTED VARIABLES & RENAME INTUITIVELY FOR WAVE 3
# ---------------------------
final_df$age <- final_df$age.x                                              # Use age.x for age
final_df$marital_status <- final_df$marital_status.x                        # Use marital_status.x for marital_status
final_df$gender <- final_df$gender.x                                        # Use gender.x for gender
final_df$FI <- final_df$FI.x                                                # Use FI.x for food insecurity

final_df$age <- as.numeric(final_df$age)                              # Convert age to numeric
final_df$age <- final_df$age                                          # Age remains unchanged

# Remaining variables.
final_df$Wealth <- final_df$Wealth                                   # Wealth remains unchanged
final_df$intransitivity_values <- final_df$intransitivity_values     # intransitivity_values remains unchanged
final_df$d0700 <- final_df$d0700.x                                  # d0700.x remains unchanged
final_df$b0100 <- final_df$b0100.x                                  # b0100.x remains unchanged
final_df$b0600 <- final_df$b0600.x                                  # b0600.x remains unchanged
final_df$indigenous.binary <- final_df$indigenous.binary.x          # indigenous.binary.x remains unchanged

# Rename key variables to intuitive names for Wave 3.
final_df$education <- final_df$b0100.x                # Raw education score becomes "education"
final_df$religion <- as.factor(final_df$b0600.x)                  # Raw religion score becomes "religion"
final_df$individual_wealth <- final_df$d0700.x          # Raw individual wealth becomes "individual_wealth"
final_df$household_wealth <- final_df$Wealth            # Raw household wealth becomes "household_wealth"
final_df$indigenous_status <- final_df$indigenous.binary.x # Raw indigenous status becomes "indigenous_status"

# Create network-derived variables for Wave 3.
total_neigh <- final_df$depressed_w3_neigh + final_df$not_depressed_w3_neigh  # Total neighbors for Wave 3
final_df$percentage_depressed <- ifelse(
  total_neigh == 0,
  0,
  final_df$depressed_w3_neigh / total_neigh         # Proportion of depressed neighbors
)
final_df$friends <- ifelse(
  total_neigh == 0,
  0,
  final_df$depressed_w3_neigh + final_df$not_depressed_w3_neigh  # Total neighbors
)

# Additional network-level variables.
final_df$Adversaries <- final_df$Adversaries       # Adversaries remains unchanged
final_df$network_size <- final_df$network_size     # Network size remains unchanged
final_df$network_density <- scale(final_df$network_density)  # Network density remains unchanged
final_df$Adv_density <- scale(final_df$Adv_density)         # Adversarial density remains unchanged
final_df$avg_household <- final_df$avg_household     # Average household depression remains unchanged
final_df$number_household <- final_df$number_household  # Household size remains unchanged

final_df$avg_neighbors_degree <- final_df$avg_neighbors_degree 


# Filter to keep only female respondents (assuming gender == 0 indicates female).
final_df <- final_df %>% filter(gender == 0)

# Merge household information from the individuals_wave3 dataset.
ind3_tmp <- individuals_wave3[, c("Respondent3", "Household3")]  # Select respondent and household columns for Wave 3
names(ind3_tmp) <- c("name", "Household")                       # Rename columns to "name" and "Household"
final_df <- merge(final_df, ind3_tmp, by = "name", all.x = TRUE)  # Merge by "name", keeping all rows

# Save the processed Wave 3 data.
final_df_3 <- final_df                                                 # Save final_df as final_df_3
final_df_3$wave <- 3                                                    # Set wave indicator to 3

# ------------------------------------------------------------------------------
# WAVE 4 MERGING AND PROCESSING
# ------------------------------------------------------------------------------
# Merge network attribute data for Wave 4 with additional network data.
joined_df_4 <- dataframe_attributes_fr_4 %>%
  left_join(all_networks_df_4, by = c("name" = "Node"))                  # Primary network data for Wave 4

# Merge the joined network data with individual-level data for Wave 4.
final_df <- joined_df_4 %>%
  left_join(individuals_wave4_fr, by = c("name" = "Respondent4"))              # Merge using respondent IDs

# Recode f1500r01 for Wave 4.
final_df$f1500r01[is.na(final_df$f1500r01)] <- 0                             # Replace NA in f1500r01 with 0
final_df$f1500r01 <- ifelse(final_df$f1500r01 == "Younger than 6 months", 1, 0)  # Recode f1500r01

# Remove unwanted village-level columns for Wave 4.
final_df <- final_df %>% 
  select(-time_to_main_road, -health_center, -time_to_health_ctr, -maternal_clinic, -time_to_mat_clinic)

# Compute household-level averages for depressed_w4.x.
final_df <- final_df %>%
  group_by(Household4) %>%                                                    # Group by Household4
  mutate(
    avg_household = (sum(depressed_w4.x) - depressed_w4.x) / (n() - 1),         # Compute average (excluding self)
    avg_household = replace_na(avg_household, 0),                             # Replace NA with 0
    number_household = n()                                                    # Count members in the household
  ) %>% ungroup()                                                              # Ungroup data

# Merge village wealth info for Wave 4.
village_wealth_1 <- read.csv("honduras_villages_WAVE4_v2.csv") %>% 
  select(village_code, village_wealth_index_median)                        # Select necessary columns
final_df <- final_df %>% left_join(village_wealth_1, by = c("village" = "village_code"))  # Merge by village code

# ---------------------------
# SCALE SELECTED VARIABLES & RENAME INTUITIVELY FOR WAVE 4
# ---------------------------
final_df$age <- final_df$age.x                                               # Use age.x as age
final_df$marital_status <- final_df$marital_status.x                         # Use marital_status.x as marital_status
final_df$gender <- final_df$gender.x                                         # Use gender.x as gender
final_df$FI <- final_df$FI.x                                                 # Use FI.x as FI
final_df$d0700 <- final_df$d0700.x                                          # d0700.x remains unchanged
final_df$b0100 <- final_df$b0100.x                                          # b0100.x remains unchanged
final_df$b0600 <- final_df$b0600.x                                          # b0600.x remains unchanged
final_df$indigenous.binary <- final_df$indigenous.binary.x                  # indigenous.binary.x remains unchanged
final_df$age <- as.numeric(final_df$age)                                    # Convert age to numeric
final_df$Wealth <- final_df$Wealth                                          # Wealth remains unchanged
final_df$age <- final_df$age                                                # Age remains unchanged (again, if necessary)
final_df$intransitivity_values <- final_df$intransitivity_values            # intransitivity values remain unchanged

final_df$avg_neighbors_degree <- final_df$avg_neighbors_degree 

# ---------------------------
# RENAME INTUITIVE VARIABLES FOR WAVE 4
# ---------------------------
final_df$education <- final_df$b0100.x                # Rename b0100.x as "education" (raw education score)
final_df$religion <- as.factor(final_df$b0600.x)                  # Rename b0600.x as "religion" (raw religion score)
final_df$individual_wealth <- final_df$d0700.x         # Rename d0700.x as "individual_wealth" (raw individual wealth)
final_df$household_wealth <- final_df$Wealth           # Rename Wealth as "household_wealth" (raw household wealth)
final_df$indigenous_status <- final_df$indigenous.binary.x   # Rename indigenous.binary.x as "indigenous_status" (raw indigenous status)

# ---------------------------
# CREATE NETWORK-DERIVED VARIABLES FOR WAVE 4
# ---------------------------
total_neigh <- final_df$depressed_w4_neigh + final_df$not_depressed_w4_neigh  # Total neighbors for Wave 4
final_df$percentage_depressed <- ifelse(                                  # Compute percentage of depressed neighbors
  total_neigh == 0,
  0,
  final_df$depressed_w4_neigh / total_neigh
)
final_df$friends <- ifelse(                                               # Compute total number of friends
  total_neigh == 0,
  0,
  final_df$depressed_w4_neigh + final_df$not_depressed_w4_neigh
)

# Scale additional network-level variables for Wave 4.
final_df$Adversaries <- final_df$Adversaries         # Adversaries remain unchanged
final_df$network_size <- final_df$network_size       # Network size remains unchanged
final_df$network_density <- scale(final_df$network_density) # Network density remains unchanged
final_df$Adv_density <- scale(final_df$Adv_density)         # Adversarial density remains unchanged
final_df$avg_household <- final_df$avg_household     # Average household depression remains unchanged
final_df$number_household <- final_df$number_household  # Household size remains unchanged

# Filter to keep only female respondents.
final_df <- final_df %>% filter(gender == 0)                     # Retain only rows where gender == 0

# Merge household information from the individuals_wave4 dataset.
ind4_tmp <- individuals_wave4[, c("Respondent4", "Household4")]  # Select respondent and household columns for Wave 4
names(ind4_tmp) <- c("name", "Household")                      # Rename columns to "name" and "Household"
final_df <- merge(final_df, ind4_tmp, by = "name", all.x = TRUE) # Merge on "name", keeping all rows

# Save the processed Wave 4 dataset.
final_df_4 <- final_df                                          # Save final_df as final_df_4
final_df_4$wave <- 4                                             # Set wave indicator to 4

# ------------------------------------------------------------------------------
# COMBINE PROCESSED DATA FROM WAVES 1, 3, AND 4
# ------------------------------------------------------------------------------
# For each wave, we now rename variables for consistency and select a common set of columns.

# Process Wave 1 data.
final_df_1 <- final_df_1 %>%
  dplyr::rename(
    depressed = depressed_w1.x,                   # Rename depressed_w1.x as depressed
    percentage_depressed = percentage_depressed,  # Keep percentage_depressed as is
    friends = friends                             # Keep friends as is
  ) %>%
  dplyr::select(
    gender, name, depressed, age, intransitivity_values, percentage_depressed, friends,
    Adversaries, access_routes, education, religion, avg_household, number_household,
    indigenous_status, FI,
    network_size, network_density, marital_status, avg_shortest_path, Adv_density, f1500r01,
    village, Household, wave,  household_wealth,
    individual_wealth,
    avg_neighbors_degree 
  )

# Process Wave 3 data.
final_df_3 <- final_df_3 %>%
  dplyr::rename(
    depressed = depressed_w3.x,                   # Rename depressed_w3.x as depressed
    percentage_depressed = percentage_depressed,  # Keep percentage_depressed as is
    friends = friends                             # Keep friends as is
  ) %>%
  dplyr::select(
    gender, name, depressed, age, intransitivity_values, percentage_depressed, friends,
    Adversaries, access_routes, education, religion, avg_household, number_household,
    indigenous_status, FI,
    network_size, network_density, marital_status, avg_shortest_path, Adv_density, f1500r01,
    village, Household, wave, household_wealth,
    individual_wealth,
    avg_neighbors_degree 
  )

# Process Wave 4 data.
final_df_4 <- final_df_4 %>%
  dplyr::rename(
    depressed = depressed_w4.x,                   # Rename depressed_w4.x as depressed
    percentage_depressed = percentage_depressed,  # Keep percentage_depressed as is
    friends = friends                             # Keep friends as is
  ) %>%
  dplyr::select(
    gender, name, depressed, age, intransitivity_values, percentage_depressed, friends,
    Adversaries, access_routes, education, religion, avg_household, number_household,
    indigenous_status, FI,
    network_size, network_density, marital_status, avg_shortest_path, Adv_density, f1500r01,
    village, Household, wave,  household_wealth,
    individual_wealth,
    avg_neighbors_degree 
  )

# Combine the datasets from Waves 1, 3, and 4.
final_df <- rbind(final_df_1, final_df_3, final_df_4)                       # Row-bind all wave datasets

final_df <- final_df[final_df$f1500r01==1,]


# (Optional) Remove rows with any NA values.
# final_df <- final_df %>% drop_na()

# Convert specified categorical variables to factors.
final_df <- as.data.frame(final_df)                                         # Ensure final_df is a data frame
cat_vars <- c("depressed")                                                   # Specify the categorical variable(s)
final_df[cat_vars] <- lapply(final_df[cat_vars], as.factor)                  # Convert them to factors

# Ensure all column names are unique and valid.
colnames(final_df) <- make.names(colnames(final_df), unique = TRUE)          # Clean column names

# ------------------------------------------------------------------------------
# FIT A MIXED-EFFECTS LOGISTIC REGRESSION MODEL USING glmer()
# ------------------------------------------------------------------------------


#### WITHOUT RELATIVE ####


# ------------------------------------------------------------------------------
# FIT A MIXED-EFFECTS LOGISTIC REGRESSION MODEL USING glmer()
# ------------------------------------------------------------------------------
# Fit the model predicting depression using various fixed effects along with 
# random intercepts for name, Household, and village.
model_post_women_not_relative <- glmer(
  depressed ~ as.numeric(age) + marital_status +  percentage_depressed +
    friends + avg_neighbors_degree + intransitivity_values + Adversaries +
    access_routes + education + religion + 
    avg_household + number_household + indigenous_status + 
    FI +   network_size + network_density + Adv_density +
    wave + (1 | name) + (1 | Household) + (1 | village),                     # Random intercepts for name, Household, and village
  data = final_df,
  family = binomial(link = "logit"),                                        # Logistic regression (binomial family with logit link)
  nAGQ = 0                                                                  # Use Laplace approximation (nAGQ = 0)
)

# Display a summary of the fitted mixed-effects model.
summary(model_post_women_not_relative)

save(model_post_women_not_relative, file = "../Code/Results/model_not_relative_post_women.rda")


# ------------------------------------------------------------------------------
# RENAME MODEL VARIABLES FOR PRESENTATION (FINAL OUTPUT TABLE)
# ------------------------------------------------------------------------------
# Define a vector of intuitive variable names corresponding to the model predictors.
Variables <- c(
  "(Intercept)",                   # Intercept
  "Age",                           # as.numeric(age)
  "Marital Status",                # marital_status
  "Percentage Depressed",          # percentage_depressed
  "Friends",                       # friends
  "Average Friends of Friends",    # avg friends of friends
  "Social Intransitivity",         # intransitivity_values
  "Adversaries",                   # Adversaries
  "Isolated Village",              # access_routes (if this variable reflects isolation)
  "Education",                     # education (raw education score)
  "Religion Protestant",                      # religion (raw religion score)
  "Religion Catholic",                      # religion (raw religion score)
  "Depressed Household",           # avg_household
  "Household Size",                # number_household
  "Indigenous Status",             # indigenous_status
  "Food Insufficiencies",          # FI
  #"Individual Wealth",             # individual_wealth (raw individual wealth)
  #"Household Wealth",              # household_wealth (raw household wealth)
  "Village Size",                  # network_size
  "Village Density Friendship",    # network_density
  "Village Density Adversarial",   # Adv_density
  "wave"                           # wave
)

# ------------------------------------------------------------------------------
# EXTRACT MODEL COEFFICIENTS, COMPUTE ODDS RATIOS, AND CONFIDENCE INTERVALS
# ------------------------------------------------------------------------------
coefficients <- summary(model_post_women_not_relative)$coefficients             # Extract coefficient matrix from model summary
odds_ratios <- exp(coefficients[, "Estimate"])                    # Compute odds ratios by exponentiating estimates
std_errors <- coefficients[, "Std. Error"]                        # Extract standard errors
conf_int_lower <- exp(coefficients[, "Estimate"] - 1.96 * std_errors)  # Compute lower bound of 95% confidence interval
conf_int_upper <- exp(coefficients[, "Estimate"] + 1.96 * std_errors)  # Compute upper bound of 95% confidence interval
p_values <- coefficients[, "Pr(>|z|)"]                             # Extract p-values for each predictor

# ------------------------------------------------------------------------------
# FORMAT THE MODEL RESULTS INTO A FRIENDLY TABLE
# ------------------------------------------------------------------------------
formatted_results <- data.frame(
  Variable = Variables,                                          # Use the intuitive variable names defined above
  `Odds Ratio (95% CI)` = paste0(
    sprintf("%.3f", odds_ratios),                                # Format odds ratios to 3 decimal places
    " (",
    sprintf("%.3f", conf_int_lower),                             # Format lower bound of CI
    ", ",
    sprintf("%.3f", conf_int_upper),                             # Format upper bound of CI
    ")"
  ),
  `p-value` = sprintf("[%.3f]", p_values)                        # Format p-values to 3 decimal places in brackets
)

# Display the final formatted model results.
print(formatted_results)


# NULL MODEL


## -----------------------------------------------------------------
## 1. Keep exactly the rows the full model used (same N, same cases)
keep   <- rownames(model.frame(model_post_women_not_relative))
df_use <- final_df[keep, , drop = FALSE]

## -----------------------------------------------------------------
## 2. Re-fit both models on *that identical data object*
model_post_women_not_relative_full <- update(model_post_women_not_relative, data = df_use)

model_post_women_not_relative_null <- glmer(
  depressed ~ 1 + (1 | name) + (1 | Household) + (1 | village),
  data   = df_use,
  family = binomial(link = "logit"),
  nAGQ   = 0
)

print(anova(model_post_women_not_relative_null, model_post_women_not_relative_full))

lrt <- anova(model_post_women_not_relative_null, model_post_women_not_relative_full)
chi_sq <- lrt$Chisq[2]
df     <- lrt$Df[2]
pval   <- lrt$`Pr(>Chisq)`[2]

chi_sq   # likelihood-ratio statistic
df       # degrees of freedom
pval     # p-value

save(model_post_women_not_relative_null, file = "../Code/Results/model_not_relative_post_women_null.rda")


##— R^2 computation

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model_post_women_not_relative)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model_post_women_not_relative)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model_post_women_not_relative)        # from performance





# 1. Define the exact variables in the fixed‐ and random‐part of your formula:
keep_vars <- c(
  # outcome
  "depressed",
  # predictors
  "age", "marital_status", "percentage_depressed", "friends",
  "avg_neighbors_degree", "intransitivity_values", "Adversaries",
  "access_routes", "education", "religion", "avg_household",
  "number_household", "indigenous_status", "FI", "network_size",
  "network_density", "Adv_density", "wave",
  # grouping factors
  "name", "Household", "village"
)

final_df_1 <- final_df_1[final_df_1$f1500r01==1,]
final_df_3 <- final_df_3[final_df_3$f1500r01==1,]
final_df_4 <- final_df_4[final_df_4$f1500r01==1,]


# 2. Subset final_df to only those columns:
final_df_1_trimmed <- na.omit(final_df_1[, keep_vars])
final_df_3_trimmed <- na.omit(final_df_3[, keep_vars])
final_df_4_trimmed <- na.omit(final_df_4[, keep_vars])


length(unique(na.omit(final_df_1_trimmed$name)))

length(unique(na.omit(final_df_3_trimmed$name)))

length(unique(na.omit(final_df_4_trimmed$name)))

# get the unique, non‐missing names from each
u1 <- unique(na.omit(final_df_1_trimmed$name))
u3 <- unique(na.omit(final_df_3_trimmed$name))

# find the names in df3 that are not in df1
new_in_3 <- setdiff(u3, u1)

# how many are they?
length(new_in_3)


dim(na.omit(final_df_1_trimmed))
dim(na.omit(final_df_3_trimmed))
dim(na.omit(final_df_4_trimmed))
dim(na.omit(final_df_trimmed))

# unique, non-missing names in df1, df3 and df4
u1 <- unique(na.omit(final_df_1_trimmed$name))
u3 <- unique(na.omit(final_df_3_trimmed$name))
u4 <- unique(na.omit(final_df_4_trimmed$name))

# names in df4 that are in neither df1 nor df3
new_in_4 <- setdiff(u4, union(u1, u3))

length(new_in_4) + length(new_in_3) + length(u1)


df_sub_1    <- final_df_1_trimmed[final_df_1_trimmed$name %in% u1, ]
df_sub_3    <- final_df_3_trimmed[final_df_3_trimmed$name %in% new_in_3, ]
df_sub_4    <- final_df_4_trimmed[final_df_4_trimmed$name %in% new_in_4, ]


## How many depressed?

sum(c(df_sub_1$depressed, df_sub_3$depressed, df_sub_4$depressed))




