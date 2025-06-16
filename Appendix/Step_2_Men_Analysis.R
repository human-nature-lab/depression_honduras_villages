# ==============================================================================
# DICTIONARY & OVERVIEW (Comments Only)
# ==============================================================================
# This script processes data from Waves 1, 3, and 4 for a mixed-effects logistic 
# regression analysis of depression. The key steps include:
#
#   1. Loading required packages.
#
#   2. Standardizing wealth variables:
#      - Wealth1_diff, Wealth3_diff, Wealth4_diff: Wave-specific wealth difference.
#      - Wealth1, Wealth3, Wealth4: Wave-specific raw wealth values.
#      These are copied to standardized columns "Wealth_diff" and "Wealth" for all waves.
#
#   3. Merging and processing for each wave:
#      - Merging network data (all_networks_df_*) with individual-level data (individuals_wave*_fr).
#      - Recoding variables, e.g., f1500r01 is recoded to 1 if "Younger than 6 months" and 0 otherwise.
#      - Removing unwanted columns (e.g., time_to_main_road, health_center, etc.).
#      - Computing household-level averages (avg_household) for depression scores.
#      - Merging additional village-level wealth data from CSV files.
#
#   4. Harmonizing and scaling key variables:
#      - Variables with the suffix ".x" come from individual-level merges.
#      - Key variables (e.g., age, marital_status, gender, FI) are assigned from the .x versions.
#      - Other variables are scaled and renamed to:
#         • household_wealth = Wealth (scaled raw wealth)
#         • individual_wealth = d0700.x (scaled)
#         • education = b0100.x (scaled raw education score)
#         • religion = b0600.x (scaled raw religion score)
#         • indigenous_status = indigenous.binary.x (scaled raw indigenous status)
#
#   5. Creating network-derived variables:
#      - percentage_depressed: The scaled proportion of depressed neighbors.
#      - friends: The scaled total count of neighbors (depressed + not depressed).
#
#   6. Filtering to retain only male respondents (where gender is assumed to be 0 or 1 as specified).
#
#   7. Merging household information (Household1, Household3, Household4) from the corresponding
#      individuals data.
#
#   8. Combining the processed data from Waves 1, 3, and 4 into one final dataset.
#
#   9. Fitting a mixed-effects logistic regression model using glmer(), with fixed effects for 
#      various individual- and network-level predictors and random intercepts for name, Household, 
#      and village.
#
#  10. Extracting model coefficients, computing odds ratios and 95% confidence intervals, and 
#      formatting the results into a final user-friendly output table with intuitive variable names.
#
# ==============================================================================
#
# ==============================================================================
# BEGIN SCRIPT
# ==============================================================================

# OVERALL 5651 men, that completed both the questioner and the census in 2 waves (wave 1 and 3)

# ------------------------------------------------------------------------------
# Load required packages for modeling
# ------------------------------------------------------------------------------
library(Matrix)            # For advanced matrix operations (if needed later)
library(lme4)              # For fitting mixed-effects models (glmer function)
library(dplyr)             # For data manipulation and piping (%>%)
library(tidyr)
##— install once —─────────────────────────────────────────────
#install.packages("MuMIn")       # Nakagawa & Schielzeth R²
#install.packages("performance") # several pseudo-R² variants
library(MuMIn)
library(performance)
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
# WAVE 1: MERGING AND PROCESSING
# ------------------------------------------------------------------------------
# Merge network attribute data for Wave 1 with additional network datasets.
joined_df_1 <- dataframe_attributes_fr_1 %>%                                # Start with Wave 1 network attributes
  left_join(all_networks_df_1, by = c("name" = "Node"))                     # Merge primary network data using "name" from attributes and "Node" from network

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

# Convert age to numeric and then leave as is.
final_df$age <- as.numeric(final_df$age)                              # Ensure age is numeric
final_df$age <- final_df$age                                          # Age remains unchanged

# ---------------------------
# CREATE NETWORK-DERIVED VARIABLES (WAVE 1)
# ---------------------------
# Calculate the total number of neighbors (sum of depressed and not depressed neighbors).
total_neigh <- final_df$depressed_w1_neigh + final_df$not_depressed_w1_neigh
# Create a variable for the percentage of depressed neighbors.
final_df$percentage_depressed <- ifelse(
  total_neigh == 0,                          # If there are no neighbors...
  0,                                         # assign 0,
  final_df$depressed_w1_neigh / total_neigh    # otherwise, calculate the proportion
)
# Create a variable for the total number of friends (neighbors).
final_df$friends <- ifelse(
  total_neigh == 0,                                                     # If there are no neighbors...
  0,                                                                    # assign 0,
  final_df$depressed_w1_neigh + final_df$not_depressed_w1_neigh           # otherwise, calculate the total count
)

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

# Filter to include only male respondents (assuming gender == 1 indicates male).
final_df <- final_df %>% filter(gender == 1)

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
  left_join(all_networks_df_3, by = c("name" = "Node"))                    # Primary network data for Wave 3

# Merge the joined network data with individual-level data for Wave 3.
final_df <- joined_df_3 %>%
  left_join(individuals_wave3_fr, by = c("name" = "Respondent3"))              # Merge using respondent IDs

# Create the IHV (Interpersonal Household Violence) variable.
final_df$IHV <- ifelse(
  final_df$i0300 == "Yes" | final_df$i0400 == "Yes" | 
    final_df$i0500 == "Yes" | final_df$i0600 == "Yes" | 
    final_df$i0700 == "Yes", 1, 0                                              # If any violence indicator is "Yes", assign 1; else 0
)

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

# Scale remaining variables.
final_df$Wealth <- final_df$Wealth                                   # Wealth remains unchanged
final_df$intransitivity_values <- final_df$intransitivity_values     # intransitivity_values remains unchanged
final_df$d0700 <- final_df$d0700.x                                  # d0700.x remains unchanged
final_df$b0100 <- final_df$b0100.x                                  # b0100.x remains unchanged
final_df$b0600 <- final_df$b0600.x                                  # b0600.x remains unchanged
final_df$indigenous.binary <- final_df$indigenous.binary.x          # indigenous.binary.x remains unchanged

# Rename key variables to intuitive names for Wave 3.
final_df$education <- final_df$b0100.x                # Raw education score becomes "education"
final_df$religion <- as.factor(final_df$b0600.x)                 # Raw religion score becomes "religion"
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

# Scale additional network-level variables.
final_df$Adversaries <- final_df$Adversaries       # Adversaries remains unchanged
final_df$network_size <- final_df$network_size     # Network size remains unchanged
final_df$network_density <- scale(final_df$network_density)  # Network density remains unchanged
final_df$Adv_density <- scale(final_df$Adv_density)         # Adversarial density remains unchanged
final_df$avg_household <- final_df$avg_household     # Average household depression remains unchanged
final_df$number_household <- final_df$number_household  # Household size remains unchanged

final_df$avg_neighbors_degree <- final_df$avg_neighbors_degree 


# Filter to keep only male respondents (assuming gender == 1 indicates male).
final_df <- final_df %>% filter(gender == 1)

# Merge household information from the individuals_wave3 dataset.
ind3_tmp <- individuals_wave3[, c("Respondent3", "Household3")]  # Select respondent and household columns for Wave 3
names(ind3_tmp) <- c("name", "Household")                       # Rename columns to "name" and "Household"
final_df <- merge(final_df, ind3_tmp, by = "name", all.x = TRUE)  # Merge by "name", keeping all rows

# Save the processed Wave 3 data.
final_df_3 <- final_df                                                 # Save final_df as final_df_3
final_df_3$wave <- 3                                                    # Set wave indicator to 3

# ------------------------------------------------------------------------------
# COMBINE PROCESSED DATA FROM WAVES 1 AND 3
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
    village, Household, wave,household_wealth,
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



# Combine the datasets from Waves 1, 3, and 4.
final_df <- rbind(final_df_1, final_df_3)                       # Row-bind all wave datasets

# Remove rows with any NA values
final_df <- final_df %>% drop_na()

# Convert categorical variables to factors
final_df <- as.data.frame(final_df)
cat_vars <- c("depressed")
final_df[cat_vars] <- lapply(final_df[cat_vars], as.factor)

# Ensure all column names are unique and valid
colnames(final_df) <- make.names(colnames(final_df), unique = TRUE)

# Filter to individuals who appear in both waves (1 and 3)
filtered_data <- final_df %>%
  group_by(name) %>%
  filter(n_distinct(wave) == 2, sum(wave == 1) == 1, sum(wave == 3) == 1) %>%
  ungroup() %>%
  distinct()

# Display filtered data
#print(filtered_data)

# ------------------------------------------------------------------------------
# FIT A MIXED-EFFECTS LOGISTIC REGRESSION MODEL USING glmer()
# ------------------------------------------------------------------------------

### NON RELATIVE ####



# Fit the model predicting depression using various fixed effects along with 
# random intercepts for name, Household, and village.
model_men_not_relative_appendix <- glmer(
  depressed ~ as.numeric(age) + marital_status +  percentage_depressed +
    friends + avg_neighbors_degree +intransitivity_values + Adversaries +
    access_routes + education +  religion +
    avg_household + number_household + indigenous_status +
    FI +  network_size + network_density + Adv_density +
    wave + (1 + wave | name) + (1 | Household) + (1 | village),                     # Random intercepts & slope for name, Household, and village
  data = filtered_data,
  family = binomial(link = "logit"),                                        # Logistic regression (binomial family with logit link)
  nAGQ = 0                                                                  # Use Laplace approximation (nAGQ = 0)
)

# Display a summary of the fitted mixed-effects model.
summary(model_men_not_relative_appendix)

save(model_men_not_relative_appendix, file = "../Code/Results/model_not_relative_men_appendix.rda")


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
  #"Domestic Violence",            # Domestic Violence
  "wave"                           # wave
)

# ------------------------------------------------------------------------------
# EXTRACT MODEL COEFFICIENTS, COMPUTE ODDS RATIOS, AND CONFIDENCE INTERVALS
# ------------------------------------------------------------------------------
coefficients <- summary(model_men_not_relative_appendix)$coefficients             # Extract coefficient matrix from model summary
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
## rows actually used by the full model ───────────────────────────
keep   <- rownames(model.frame(model_men_not_relative_appendix))
df_use <- droplevels(filtered_data[keep, , drop = FALSE])   # <─ KEY!


## -----------------------------------------------------------------
## 2. Re-fit both models on *that identical data object*
model_men_not_relative_appendix_full <- update(model_men_not_relative_appendix, data = df_use)

model_men_not_relative_appendix_null <- glmer(
  depressed ~ 1 +  wave + (1 + wave | name) + (1 | Household) + (1 | village),    
  data   = df_use,
  family = binomial(link = "logit"),
  nAGQ   = 0
)

print(anova(model_men_not_relative_appendix_null, model_men_not_relative_appendix_full))

lrt <- anova(model_men_not_relative_appendix_null, model_men_not_relative_appendix_full)
chi_sq <- lrt$Chisq[2]
df     <- lrt$Df[2]
pval   <- lrt$`Pr(>Chisq)`[2]

chi_sq   # likelihood-ratio statistic
df       # degrees of freedom
pval     # p-value

save(model_men_not_relative_appendix_null, file = "../Code/Results/model_not_relative_men_appendix_null.rda")




##— R^2 computation

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model_men_not_relative_appendix)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model_men_not_relative_appendix)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model_men_not_relative_appendix)        # from performance




