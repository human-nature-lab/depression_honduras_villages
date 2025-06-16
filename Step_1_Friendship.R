#install.packages(c("igraph", "dplyr", "readxl", "tidyverse"))

# ---------------------------------------------------------------------------------------
# Load required libraries
# ---------------------------------------------------------------------------------------
library(igraph)    # For network graph construction and analysis
library(dplyr)     # For data manipulation and piping
library(readxl)    # For reading Excel files

# ---------------------------------------------------------------------------------------
# OVERVIEW:
# This script performs the following tasks:
#   1. Loads dosage information and classifies respondents by friend treatment and dosage.
#   2. Loads and preprocesses individual-level data for multiple waves (Wave 1, 3, and 4).
#   3. Loads and preprocesses network connection (friendship) data from the waves.
#   4. Merges and filters datasets to keep only consistent respondent overlap.
#   5. Builds igraph network objects to represent friendships and social connections.
#   6. Calculates various network metrics (like degree, intransitivity, and neighborhood attributes)
#      and merges them with individual-level attributes.
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# Load and Filter Dosage Data
# ---------------------------------------------------------------------------------------
# Read dosage information from a CSV file
dosage_data <- read.csv("outcomes1.csv")

# Keep only unique rows for these key columns: village_code_w1, friend_treatment, and dosage
dosage_data <- unique(dosage_data[, c('village_code_w1', 'friend_treatment', 'dosage')])

# Create an empty list to store dosage values indexed by respondent (or record)
dosage_list <- list()

# For all indices from 1 to 176, store the dosage value
for (index_i in c(1:176)) {
  dosage_list[[index_i]] <- dosage_data[index_i, ]$dosage
}

# Initialize a nested list to hold groups based on treatment (0 or 1) and different dosage levels
arm_index <- list(list(), list())

# Define the possible dosage levels we want to consider
dosage_levels <- c(0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.75, 1)

# For each treatment group and dosage level, collect the corresponding village_code_w1 values
for (treatment_j in 1:2) {
  for (dose_i in 1:8) {
    arm_index[[treatment_j]][[dose_i]] <- dosage_data[
      dosage_data$friend_treatment == (treatment_j - 1) &  # treatment adjusted to 0/1
        dosage_data$dosage == dosage_levels[dose_i],
    ]$village_code_w1
  }
}

# ---------------------------------------------------------------------------------------
# Load and Preprocess Individual-level Data for Wave 1
# ---------------------------------------------------------------------------------------
# Read the raw individual data for Wave 1 (with 50467 records)
individuals_wave1_raw <- read.csv("individuals_wave1.csv")

# Filter to include only complete records from data source 1
individuals_wave1_raw <- individuals_wave1_raw[
  !is.na(individuals_wave1_raw$complete) & 
    individuals_wave1_raw$complete == 1 & 
    individuals_wave1_raw$data_source_w1 == 1, 
]

# Select only the needed variables. Now we have 24922 individuals.
individuals_wave1_raw <- cbind(
  individuals_wave1_raw$respondent_master_id,
  individuals_wave1_raw$resp_target,
  individuals_wave1_raw$building_id_w1,
  individuals_wave1_raw$gender,
  individuals_wave1_raw$marital_name,
  individuals_wave1_raw$age_at_survey,
  individuals_wave1_raw$village_code_w1
)

# Convert the matrix to a data frame and remove rows with any missing values
individuals_wave1 <- as.data.frame(individuals_wave1_raw)
individuals_wave1 <- na.omit(individuals_wave1)

# Rename the columns to make them clear and consistent
colnames(individuals_wave1) <- c('Respondent1', 'Treatment', 'Household1', 
                                 'gender1', 'marital_status1', 'age1', 'village1')

# ---------------------------------------------------------------------------------------
# Load and Preprocess Individual-level Data for Wave 3
# ---------------------------------------------------------------------------------------
# Read the raw individual data for Wave 3 (48723 records)
individuals_wave3_raw <- read.csv("individuals_wave3.csv")

# Filter to keep only complete records from data source 3 (now 17595 individuals)
individuals_wave3_raw <- individuals_wave3_raw[
  !is.na(individuals_wave3_raw$complete) &
    individuals_wave3_raw$complete == 1 &
    individuals_wave3_raw$data_source_w3 == 1, 
]

# Keep the selected variables from Wave 3
individuals_wave3_raw <- cbind(
  individuals_wave3_raw$respondent_master_id,
  individuals_wave3_raw$resp_target,
  individuals_wave3_raw$building_id_w3,
  individuals_wave3_raw$gender,
  individuals_wave3_raw$marital_name,
  individuals_wave3_raw$age_at_survey,
  individuals_wave3_raw$village_code_w3
)

# Convert to a data frame and remove rows with missing values (resulting in 17590 individuals)
individuals_wave3 <- as.data.frame(individuals_wave3_raw)
individuals_wave3 <- na.omit(individuals_wave3)

# Rename columns for clarity
colnames(individuals_wave3) <- c('Respondent3', 'Treatment', 'Household3', 'gender', 
                                 'marital_status', 'age', 'village')

# ---------------------------------------------------------------------------------------
# Load and Preprocess Individual-level Data for Wave 4
# ---------------------------------------------------------------------------------------
# Read the raw individual data for Wave 4 (25598 records)
individuals_wave4_raw <- read.csv("individuals_wave4.csv")

# Filter Wave 4 respondents: complete records, no status flag, and correct data source (8216 individuals)
individuals_wave4_raw <- individuals_wave4_raw[
  !is.na(individuals_wave4_raw$complete) &
    is.na(individuals_wave4_raw$status_w4) &
    (individuals_wave4_raw$complete == 1) &
    (individuals_wave4_raw$data_source_w4 == 1), 
]

# Keep the selected variables for Wave 4
individuals_wave4_raw <- cbind(
  individuals_wave4_raw$respondent_master_id,
  individuals_wave4_raw$building_id_w4,
  individuals_wave4_raw$building_id_w3,
  individuals_wave4_raw$gender,
  individuals_wave4_raw$marital_name,
  individuals_wave4_raw$age_at_survey,
  individuals_wave4_raw$village_code_w4
)

# Rename columns for Wave 4 for clarity
colnames(individuals_wave4_raw) <- c('Respondent4', 'Household4', 'Household3', 'gender', 
                                     'marital_status', 'age', 'village')

# Convert to a data frame and remove rows with missing values (8216 individuals)
individuals_wave4 <- na.omit(as.data.frame(individuals_wave4_raw))

# ---------------------------------------------------------------------------------------
# Load and Filter Network (Friendship) Connection Data for Waves 1, 3, and 4
# ---------------------------------------------------------------------------------------
# Read connection data from CSV files
connections_w1_fr <- read.csv("health_wave1.csv")  # 517877 connections
connections_w3_fr <- read.csv("health_wave3.csv")  # 424383 connections
connections_w4_fr <- read.csv("health_wave4.csv")  # 208206 connections

# Filter to include only specific friendship relationships and where alter_source equals 1
connections_w1_fr <- connections_w1_fr[
  (connections_w1_fr$relationship %in% c("closest_friend", "personal_private", "free_time")) &
    (connections_w1_fr$alter_source == 1), 
]

connections_w3_fr <- connections_w3_fr[
  (connections_w3_fr$relationship %in% c("closest_friend", "personal_private", "free_time")) &
    (connections_w3_fr$alter_source == 1), 
]

connections_w4_fr <- connections_w4_fr[
  (connections_w4_fr$relationship %in% c("closest_friend", "personal_private", "free_time")) &
    (connections_w4_fr$alter_source == 1), 
]

# For each wave, keep the relevant columns and rename them
connections_w1_fr <- cbind(connections_w1_fr$ego, connections_w1_fr$alter, connections_w1_fr$village_code_w1)
colnames(connections_w1_fr) <- c('Respondent1', 'Alter', 'Village1')

connections_w3_fr <- cbind(connections_w3_fr$ego, connections_w3_fr$alter, connections_w3_fr$village_code_w3)
colnames(connections_w3_fr) <- c('Respondent3', 'Alter', 'Village3')

connections_w4_fr <- cbind(connections_w4_fr$ego, connections_w4_fr$alter, connections_w4_fr$village_code_w4)
colnames(connections_w4_fr) <- c('Respondent4', 'Alter', 'Village4')

# Assign the filtered connections to separate variables for each wave
connections_individuals1 <- connections_w1_fr  # 155044 connections after filtering
connections_individuals3 <- connections_w3_fr  # 119811 connections after filtering
connections_individuals4 <- connections_w4_fr  # 61601 connections after filtering

# ---------------------------------------------------------------------------------------
# Harmonize Column Names and Subset Attributes for Respondents Across Waves
# ---------------------------------------------------------------------------------------
# Ensure that the individuals data frames have consistent column names and order
individuals_wave1 <- as.data.frame(individuals_wave1[, 1:7])
colnames(individuals_wave1) <- c('Respondent1', 'Treatment', 'Household1', 'gender', 
                                 'marital_status', 'age', 'village')

individuals_wave3 <- as.data.frame(individuals_wave3[, 1:7])
colnames(individuals_wave3) <- c('Respondent3', 'Treatment', 'Household3', 'gender', 
                                 'marital_status', 'age', 'village')

individuals_wave4 <- as.data.frame(individuals_wave4[, 1:7])
colnames(individuals_wave4) <- c('Respondent4', 'Household4', 'Household3', 'gender', 
                                 'marital_status', 'age', 'village')

# Merge connection data with corresponding individual-level data to enrich the network information
# Merge for Wave 1 connections: join on Respondent1 and then join for Alter details
connections_individuals1 <- merge(connections_individuals1, individuals_wave1, 
                                  by.x = 'Respondent1', by.y = 'Respondent1')
connections_individuals1 <- merge(connections_individuals1, individuals_wave1, 
                                  by.x = 'Alter', by.y = 'Respondent1')
connections_individuals1 <- na.omit(as.data.frame(connections_individuals1))  # now 134654 connections

# Merge for Wave 3 connections: similar approach for Respondent3 and Alter
connections_individuals3 <- merge(connections_individuals3, individuals_wave3, 
                                  by.x = 'Respondent3', by.y = 'Respondent3')
connections_individuals3 <- merge(connections_individuals3, individuals_wave3, 
                                  by.x = 'Alter', by.y = 'Respondent3')
connections_individuals3 <- na.omit(as.data.frame(connections_individuals3))  # now 72187 connections

# Merge for Wave 4 connections: using Respondent4 join keys
connections_individuals4 <- merge(connections_individuals4, individuals_wave4, 
                                  by = 'Respondent4')
connections_individuals4 <- merge(connections_individuals4, individuals_wave4, 
                                  by.x = 'Alter', by.y = 'Respondent4')
connections_individuals4 <- na.omit(as.data.frame(connections_individuals4))  # now 35267 connections

# Extract unique village codes from the connections (as numeric values)
villages_1_3 <- c(1:176) # 176 villages
villages_4   <- as.numeric(unique(connections_individuals4[, 3]))       # 82 villages

# Initialize lists to store village-level connection data and edge lists for network graphs
village_connection1 <- list()
village_connection3 <- list()
edges1 <- list()
edges3 <- list()

# Prepare lists to store igraph objects for each village in Waves 1 and 3
gr1 <- list()         # Wave 1 graphs
gr3 <- list()         # Wave 3 graphs

# ---------------------------------------------------------------------------------------
# Create Graphs for Each Village for Wave 1 and Wave 3
# ---------------------------------------------------------------------------------------
for (village_i in villages_1_3) {
  
  # Subset connections for the current village in Wave 3 and Wave 1
  village_connection3[[village_i]] <- connections_individuals3[connections_individuals3$Village3 == village_i, ]
  village_connection1[[village_i]] <- connections_individuals1[connections_individuals1$Village1 == village_i, ]
  
  # Extract the edge lists (columns for Respondent and Alter) for graph construction
  edges3[[village_i]] <- village_connection3[[village_i]][, c('Respondent3', 'Alter')]
  edges1[[village_i]] <- village_connection1[[village_i]][, c('Respondent1', 'Alter')]
  
  # Create undirected igraph objects from the edge lists for each wave
  gr3[[village_i]] <- graph_from_edgelist(as.matrix(edges3[[village_i]]), directed = FALSE)
  gr1[[village_i]] <- graph_from_edgelist(as.matrix(edges1[[village_i]]), directed = FALSE)
  
  # Add any missing vertices (nodes) that are in the individual data but not in the graph (for Wave 1)
  individuals_set <- individuals_wave1[individuals_wave1$village == village_i, ]$Respondent1
  missing_nodes <- unique(c(setdiff(individuals_set, V(gr1[[village_i]])$name)))
  if (length(missing_nodes) > 0) {
    gr1[[village_i]] <- igraph::add_vertices(gr1[[village_i]], length(missing_nodes), name = missing_nodes)
  }
  
  # Repeat the missing node addition for Wave 3
  individuals_set <- individuals_wave3[individuals_wave3$village == village_i, ]$Respondent3
  missing_nodes <- unique(c(setdiff(individuals_set, V(gr3[[village_i]])$name)))
  if (length(missing_nodes) > 0) {
    gr3[[village_i]] <- igraph::add_vertices(gr3[[village_i]], length(missing_nodes), name = missing_nodes)
  }
  
  # Simplify the graphs to remove any duplicate edges (multiedges)
  gr1[[village_i]] <- igraph::simplify(gr1[[village_i]], remove.multiple = TRUE)
  gr3[[village_i]] <- igraph::simplify(gr3[[village_i]], remove.multiple = TRUE)
}

# ---------------------------------------------------------------------------------------
# Load Additional Data: Depression Data
# ---------------------------------------------------------------------------------------
# Load a previously saved R data object for depression measures
load("resp_paternal_depr_dfs_20240515.rda")

# If tidyverse is not installed, install it; then load tidyverse for additional data manipulation
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# ---------------------------------------------------------------------------------------
# WAVE 1: Add and Encode Additional Attributes
# ---------------------------------------------------------------------------------------

# ----- Gender Encoding -----
# Convert gender to numeric: "male" becomes 1, else 0
individuals_wave1[,'gender'] <- ifelse(individuals_wave1[,'gender'] == "male", 1, 0)

# ----- Marital Status (Partnered vs. Unpartnered) -----
# First recode the various marital statuses to numeric codes
individuals_wave1[,'marital_status'] <- ifelse(individuals_wave1[,'marital_status'] == "Single", 1,
                                               ifelse(individuals_wave1[,'marital_status'] == "Widowed", 2,
                                                      ifelse(individuals_wave1[,'marital_status'] == "Separated", 3,
                                                             ifelse(individuals_wave1[,'marital_status'] == "Civil Union", 4,
                                                                    ifelse(individuals_wave1[,'marital_status'] == "Married", 5, 6)))))

# Then recode: treat codes 4 and 5 as partnered (1) and others as 0; treat code 6 as missing (NA)
individuals_wave1$marital_status <- ifelse(individuals_wave1$marital_status == 6, NA,
                                           ifelse(individuals_wave1$marital_status %in% c(4,5), 1, 0))

# (Note: Alternative code is commented out below.)

# ----- Adding Village-Level Attributes -----
# Read village-level characteristics from a CSV file and select key variables
village_characteristics <- read.csv("honduras_villages_WAVE1_v8.csv")
village_characteristics <- village_characteristics[, c('village_code', 'aldea_latitude', 'aldea_longitude', 
                                                       'time_to_main_road', 'health_center', 
                                                       'time_to_health_ctr', 'maternal_clinic', 'time_to_mat_clinic')]

# Ensure the village column is numeric and merge the village characteristics with the individual data
individuals_wave1[, 'village'] <- as.numeric(individuals_wave1[, 'village'])
individuals_wave1 <- merge(individuals_wave1, village_characteristics, by.x = 'village', by.y = 'village_code', all = FALSE)

# ----- Adding Access Routes Data -----
# Read an Excel file that contains data on access routes and extract relevant columns
data <- read_excel("Built Environment Indices_MDALR_V2.xlsx")
data_extracted <- data %>% dplyr::select(village_code, access_routes)
# Left join the access routes to the Wave 1 data based on the village code
individuals_wave1 <- individuals_wave1 %>%
  left_join(data_extracted, by = c("village" = "village_code"))

# ----- Adding Depression Characteristics -----
psycho_characteristics <- resp_w1_parental_depr  # Loaded earlier from the .rda file
individuals_wave1 <- merge(individuals_wave1, psycho_characteristics, 
                           by.x = 'Respondent1', by.y = 'respondent_master_id', all = FALSE)

# ----- Education Data Processing -----
education <- read.csv("hondras_demog_w1_to_w4_marios_2024-02-13.csv")
# Select the relevant education columns
education <- education %>% dplyr::select(respondent_master_id, b0100, b0200, b0600)

# Convert education levels from text to numeric codes (example mapping provided)
education$b0100 <- ifelse(education$b0100 == "1st grade", 1,
                          ifelse(education$b0100 == "2nd grade", 2,
                                 ifelse(education$b0100 == "3rd grade", 3,
                                        ifelse(education$b0100 == "4th grade", 4,
                                               ifelse(education$b0100 == "5th grade", 5,
                                                      ifelse(education$b0100 == "6th grade", 6,
                                                             ifelse(education$b0100 == "Some secondary", 8,
                                                                    ifelse(education$b0100 == "Secondary", 11,
                                                                           ifelse(education$b0100 == "Have not completed any type of school", 0,
                                                                                  ifelse(education$b0100 == "More than secondary", 13, 0))))))))))

# Encode religion information: using case_when for clarity
education <- education %>% mutate(b0600 = case_when(is.na(b0600) ~ NA_real_,
                                                    b0600 == "Catholic" ~ 2,
                                                    b0600 == "Protestant" ~ 1,
                                                    b0600 == "No Religion" ~ 0,
                                                    b0600 %in% c("Dont_Know","Refused","Mormon") ~ NA_real_,
                                                    TRUE ~ NA_real_))
# Merge education data into the Wave 1 individual data
individuals_wave1 <- merge(individuals_wave1, education, 
                           by.x = 'Respondent1', by.y = 'respondent_master_id', all = FALSE)

# ----- Recode Depression Measures (Using Items c0300 and c0400) -----
individuals_wave1 <- individuals_wave1 %>%
  mutate(c0300_num = dplyr::recode(c0300,
                                   "Not at all" = 0,
                                   "Several days" = 1,
                                   "More than half the days" = 2,
                                   "Nearly every day" = 3,
                                   .default = NA_real_),
         c0400_num = dplyr::recode(c0400,
                                   "Not at all" = 0,
                                   "Several days" = 1,
                                   "More than half the days" = 2,
                                   "Nearly every day" = 3,
                                   .default = NA_real_)) %>%
  mutate(total_score = c0300_num + c0400_num,
         depressed_w1 = ifelse(total_score >= 2, 1, 0)) %>%
  dplyr::select(-c0300, -c0400, -c0300_num, -c0400_num, -total_score)

# ----- Add Additional Individual Characteristics -----
indiv_char <- read.csv("individuals_1.csv") %>%
  dplyr::select(respondent_master_id, d0100, d0200, d0700)
individuals_wave1 <- individuals_wave1 %>%
  left_join(indiv_char, by = c("Respondent1" = "respondent_master_id"))

# ----- Encode Individual Wealth -----
# Recode wealth responses to numeric values
individuals_wave1$d0700 <- ifelse(individuals_wave1$d0700 == 'There is enough to live on and save', 4,
                                  ifelse(individuals_wave1$d0700 == 'It is sufficient, without major difficulties', 3,
                                         ifelse(individuals_wave1$d0700 == 'It is not sufficient and there are difficulties', 2,
                                                ifelse(individuals_wave1$d0700 == 'It is not sufficient and there are major difficulties', 1, 0))))

# ----- Food Insufficiency Measures -----
# Recode food insufficiency responses to binary values
individuals_wave1$d0100 <- ifelse(individuals_wave1$d0100 == 'Yes', 1, 0)
individuals_wave1$d0200 <- ifelse(individuals_wave1$d0200 == 'Yes', 1, 0)

# Create a Food Insecurity (FI) measure based on the two food insufficiency items
individuals_wave1$FI <- ifelse(individuals_wave1$d0100 == 1 & individuals_wave1$d0200 == 1, 3,
                               ifelse(individuals_wave1$d0100 == 0 & individuals_wave1$d0200 == 0, 1, 2))

# Recode FI to binary using thresholds based on literature (e.g., Kumar et al.)
individuals_wave1 <- individuals_wave1 %>% mutate(FI = case_when(FI == 2 | FI == 3 ~ 1,
                                                                 FI == 1 ~ 0,
                                                                 TRUE ~ NA_real_))

# ----- Encode Indigenous Status -----
individuals_wave1 <- individuals_wave1 %>%
  mutate(indigenous.binary = case_when(
    b0200 %in% c("Yes, Lenca", "Yes, Maya Chorti", "Other") ~ 1,
    b0200 == "No" ~ 0,
    TRUE ~ NA_real_
  ))

# ----- Add Household Wealth (Wave 1) -----
# Read household data and select relevant columns
households_wave1 <- read.csv("household_wave1.csv")
households_wave1 <- households_wave1[, c('building_id', 'household_wealth_index_w1', 'building_latitude', 'building_longitude')]
colnames(households_wave1) <- c('Household', 'Wealth1', 'Latitude', 'Longitude')
# Merge household wealth with individual data based on household identifier
individuals_wave1 <- individuals_wave1 %>%
  left_join(households_wave1, by = c("Household1" = "Household"))

# ---------------------------------------------------------------------------------------
# WAVE 3: Similar Attribute Encoding as Wave 1
# ---------------------------------------------------------------------------------------
# (Re-load parental depression if needed)
load("resp_paternal_depr_dfs_20240515.rda")
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# ----- Gender Encoding for Wave 3 -----
individuals_wave3[,'gender'] <- ifelse(individuals_wave3[,'gender'] == "male", 1, 0)

# ----- Marital Status Encoding for Wave 3 -----
individuals_wave3[,'marital_status'] <- ifelse(individuals_wave3[,'marital_status'] == "Single", 1,
                                               ifelse(individuals_wave3[,'marital_status'] == "Widowed", 2,
                                                      ifelse(individuals_wave3[,'marital_status'] == "Separated", 3,
                                                             ifelse(individuals_wave3[,'marital_status'] == "Civil Union", 4,
                                                                    ifelse(individuals_wave3[,'marital_status'] == "Married", 5, 6)))))
individuals_wave3$marital_status <- ifelse(individuals_wave3$marital_status == 6, NA,
                                           ifelse(individuals_wave3$marital_status %in% c(4,5), 1, 0))

# ----- Merge Village Characteristics into Wave 3 Data -----
village_characteristics <- read.csv("honduras_villages_WAVE1_v8.csv")
village_characteristics <- village_characteristics[, c('village_code', 'aldea_latitude', 'aldea_longitude', 
                                                       'time_to_main_road', 'health_center', 
                                                       'time_to_health_ctr', 'maternal_clinic', 'time_to_mat_clinic')]
individuals_wave3[, 'village'] <- as.numeric(individuals_wave3[, 'village'])
individuals_wave3 <- merge(individuals_wave3, village_characteristics, 
                           by.x = 'village', by.y = 'village_code', all = FALSE)
individuals_wave3 <- individuals_wave3 %>%
  left_join(data_extracted, by = c("village" = "village_code"))

# ----- Merge Depression for Wave 3 -----
psycho_characteristics <- resp_w3_parental_depr
individuals_wave3 <- merge(individuals_wave3, psycho_characteristics, 
                           by.x = 'Respondent3', by.y = 'respondent_master_id', all = FALSE)

# ----- Process Education Data for Wave 3 -----
education <- read.csv("hondras_demog_w1_to_w4_marios_2024-02-13.csv")
education <- education %>% dplyr::select(respondent_master_id, b0100, b0200, b0600)
education$b0100 <- ifelse(education$b0100 == "1st grade", 1,
                          ifelse(education$b0100 == "2nd grade", 2,
                                 ifelse(education$b0100 == "3rd grade", 3,
                                        ifelse(education$b0100 == "4th grade", 4,
                                               ifelse(education$b0100 == "5th grade", 5,
                                                      ifelse(education$b0100 == "6th grade", 6,
                                                             ifelse(education$b0100 == "Some secondary", 8,
                                                                    ifelse(education$b0100 == "Secondary", 11,
                                                                           ifelse(education$b0100 == "Have not completed any type of school", 0,
                                                                                  ifelse(education$b0100 == "More than secondary", 13, 0))))))))))
education <- education %>% mutate(b0600 = case_when(is.na(b0600) ~ NA_real_,
                                                    b0600 == "Catholic" ~ 2,
                                                    b0600 == "Protestant" ~ 1,
                                                    b0600 == "No Religion" ~ 0,
                                                    b0600 %in% c("Dont_Know","Refused","Mormon") ~ NA_real_,
                                                    TRUE ~ NA_real_))
individuals_wave3 <- merge(individuals_wave3, education, 
                           by.x = 'Respondent3', by.y = 'respondent_master_id', all = FALSE)

# ----- Recode Depression Items for Wave 3 -----
individuals_wave3 <- individuals_wave3 %>%
  mutate(c0300_num = dplyr::recode(c0300,
                                   "Not at all" = 0,
                                   "Several days" = 1,
                                   "More than half the days" = 2,
                                   "Nearly every day" = 3,
                                   .default = NA_real_),
         c0400_num = dplyr::recode(c0400,
                                   "Not at all" = 0,
                                   "Several days" = 1,
                                   "More than half the days" = 2,
                                   "Nearly every day" = 3,
                                   .default = NA_real_),
         total_score = c0300_num + c0400_num,
         depressed_w3 = ifelse(total_score >= 2, 1, 0)) %>%
  dplyr::select(-c0300, -c0400, -c0300_num, -c0400_num, -total_score)

# ----- Merge Additional Individual Characteristics for Wave 3 -----
indiv_char <- read.csv("honduras_respondents_WAVE3_v3_marios_2024-06-03.csv") %>%
  dplyr::select(respondent_master_id, d0100, d0200, d0700, e0300, c1100, c1200, c1300, i0300, i0400, i0500, i0600, i0700)
individuals_wave3 <- individuals_wave3 %>%
  left_join(indiv_char, by = c("Respondent3" = "respondent_master_id"))

# ----- Encode Individual Wealth for Wave 3 -----
individuals_wave3$d0700 <- ifelse(individuals_wave3$d0700 == 'There is enough to live on and save', 4,
                                  ifelse(individuals_wave3$d0700 == 'It is sufficient, without major difficulties', 3,
                                         ifelse(individuals_wave3$d0700 == 'It is not sufficient and there are difficulties', 2,
                                                ifelse(individuals_wave3$d0700 == 'It is not sufficient and there are major difficulties', 1, 0))))

# ----- Encode Food Insecurity for Wave 3 -----
individuals_wave3$d0100 <- ifelse(individuals_wave3$d0100 == 'Yes', 1, 0)
individuals_wave3$d0200 <- ifelse(individuals_wave3$d0200 == 'Yes', 1, 0)
individuals_wave3$FI <- ifelse(individuals_wave3$d0100 == 1 & individuals_wave3$d0200 == 1, 3,
                               ifelse(individuals_wave3$d0100 == 0 & individuals_wave3$d0200 == 0, 1, 2))
# Recode FI for Wave 3 based on similar thresholds as Wave 1
individuals_wave3 <- individuals_wave3 %>% mutate(FI = case_when(FI == 2 | FI == 3 ~ 1,
                                                                 FI == 1 ~ 0,
                                                                 TRUE ~ NA_real_))
# ---------------------------------------------------------------------------------------
# Encode Indigenous Status, Cohabitating Status, and IPV History for Wave 3
# ---------------------------------------------------------------------------------------
# Using dplyr's mutate with case_when to create new variables based on responses
individuals_wave3 <- individuals_wave3 %>%
  mutate(
    # Create a binary variable for indigenous status:
    # If b0200 indicates "Yes, Lenca", "Yes, Maya Chorti", or "Other", set to 1; if "No", set to 0;
    # Otherwise, assign NA.
    indigenous.binary = case_when(
      b0200 %in% c("Yes, Lenca", "Yes, Maya Chorti", "Other") ~ 1,
      b0200 == "No" ~ 0,
      TRUE ~ NA_real_
    )
  )

# ---------------------------------------------------------------------------------------
# Get Household Wealth for Wave 3
# ---------------------------------------------------------------------------------------
# Read in the household data for Wave 3
households_wave3 <- read.csv("household_wave3.csv")
# Subset the columns that we need: building_id, wealth index, latitude, and longitude
households_wave3 <- households_wave3[, c('building_id', 'household_wealth_index_w3', 'building_latitude', 'building_longitude')]
# Rename the columns to more descriptive names
colnames(households_wave3) <- c('Household', 'Wealth3', 'Latitude', 'Longitude')
# Merge the household wealth information into the individuals_wave3 data using the household identifier
individuals_wave3 <- individuals_wave3 %>%
  left_join(households_wave3, by = c("Household3" = "Household"))

# ---------------------------------------------------------------------------------------
# Assign Final Variables for Friend Relationships
# ---------------------------------------------------------------------------------------
# For consistency, assign the current versions of the individuals and graph objects to friend-specific variables
individuals_wave1_fr <- individuals_wave1
individuals_wave3_fr <- individuals_wave3
gr1_fr <- gr1
gr3_fr <- gr3

# Then reassign these friend-specific datasets back to the original individuals data frames
individuals_wave1 <- individuals_wave1_fr
individuals_wave3 <- individuals_wave3_fr

# ---------------------------------------------------------------------------------------
# Functions to Compute Social Intransitivity
# ---------------------------------------------------------------------------------------

# This function computes social intransitivity for a graph
calculate_social_intransitivity <- function(g) {
  # Count the number of triangles (closed triads) in the graph
  triangles <- count_triangles(g)
  # Get the degree (number of edges) for each node
  degrees <- degree(g)
  # Calculate the maximum possible number of triads for each node
  possible_triads <- degrees * (degrees - 1) / 2
  # Calculate the number of open triads (possible triads minus closed triads)
  open_triads <- possible_triads - triangles
  # Compute the social intransitivity as the fraction of open triads,
  # and return 0 if no triads are possible
  social_intransitivity <- ifelse(possible_triads > 0, open_triads / possible_triads, 0)
  return(social_intransitivity)
}

# This function computes an intransitivity index for each node in a graph.
intransitivity_index <- function(graph) {
  # Convert the graph to an undirected version to ignore directionality
  undirected_graph <- as.undirected(graph, mode = "collapse")
  # Initialize a numeric vector to store the intransitivity index for each node
  intransitivity_indices <- numeric(vcount(undirected_graph))
  
  # Loop over each node in the graph
  for (node in V(undirected_graph)) {
    # Get the neighbors of the current node
    neighs <- neighbors(undirected_graph, node)
    num_neighbors <- length(neighs)
    
    # If fewer than 2 neighbors exist, there can be no triads, so assign 0
    if (num_neighbors < 2) {
      intransitivity_indices[node] <- 0
    } else {
      two_step_relations <- 0
      non_direct_relations <- 0
      
      # For every unique pair of neighbors, count as a potential two-step relation
      # and check if the two neighbors are not directly connected.
      for (i in 1:(num_neighbors - 1)) {
        for (j in (i + 1):num_neighbors) {
          two_step_relations <- two_step_relations + 1
          if (!are_adjacent(undirected_graph, neighs[i], neighs[j])) {
            non_direct_relations <- non_direct_relations + 1
          }
        }
      }
      # The intransitivity index for the node is the fraction of non-direct connections
      intransitivity_indices[node] <- non_direct_relations / two_step_relations
    }
  }
  
  return(intransitivity_indices)
}

# ---------------------------------------------------------------------------------------
# Add Depression Attributes to Wave 1 and Wave 3 Graphs
# ---------------------------------------------------------------------------------------
# Loop over each village (as defined in villages_1_3) to add node-level depression data
for (village_i in villages_1_3) {
  # Ensure the names of vertices in the graphs are character strings
  V(gr1_fr[[village_i]])$name <- as.character(V(gr1_fr[[village_i]])$name)
  
  # Ensure that the column names in the individual data frames are valid and unique
  names(individuals_wave1) <- make.names(names(individuals_wave1), unique = TRUE)
  
  # For Wave 1: Filter individuals that are in the current village graph and select depression data
  node_data_1 <- individuals_wave1 %>%
    dplyr::filter(Respondent1 %in% V(gr1_fr[[village_i]])$name) %>%
    dplyr::select(Respondent1, depressed_w1)
  
 
  
  # Loop over each vertex in the Wave 1 graph
  for (vertex_id in V(gr1_fr[[village_i]])) {
    vertex_name <- V(gr1_fr[[village_i]])[vertex_id]$name
    # If this vertex exists in the depression data, attach the depressed_w1 attribute
    if (vertex_name %in% node_data_1$Respondent1) {
      V(gr1_fr[[village_i]])[vertex_id]$depressed_w1 <-
        node_data_1[node_data_1$Respondent1 == vertex_name, ]$depressed_w1
    }
  }

  if(village_i!=155 | village_i!=156){

    V(gr3_fr[[village_i]])$name <- as.character(V(gr3_fr[[village_i]])$name)
    names(individuals_wave3) <- make.names(names(individuals_wave3), unique = TRUE)

    # For Wave 3: Similarly, filter individuals and select depression data
    node_data_3 <- individuals_wave3 %>%
      dplyr::filter(Respondent3 %in% V(gr3_fr[[village_i]])$name) %>%
      dplyr::select(Respondent3, depressed_w3)
    
    # Loop over each vertex in the Wave 3 graph and do the same for depressed_w3
    for (vertex_id in V(gr3_fr[[village_i]])) {
      vertex_name <- V(gr3_fr[[village_i]])[vertex_id]$name
      if (vertex_name %in% node_data_3$Respondent3) {
        V(gr3_fr[[village_i]])[vertex_id]$depressed_w3 <-
          node_data_3[node_data_3$Respondent3 == vertex_name, ]$depressed_w3
      }
    }
  }
}

# ---------------------------------------------------------------------------------------
# Compute Intransitivity and Neighbor-based Depression Attributes for Wave 1 Graphs
# ---------------------------------------------------------------------------------------
for (village_i in villages_1_3) {
  # Compute intransitivity values for the current Wave 1 graph
  intransitivity_values <- intransitivity_index(gr1_fr[[village_i]])
  
  # Loop over each vertex in the current village graph
  for (v in V(gr1_fr[[village_i]])) {
    # Get the list of all neighbors (connections) for the current vertex
    neighbors_v <- neighbors(gr1_fr[[village_i]], v, mode = "all")
    
    # Calculate the number of neighboring nodes that are depressed (depressed_w1 == 1)
    if (length(neighbors_v) > 0) {
      V(gr1_fr[[village_i]])[v]$depressed_w1_neigh <- sum(V(gr1_fr[[village_i]])[neighbors_v]$depressed_w1 == 1)
    } else {
      V(gr1_fr[[village_i]])[v]$depressed_w1_neigh <- 0
    }
    
    # Similarly, calculate the number of neighbors that are not depressed (depressed_w1 == 0)
    if (length(neighbors_v) > 0) {
      V(gr1_fr[[village_i]])[v]$not_depressed_w1_neigh <- sum(V(gr1_fr[[village_i]])[neighbors_v]$depressed_w1 == 0)
    } else {
      V(gr1_fr[[village_i]])[v]$not_depressed_w1_neigh <- 0
    }
    
    # Assign the computed intransitivity value to the vertex
    V(gr1_fr[[village_i]])[v]$intransitivity_values <- intransitivity_values[v]
  }
}

# ---------------------------------------------------------------------------------------
# Compute Intransitivity and Neighbor-based Depression Attributes for Wave 3 Graphs
# ---------------------------------------------------------------------------------------
for (village_i in setdiff(villages_1_3, c(155,156))) {
  # Compute the intransitivity index for the current Wave 3 graph
  intransitivity_values <- intransitivity_index(gr3_fr[[village_i]])
  
  # Loop over each vertex in the current village graph
  for (v in V(gr3_fr[[village_i]])) {
    # Get the neighbors for the current vertex
    neighbors_v <- neighbors(gr3_fr[[village_i]], v, mode = "all")
    
    # Count how many neighbors are depressed in Wave 3
    if (length(neighbors_v) > 0) {
      V(gr3_fr[[village_i]])[v]$depressed_w3_neigh <- sum(V(gr3_fr[[village_i]])[neighbors_v]$depressed_w3 == 1)
    } else {
      V(gr3_fr[[village_i]])[v]$depressed_w3_neigh <- 0
    }
    
    # Count how many neighbors are not depressed in Wave 3
    if (length(neighbors_v) > 0) {
      V(gr3_fr[[village_i]])[v]$not_depressed_w3_neigh <- sum(V(gr3_fr[[village_i]])[neighbors_v]$depressed_w3 == 0)
    } else {
      V(gr3_fr[[village_i]])[v]$not_depressed_w3_neigh <- 0
    }
    
    # Attach the intransitivity value for this vertex
    V(gr3_fr[[village_i]])[v]$intransitivity_values <- intransitivity_values[v]
  }
}

# ---------------------------------------------------------------------------------------
# Add Wealth, FI, Marital, Age, Gender and Compute Neighbor-based Statistics for Wave 1 Graphs
# ---------------------------------------------------------------------------------------
# Loop over villages (excluding village 156) to add additional attributes to the graph nodes
for (village_i in villages_1_3) {
  if (!is.null(gr1_fr[[village_i]])) {
    # Ensure the vertex names are characters
    V(gr1_fr[[village_i]])$name <- as.character(V(gr1_fr[[village_i]])$name)
    
    # Create a subset of individual-level data for nodes in the current village,
    # selecting variables for wealth, food insecurity (FI), marital status, age, gender,
    # education (b0100), indigenous status, religion (b0600), and household wealth (d0700)
    node_data_1 <- individuals_wave1 %>%
      dplyr::filter(Respondent1 %in% V(gr1_fr[[village_i]])$name) %>%
      dplyr::select(Respondent1, Wealth1, FI, marital_status, age, gender, b0100, indigenous.binary, b0600, d0700)
    
    # Loop over every vertex in the current village graph
    for (v in seq_along(V(gr1_fr[[village_i]]))) {
      vertex_name <- V(gr1_fr[[village_i]])[v]$name
      # If the vertex has a matching record in the individual data, assign the attributes to the vertex
      if (vertex_name %in% node_data_1$Respondent1) {
        V(gr1_fr[[village_i]])[v]$Wealth1 <- node_data_1$Wealth1[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$FI <- node_data_1$FI[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$marital_status <- node_data_1$marital_status[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$age <- node_data_1$age[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$gender <- node_data_1$gender[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$b0100 <- node_data_1$b0100[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$indigenous.binary <- node_data_1$indigenous.binary[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$b0600 <- node_data_1$b0600[node_data_1$Respondent1 == vertex_name]
        V(gr1_fr[[village_i]])[v]$d0700 <- node_data_1$d0700[node_data_1$Respondent1 == vertex_name]
      } else {
        # If no matching record is found, assign NA to all attributes for that vertex
        V(gr1_fr[[village_i]])[v]$Wealth1 <- NA
        V(gr1_fr[[village_i]])[v]$FI <- NA
        V(gr1_fr[[village_i]])[v]$marital_status <- NA
        V(gr1_fr[[village_i]])[v]$age <- NA
        V(gr1_fr[[village_i]])[v]$gender <- NA
        V(gr1_fr[[village_i]])[v]$b0100 <- NA
        V(gr1_fr[[village_i]])[v]$indigenous.binary <- NA
        V(gr1_fr[[village_i]])[v]$b0600 <- NA
        V(gr1_fr[[village_i]])[v]$d0700 <- NA
      }
    }
    
    # Now, compute neighbor-based summary statistics for each vertex in the village graph
    for (v in seq_along(V(gr1_fr[[village_i]]))) {
      v_vertex <- V(gr1_fr[[village_i]])[v]
      node_gender <- V(gr1_fr[[village_i]])[v]$gender
      # Get the vertex's neighbors
      neigh_seq <- neighbors(gr1_fr[[village_i]], v_vertex, mode = "all")
      # Include the vertex itself along with its neighbors
      neigh_seq_incl_self <- c(v_vertex ,neigh_seq)
      # Not include the vertex itself along with its neighbors
      neigh_seq_not_incl_self <- c(neigh_seq)
      # Get the gender values of these nodes
      neigh_genders <- V(gr1_fr[[village_i]])[neigh_seq_not_incl_self]$gender
      # Identify neighbors that share the same gender as the vertex
      neighbors_same_gender <- neigh_seq_not_incl_self[which(!is.na(neigh_genders) & neigh_genders == node_gender)]
      
      neighbors_same_gender <- neigh_seq_not_incl_self
      
      # Compute the degree (number of connections) for the vertex
      node_degree <- degree(gr1_fr[[village_i]], v, mode = "all")
      node_degree[is.na(node_degree)] <- 0
      
      if (length(neighbors_same_gender) > 0) {
        # Get neighbor values for several attributes
        neigh_wealth_values <- V(gr1_fr[[village_i]])[neighbors_same_gender]$Wealth1
        neigh_fi_values <- V(gr1_fr[[village_i]])[neighbors_same_gender]$FI
        neigh_marital_values <- V(gr1_fr[[village_i]])[neighbors_same_gender]$marital_status
        neigh_age_values <- as.numeric(V(gr1_fr[[village_i]])[neighbors_same_gender]$age)
        neigh_b0100_values <- V(gr1_fr[[village_i]])[neighbors_same_gender]$b0100
        neigh_indigenous.binary_values <- V(gr1_fr[[village_i]])[neighbors_same_gender]$indigenous.binary
        neigh_b0600_values <- V(gr1_fr[[village_i]])[neighbors_same_gender]$b0600
        neigh_d0700_values <- V(gr1_fr[[village_i]])[neighbors_same_gender]$d0700
        # Calculate the mean for each attribute among the neighbors (ignoring NA values)
        mean_wealth_neigh <- mean(neigh_wealth_values, na.rm = TRUE)
        mean_fi_neigh <- mean(neigh_fi_values, na.rm = TRUE)
        mean_marital_neigh <- mean(neigh_marital_values, na.rm = TRUE)
        mean_age_neigh <- mean(neigh_age_values, na.rm = TRUE)
        mean_b0100_neigh <- mean(neigh_b0100_values, na.rm = TRUE)
        mean_indigenous.binary_neigh <- mean(neigh_indigenous.binary_values, na.rm = TRUE)
        mean_b0600_neigh <- mean(neigh_b0600_values, na.rm = TRUE)
        mean_d0700_neigh <- mean(neigh_d0700_values, na.rm = TRUE)
        # Also compute the mean degree of the same-gender neighbors
        neighbor_degrees <- degree(gr1_fr[[village_i]], neighbors_same_gender, mode = "all")
        neighbor_degrees[is.na(neighbor_degrees)] <- 0
        mean_neighbor_degree <- mean(neighbor_degrees, na.rm = TRUE)

        # Retrieve the node's own attribute values
        node_wealth <- V(gr1_fr[[village_i]])[v]$Wealth1
        node_fi <- V(gr1_fr[[village_i]])[v]$FI
        node_age <- as.numeric(V(gr1_fr[[village_i]])[v]$age)
        node_marital <- V(gr1_fr[[village_i]])[v]$marital_status
        node_b0100 <- V(gr1_fr[[village_i]])[v]$b0100
        node_indigenous.binary <- V(gr1_fr[[village_i]])[v]$indigenous.binary
        node_b0600 <- V(gr1_fr[[village_i]])[v]$b0600
        node_d0700 <- V(gr1_fr[[village_i]])[v]$d0700

        # Compute the difference between the node's own value and the mean of its neighbors
        V(gr1_fr[[village_i]])[v]$avg_neighbors_degree <- mean_neighbor_degree
        
      } else {

        # If there are no same-gender neighbors, assign NA to all computed neighbor-based attributes

        V(gr1_fr[[village_i]])[v]$avg_neighbors_degree <- 0

      }
    }
  }
}

# ---------------------------------------------------------------------------------------
# Add attributes and compute statistics for Wave 3 graphs
# ---------------------------------------------------------------------------------------
# Loop over all village IDs in villages_1_3 excluding village 156.
for (village_i in setdiff(villages_1_3, c(155,156))) {
  # Proceed only if the graph for this village in the Wave 3 friend relationships is not NULL.
  if (!is.null(gr3_fr[[village_i]])) {
    # Ensure all vertex names in the current Wave 3 graph are character strings.
    V(gr3_fr[[village_i]])$name <- as.character(V(gr3_fr[[village_i]])$name)
    
    # Subset the individual-level data for Wave 3 to only include those respondents
    # that are present in the current village graph. Then, select the attributes we need.
    node_data_3 <- individuals_wave3 %>%
      dplyr::filter(Respondent3 %in% V(gr3_fr[[village_i]])$name) %>%
      dplyr::select(Respondent3, Wealth3, FI, marital_status, age, gender, b0100, indigenous.binary, b0600, d0700)
    
    # Loop over each vertex (node) in the current village graph.
    for (v in seq_along(V(gr3_fr[[village_i]]))) {
      # Get the name of the current vertex.
      vertex_name <- V(gr3_fr[[village_i]])[v]$name
      
      # If the vertex's name is found in the individual data, assign the attributes.
      if (vertex_name %in% node_data_3$Respondent3) {
        V(gr3_fr[[village_i]])[v]$Wealth3         <- node_data_3$Wealth3[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$FI              <- node_data_3$FI[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$marital_status  <- node_data_3$marital_status[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$age             <- node_data_3$age[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$gender          <- node_data_3$gender[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$b0100           <- node_data_3$b0100[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$indigenous.binary <- node_data_3$indigenous.binary[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$b0600           <- node_data_3$b0600[node_data_3$Respondent3 == vertex_name]
        V(gr3_fr[[village_i]])[v]$d0700           <- node_data_3$d0700[node_data_3$Respondent3 == vertex_name]
      } else {
        # If the vertex's name is not found in the individual data,
        # assign NA to all attributes.
        V(gr3_fr[[village_i]])[v]$Wealth3         <- NA
        V(gr3_fr[[village_i]])[v]$FI              <- NA
        V(gr3_fr[[village_i]])[v]$marital_status  <- NA
        V(gr3_fr[[village_i]])[v]$age             <- NA
        V(gr3_fr[[village_i]])[v]$gender          <- NA
        V(gr3_fr[[village_i]])[v]$b0100           <- NA
        V(gr3_fr[[village_i]])[v]$indigenous.binary <- NA
        V(gr3_fr[[village_i]])[v]$b0600           <- NA
        V(gr3_fr[[village_i]])[v]$d0700           <- NA
      }
    }
    
    # Next, for each vertex, compute neighbor-based summary statistics.
    for (v in seq_along(V(gr3_fr[[village_i]]))) {
      # Get the current vertex.
      v_vertex <- V(gr3_fr[[village_i]])[v]
      # Retrieve the vertex's gender.
      node_gender <- V(gr3_fr[[village_i]])[v]$gender
      # Get the set of neighbors (all connected nodes) for the current vertex.
      neigh_seq <- neighbors(gr3_fr[[village_i]], v_vertex, mode = "all")
      # Include the vertex itself along with its neighbors
      neigh_seq_incl_self <- c(v_vertex ,neigh_seq)
      # Not include the vertex itself along with its neighbors
      neigh_seq_not_incl_self <- c(neigh_seq)
      # Extract the gender values for the vertex and its neighbors.
      neigh_genders <- V(gr3_fr[[village_i]])[neigh_seq_not_incl_self]$gender
      # Identify the neighbors that share the same gender as the current vertex.
      neighbors_same_gender <- neigh_seq_not_incl_self[which(!is.na(neigh_genders) & neigh_genders == node_gender)]
      
      neighbors_same_gender <- neigh_seq_not_incl_self
      
      # Compute the degree (i.e., number of connections) for the vertex.
      node_degree <- degree(gr3_fr[[village_i]], v, mode = "all")
      # Replace any NA values in degree with 0.
      node_degree[is.na(node_degree)] <- 0
      
      # If there are same-gender neighbors, compute various neighbor-level statistics.
      if (length(neighbors_same_gender) > 0) {
        # Get the neighbor values for each attribute.
        neigh_wealth_values          <- V(gr3_fr[[village_i]])[neighbors_same_gender]$Wealth3
        neigh_fi_values              <- V(gr3_fr[[village_i]])[neighbors_same_gender]$FI
        neigh_marital_values         <- V(gr3_fr[[village_i]])[neighbors_same_gender]$marital_status
        neigh_age_values             <- as.numeric(V(gr3_fr[[village_i]])[neighbors_same_gender]$age)
        neigh_b0100_values           <- V(gr3_fr[[village_i]])[neighbors_same_gender]$b0100
        neigh_indigenous.binary_values <- V(gr3_fr[[village_i]])[neighbors_same_gender]$indigenous.binary
        neigh_b0600_values           <- V(gr3_fr[[village_i]])[neighbors_same_gender]$b0600
        neigh_d0700_values           <- V(gr3_fr[[village_i]])[neighbors_same_gender]$d0700
        
        # Calculate the mean of each neighbor attribute, excluding missing values.
        mean_wealth_neigh    <- mean(neigh_wealth_values, na.rm = TRUE)
        mean_fi_neigh        <- mean(neigh_fi_values, na.rm = TRUE)
        mean_marital_neigh   <- mean(neigh_marital_values, na.rm = TRUE)
        mean_age_neigh       <- mean(neigh_age_values, na.rm = TRUE)
        mean_b0100_neigh     <- mean(neigh_b0100_values, na.rm = TRUE)
        mean_indigenous.binary_neigh <- mean(neigh_indigenous.binary_values, na.rm = TRUE)
        mean_b0600_neigh     <- mean(neigh_b0600_values, na.rm = TRUE)
        mean_d0700_neigh     <- mean(neigh_d0700_values, na.rm = TRUE)
        
        # Calculate the degrees (number of connections) for the same-gender neighbors.
        neighbor_degrees <- degree(gr3_fr[[village_i]], neighbors_same_gender, mode = "all")
        neighbor_degrees[is.na(neighbor_degrees)] <- 0
        # Compute the mean neighbor degree.
        mean_neighbor_degree <- mean(neighbor_degrees, na.rm = TRUE)
        
        
        # Compute the differences between the vertex's own value and the mean value of its same-gender neighbors.
        # (These differences can be interpreted as the deviation of the vertex from its neighborhood average.)
 
        V(gr3_fr[[village_i]])[v]$avg_neighbors_degree <- mean_neighbor_degree

        
        
      } else {
        # If there are no same-gender neighbors, assign NA to the neighbor-based summary attributes.

        V(gr3_fr[[village_i]])[v]$avg_neighbors_degree <- 0
      }
    }
  }
}

# ---------------------------------------------------------------------------------------
# Extract vertex attributes into data frames for Wave 1 and Wave 3
# ---------------------------------------------------------------------------------------
# Initialize an empty object to store the vertex attributes data frame for Wave 1 graphs.
dataframe_attributes_fr_1 <- {}

# Loop over all village IDs in villages_1_3.
for (village_i in villages_1_3) {
  # Skip village 156.
  #if (village_i == 156) next
  # Get the Wave 1 graph for the current village.
  g <- gr1_fr[[village_i]]
  
  # Extract all vertex attributes from the graph.
  vertex_attributes <- vertex_attr(g)
  # Convert the vertex attributes list to a data frame.
  vertex_attributes_df <- as.data.frame(vertex_attributes)
  # Save the vertex names (which are the rownames) into a new column.
  vertex_attributes_df$vertex_names <- rownames(vertex_attributes_df)
  
  # Compute overall network-level metrics:
  network_size <- vcount(g)
  network_density <- edge_density(g)
  number_of_edges <- ecount(g)
  avg_shortest_path <- mean_distance(g, directed = FALSE)
  
  # Append the network-level metrics to every row in the vertex attributes data frame.
  vertex_attributes_df$network_size <- network_size
  vertex_attributes_df$network_density <- network_density
  vertex_attributes_df$number_of_edges <- number_of_edges
  vertex_attributes_df$avg_shortest_path <- avg_shortest_path
  
  # Combine this village's vertex attributes with the overall data frame.
  dataframe_attributes_fr_1 <- rbind(dataframe_attributes_fr_1, vertex_attributes_df)
}

# Repeat a similar process for Wave 3 graphs.
dataframe_attributes_fr_3 <- {}

for (village_i in villages_1_3) {
  if (village_i == 155 | village_i == 156) next
  # Get the Wave 3 graph for the current village.
  g <- gr3_fr[[village_i]]
  
  # Extract vertex attributes and convert to a data frame.
  vertex_attributes <- vertex_attr(g)
  vertex_attributes_df <- as.data.frame(vertex_attributes)
  vertex_attributes_df$vertex_names <- rownames(vertex_attributes_df)
  
  # Compute network-level metrics.
  network_size <- vcount(g)
  network_density <- edge_density(g)
  number_of_edges <- ecount(g)
  avg_shortest_path <- mean_distance(g, directed = FALSE)
  
  # Append the metrics to the data frame.
  vertex_attributes_df$network_size <- network_size
  vertex_attributes_df$network_density <- network_density
  vertex_attributes_df$number_of_edges <- number_of_edges
  vertex_attributes_df$avg_shortest_path <- avg_shortest_path
  
  # Combine the current village data with the overall Wave 3 data frame.
  dataframe_attributes_fr_3 <- rbind(dataframe_attributes_fr_3, vertex_attributes_df)
}

# End of script

# Graph 4

### Load respondents (individuals) from wave 4 and keep certain variables

# Read individuals from wave 4 (initially 25,598 rows)
individuals_wave4 <- read.csv("individuals_wave4.csv") 

# Exclude individuals that:
#   - Have completed the census
#   - Have completed the questionnaire 
#   - Have a non-missing status (i.e., moved, new, etc.)
#   - And belong to the correct data source (data_source_w4 == 1)
# (Resulting in 8,216 respondents)
individuals_wave4 <- individuals_wave4[!is.na(individuals_wave4$complete) & 
                                         (individuals_wave4$complete == 1) &
                                         is.na(individuals_wave4$status_w4) & 
                                         # (individuals_wave4$new_respondent_w4 == 0) &    # Commented out
                                         # (individuals_wave4$building_id_w4 == individuals_wave4$building_id_w3) &    # Commented out
                                         (individuals_wave4$data_source_w4 == 1), ]

# Keep only the selected variables by combining columns:
# respondent_master_id, building_id_w4, building_id_w3, gender, marital_name, age_at_survey, and village_code_w4
individuals_wave4 <- cbind(individuals_wave4$respondent_master_id, 
                           individuals_wave4$building_id_w4, 
                           individuals_wave4$building_id_w3, 
                           individuals_wave4$gender, 
                           individuals_wave4$marital_name, 
                           individuals_wave4$age_at_survey, 
                           individuals_wave4$village_code_w4)

# Rename the columns for clarity
colnames(individuals_wave4) <- c('Respondent4', 'Household4', 'Household3', 'gender', 'marital_status', 'age', 'village')

# Convert the result into a data frame and remove any rows with NA values
individuals_wave4 <- na.omit(as.data.frame(individuals_wave4))


### Load network connection data for wave 4

# Read connections from wave 4
connections_individuals4_fr <- read.csv("health_wave4.csv")

# Filter connections to include only specific relationship types and ensure alter_source equals 1
connections_individuals4_fr <- connections_individuals4_fr[
  (connections_individuals4_fr$relationship %in% c("closest_friend", "personal_private", "free_time")) & 
    (connections_individuals4_fr$alter_source == 1), ]

# Keep only the relevant columns: ego, alter, and village_code_w4, and rename them accordingly
connections_individuals4_fr <- cbind(
  connections_individuals4_fr$ego, 
  connections_individuals4_fr$alter, 
  connections_individuals4_fr$village_code_w4
)
colnames(connections_individuals4_fr) <- c('Respondent4', 'Alter', 'Village4')

# Assign the filtered connections to a new variable (resulting in 61,601 connections)
connections_individuals4 <- connections_individuals4_fr 

# Ensure individuals_wave4 has the desired columns (first 7) and rename them again for consistency
individuals_wave4 <- as.data.frame(individuals_wave4[, 1:7])
colnames(individuals_wave4) <- c('Respondent4', 'Household4', 'Household3', 'gender', 'marital_status', 'age', 'village')

# Merge the connections data with the individuals data by Respondent4 (ego side)
connections_individuals4 <- merge(connections_individuals4, individuals_wave4, by = 'Respondent4')

# Merge the result with individuals data again by Alter (to attach alter information)
connections_individuals4 <- merge(connections_individuals4, individuals_wave4, by.x = 'Alter', by.y = 'Respondent4')

# Remove any rows with NA values after merging
connections_individuals4 <- na.omit(as.data.frame(connections_individuals4))

# Extract unique village codes from the connections data (as numeric) – there should be 82 villages
villages_4 <- as.numeric(unique(connections_individuals4[, 3])) 

# ---------------------------------------------------------------------------------------
# Initialize lists for storing village-level connection data and graph edges
village_connection4 <- list()
edges4 <- list()

# Graph objects for wave 4 will be stored in the list 'gr4'
gr4_fr <- list()


####### WAVE 4

# Load depression data for wave 4 respondents (saved previously)
load("resp_paternal_depr_dfs_20240515.rda")

# Install and load the tidyverse package if not already installed
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# Encode gender for wave 4: convert "man" to 1, otherwise 0
individuals_wave4[,'gender'] <- ifelse(individuals_wave4[,'gender'] == "man", 1, 0)

# Encode marital status:
# First, convert different marital statuses to numeric codes
individuals_wave4[,'marital_status'] <- ifelse(individuals_wave4[,'marital_status'] == "Single", 1,
                                               ifelse(individuals_wave4[,'marital_status'] == "Widowed", 2,
                                                      ifelse(individuals_wave4[,'marital_status'] == "Separated", 3,
                                                             ifelse(individuals_wave4[,'marital_status'] == "Civil Union", 4,
                                                                    ifelse(individuals_wave4[,'marital_status'] == "Married", 5, 6)))))
# Then, recode to a binary variable: treat codes 4 and 5 as partnered (1) and others as unpartnered (0); assign NA for code 6
individuals_wave4$marital_status <- ifelse(individuals_wave4$marital_status == 6, NA,
                                           ifelse(individuals_wave4$marital_status %in% c(4,5), 1, 0))

# Load village characteristics for Honduras (from Wave 1 file)
village_characteristics <- read.csv("honduras_villages_WAVE1_v8.csv")
village_characteristics <- village_characteristics[, c('village_code', 'aldea_latitude', 'aldea_longitude', 
                                                       'time_to_main_road', 'health_center', 
                                                       'time_to_health_ctr', 'maternal_clinic', 'time_to_mat_clinic')]

# Ensure the village column is numeric and merge village-level characteristics into individuals_wave4
individuals_wave4[, 'village'] <- as.numeric(individuals_wave4[, 'village'])
individuals_wave4 <- merge(individuals_wave4, village_characteristics, by.x = 'village', by.y = 'village_code', all = FALSE)

# Merge built environment (access route) data with individuals_wave4
individuals_wave4 <- individuals_wave4 %>%
  left_join(data_extracted, by = c("village" = "village_code"))

# Load psychological characteristics (for wave 4)
psycho_characteristics <- resp_w4_parental_depr

# Merge psychological data with individuals_wave4 based on Respondent4
individuals_wave4 <- merge(individuals_wave4, psycho_characteristics, by.x = 'Respondent4', by.y = 'respondent_master_id', all = FALSE)

# Load education data
education <- read.csv("hondras_demog_w1_to_w4_marios_2024-02-13.csv")
education <- education %>% dplyr::select(respondent_master_id, b0100, b0200, b0600)

# Convert education level (b0100) from text to numeric values using an ifelse chain
education$b0100 <- ifelse(education$b0100 == "1st grade", 1,
                          ifelse(education$b0100 == "2nd grade", 2,
                                 ifelse(education$b0100 == "3rd grade", 3,
                                        ifelse(education$b0100 == "4th grade", 4,
                                               ifelse(education$b0100 == "5th grade", 5,
                                                      ifelse(education$b0100 == "6th grade", 6,
                                                             ifelse(education$b0100 == "Some secondary", 8,
                                                                    ifelse(education$b0100 == "Secondary", 11,
                                                                           ifelse(education$b0100 == "Have not completed any type of school", 0,
                                                                                  ifelse(education$b0100 == "More than secondary", 13, 0))))))))))

# Encode religion information using case_when: Catholic = 2, Protestant = 1, No Religion = 0, and others NA
education <- education %>% mutate(b0600 = case_when(is.na(b0600) ~ NA_real_,
                                                    b0600 == "Catholic" ~ 2,
                                                    b0600 == "Protestant" ~ 1,
                                                    b0600 == "No Religion" ~ 0, 
                                                    b0600 %in% c("Dont_Know","Refused","Mormon") ~ NA_real_,
                                                    TRUE ~ NA_real_))

# Merge education data with individuals_wave4 using Respondent4
individuals_wave4 <- merge(individuals_wave4, education, by.x = 'Respondent4', by.y = 'respondent_master_id', all = FALSE)

# Recode responses for depression items c0300 and c0400 to numeric values and create a binary depression variable (depressed_w4)
individuals_wave4 <- individuals_wave4 %>%
  mutate(c0300_num = dplyr::recode(c0300, 
                                   "Not at all" = 0,
                                   "Several days" = 1,
                                   "More than half the days" = 2,
                                   "Nearly every day" = 3,
                                   .default = NA_real_),
         c0400_num = dplyr::recode(c0400, 
                                   "Not at all" = 0,
                                   "Several days" = 1,
                                   "More than half the days" = 2,
                                   "Nearly every day" = 3,
                                   .default = NA_real_))
individuals_wave4 <- individuals_wave4 %>%
  mutate(total_score = c0300_num + c0400_num,
         depressed_w4 = ifelse(total_score >= 2, 1, 0)) %>%
  dplyr::select(-c0300, -c0400, -c0300_num, -c0400_num, -total_score)

# Load additional individual characteristic data for wave 4
indiv_char <- read.csv("honduras_respondents_WAVE4_v2_marios_2024-06-03.csv")
indiv_char <- indiv_char %>%
  dplyr::select(respondent_master_id, d0100, d0200, d0700, e0300, c1100, c1200, c1300, b0530)

# Merge these characteristics with individuals_wave4
individuals_wave4 <- individuals_wave4 %>%
  left_join(indiv_char, by = c("Respondent4" = "respondent_master_id"))

# Encode individual wealth (d0700) using a series of ifelse statements
individuals_wave4$d0700 <- ifelse(individuals_wave4$d0700 == 'There is enough to live on and save', 4,
                                  ifelse(individuals_wave4$d0700 == 'It is sufficient, without major difficulties', 3,
                                         ifelse(individuals_wave4$d0700 == 'It is not sufficient and there are difficulties', 2,
                                                ifelse(individuals_wave4$d0700 == 'It is not sufficient and there are major difficulties', 1, 0))))

# Encode food insecurity variables (d0100 and d0200) as binary indicators
individuals_wave4$d0100 <- ifelse(individuals_wave4$d0100 == 'Yes', 1, 0)
individuals_wave4$d0200 <- ifelse(individuals_wave4$d0200 == 'Yes', 1, 0)

# Create a Food Insecurity (FI) measure from the two binary indicators
individuals_wave4$FI <- ifelse(individuals_wave4$d0100 == 1 & individuals_wave4$d0200 == 1, 3,
                               ifelse(individuals_wave4$d0100 == 0 & individuals_wave4$d0200 == 0, 1, 2))
# Recode FI into a binary variable based on established thresholds (per Kumar et al.)
individuals_wave4 <- individuals_wave4 %>% mutate(FI = case_when(FI == 2 | FI == 3 ~ 1,
                                                                 FI == 1 ~ 0,
                                                                 TRUE ~ NA_real_))

# Encode indigenous status and cohabitation for wave 4
individuals_wave4 <- individuals_wave4 %>%
  mutate(indigenous.binary = case_when(
    b0200 %in% c("Yes, Lenca", "Yes, Maya Chorti", "Other") ~ 1,
    b0200 == "No" ~ 0,
    TRUE ~ NA_real_
  ),
  cohabitating = case_when(
    e0300 == "Yes" ~ "cohabitating",
    e0300 == "No" ~ "not cohabitating or single",
    TRUE ~ NA_character_
  ))

# Load psychological characteristics from a CSV file (for wave 4)
psycho_characteristics <- read.csv("honduras_respondents_WAVE4_v2_marios_2024-06-03.csv")
# Keep only the specified columns
psycho_characteristics <- psycho_characteristics %>%
  dplyr::select(respondent_master_id, phq2_score)
# Create binary variables based on thresholds for each psychological measure
psycho_characteristics <- psycho_characteristics %>%
  mutate(phq2_binary = ifelse(phq2_score >= 2, 1, 0)) %>%
  dplyr::select(respondent_master_id, phq2_binary)
# Merge the psychological characteristics with individuals_wave4 based on Respondent4
individuals_wave4 <- left_join(individuals_wave4, psycho_characteristics, by = c("Respondent4" = "respondent_master_id"))

# Load household wealth data for wave 4 and keep only the building_id and wealth index
households_wave4 <- read.csv("honduras_households_WAVE4_v1.csv")
households_wave4 <- households_wave4[, c("building_id", "household_wealth_index_w4")]
colnames(households_wave4) <- c('Household', 'Wealth4')
# Merge the household wealth information with individuals_wave4 based on Household4
individuals_wave4 <- individuals_wave4 %>%
  left_join(households_wave4, by = c("Household4" = "Household"))

# Save a copy of individuals_wave4 for graph-related processing
individuals_wave4_fr <- individuals_wave4




# Iterate through each village (by village code) from villages_4
for (i in villages_4) {
  # Subset the connections for the current village based on Village4
  village_connection4[[i]] <- connections_individuals4[connections_individuals4$Village4 == i, ]
  
  # Extract the edge list (columns: Respondent4 and Alter) for the current village
  edges4[[i]] <- village_connection4[[i]][, c('Respondent4', 'Alter')]
  
  # Create an undirected graph from the edge list for the current village
  gr4_fr[[i]] <- graph_from_edgelist(as.matrix(edges4[[i]]), directed = FALSE)
  
  # Get the list of individual respondent IDs (from individuals_wave4) belonging to village i
  individuals <- individuals_wave4[individuals_wave4$village == i, ]$Respondent4
  # Identify individuals who are not already included in the graph's nodes
  g2 <- unique(c(setdiff(individuals, V(gr4_fr[[i]])$name)))
  
  # If there are any missing nodes, add them to the graph
  if (length(g2) > 0) {
    gr4_fr[[i]] <- igraph::add_vertices(gr4_fr[[i]], length(g2), name = g2)
  }
  
  # Simplify the graph to remove multiple edges (if any)
  gr4_fr[[i]] <- igraph::simplify(gr4_fr[[i]], remove.multiple = TRUE)
}

# ---------------------------------------------------------------------------------------
# Assign node attributes (e.g., depression and psychological binary scores) to wave 4 graphs
# ---------------------------------------------------------------------------------------
# For each village in villages_4, perform the following:
for (i in villages_4) {
  
  # Ensure node names in the graph are character strings
  V(gr4_fr[[i]])$name <- as.character(V(gr4_fr[[i]])$name)
  
  # Ensure that individuals_wave4 has valid and unique column names
  names(individuals_wave4) <- make.names(names(individuals_wave4), unique = TRUE)
  
  # Replace any NA or empty column names with "UnnamedColumn"
  names(individuals_wave4)[which(is.na(names(individuals_wave4)))] <- "UnnamedColumn"
  
  # Filter the individuals_wave4 data for those that are present as nodes in the current graph
  # and select only the necessary columns: Respondent4, depressed_w4
  node_data_4 <- individuals_wave4 %>%
    dplyr::filter(Respondent4 %in% V(gr4_fr[[i]])$name) %>%
    dplyr::select(Respondent4, depressed_w4)
  
  # For each vertex in the current graph, if the vertex's name exists in node_data_4,
  # then set the graph node attributes (depressed_w4) accordingly.
  for (vertex_id in V(gr4_fr[[i]])) {
    vertex_name <- V(gr4_fr[[i]])[vertex_id]$name
    if (vertex_name %in% node_data_4$Respondent4) {
      V(gr4_fr[[i]])[vertex_id]$depressed_w4 <- node_data_4[node_data_4$Respondent4 == vertex_name,]$depressed_w4
    }
  }
}



# ---------------------------------------------------------------------------------------
# Compute intransitivity and neighbor-based depression attributes for Wave 4 graphs
# ---------------------------------------------------------------------------------------
for (i in villages_4) {
  # Calculate the intransitivity index for the current graph using the custom function
  intransitivity_values <- intransitivity_index(gr4_fr[[i]])
  
  # Loop over every vertex in the current graph
  for (v in V(gr4_fr[[i]])) {
    # Get the list of neighbors (all connected nodes) for the current vertex
    neighbors_v <- neighbors(gr4_fr[[i]], v, mode = "all")
    
    # For each vertex, count the number of neighbors with depressed_w4 == 1, or 0 if none exist
    if (length(neighbors_v) > 0) {
      V(gr4_fr[[i]])[v]$depressed_w4_neigh <- sum(V(gr4_fr[[i]])[neighbors_v]$depressed_w4 == 1)
    } else {
      V(gr4_fr[[i]])[v]$depressed_w4_neigh <- 0
    }
    
    # Similarly, count the number of neighbors with depressed_w4 == 0
    if (length(neighbors_v) > 0) {
      V(gr4_fr[[i]])[v]$not_depressed_w4_neigh <- sum(V(gr4_fr[[i]])[neighbors_v]$depressed_w4 == 0)
    } else {
      V(gr4_fr[[i]])[v]$not_depressed_w4_neigh <- 0
    }
    
    # Attach the computed intransitivity value to the current vertex
    V(gr4_fr[[i]])[v]$intransitivity_values <- intransitivity_values[v]
  }
}






# Now, assign additional individual attributes (e.g., Wealth4, FI, marital status, etc.)
# and compute neighbor-based summary statistics for each village's graph in wave 4.
for (village_i in setdiff(villages_4, c(155,156))) {
  if (!is.null(gr4_fr[[village_i]])) {
    # Ensure node names are character strings.
    V(gr4_fr[[village_i]])$name <- as.character(V(gr4_fr[[village_i]])$name)
    
    # Filter individuals_wave4 for those present in the current graph and select desired attributes.
    node_data_4 <- individuals_wave4 %>%
      dplyr::filter(Respondent4 %in% V(gr4_fr[[village_i]])$name) %>%
      dplyr::select(Respondent4, Wealth4, FI, marital_status, age, gender, b0100, indigenous.binary, b0600, d0700)
    
    # Loop over each vertex in the current graph.
    for (v in seq_along(V(gr4_fr[[village_i]]))) {
      vertex_name <- V(gr4_fr[[village_i]])[v]$name
      # If the vertex exists in node_data_4, assign the corresponding attributes.
      if (vertex_name %in% node_data_4$Respondent4) {
        V(gr4_fr[[village_i]])[v]$Wealth4        <- node_data_4$Wealth4[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$FI             <- node_data_4$FI[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$marital_status <- node_data_4$marital_status[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$age            <- node_data_4$age[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$gender         <- node_data_4$gender[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$b0100          <- node_data_4$b0100[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$indigenous.binary <- node_data_4$indigenous.binary[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$b0600          <- node_data_4$b0600[node_data_4$Respondent4 == vertex_name]
        V(gr4_fr[[village_i]])[v]$d0700          <- node_data_4$d0700[node_data_4$Respondent4 == vertex_name]
      } else {
        # Otherwise, assign NA to all the vertex attributes.
        V(gr4_fr[[village_i]])[v]$Wealth4 <- NA
        V(gr4_fr[[village_i]])[v]$FI <- NA
        V(gr4_fr[[village_i]])[v]$marital_status <- NA
        V(gr4_fr[[village_i]])[v]$age <- NA
        V(gr4_fr[[village_i]])[v]$gender <- NA
        V(gr4_fr[[village_i]])[v]$b0100 <- NA
        V(gr4_fr[[village_i]])[v]$indigenous.binary <- NA
        V(gr4_fr[[village_i]])[v]$b0600 <- NA
        V(gr4_fr[[village_i]])[v]$d0700 <- NA
      }
    }
    
    # Next, compute neighbor-based summary statistics for each vertex.
    for (v in seq_along(V(gr4_fr[[village_i]]))) {
      # Get the current vertex.
      v_vertex <- V(gr4_fr[[village_i]])[v]
      # Retrieve the gender of the vertex.
      node_gender <- V(gr4_fr[[village_i]])[v]$gender
      # Get the list of neighbors (all nodes connected to the vertex).
      neigh_seq <- neighbors(gr4_fr[[village_i]], v_vertex, mode = "all")
      # Include the vertex itself along with its neighbors
      neigh_seq_incl_self <- c(v_vertex ,neigh_seq)
      # Not include the vertex itself along with its neighbors
      neigh_seq_not_incl_self <- c(neigh_seq)
      # Extract the gender for each node in the combined list.
      neigh_genders <- V(gr4_fr[[village_i]])[neigh_seq_not_incl_self]$gender
      # Identify neighbors that share the same gender as the vertex.
      neighbors_same_gender <- neigh_seq_not_incl_self[which(!is.na(neigh_genders) & neigh_genders == node_gender)]
      
      neighbors_same_gender <- neigh_seq_not_incl_self
      
      # Compute the degree (number of connections) for the vertex.
      node_degree <- degree(gr4_fr[[village_i]], v, mode = "all")
      node_degree[is.na(node_degree)] <- 0
      
      if (length(neighbors_same_gender) > 0) {
        # Retrieve neighbor attribute values.
        neigh_wealth_values          <- V(gr4_fr[[village_i]])[neighbors_same_gender]$Wealth4
        neigh_fi_values              <- V(gr4_fr[[village_i]])[neighbors_same_gender]$FI
        neigh_marital_values         <- V(gr4_fr[[village_i]])[neighbors_same_gender]$marital_status
        neigh_age_values             <- as.numeric(V(gr4_fr[[village_i]])[neighbors_same_gender]$age)
        neigh_b0100_values           <- V(gr4_fr[[village_i]])[neighbors_same_gender]$b0100
        neigh_indigenous.binary_values <- V(gr4_fr[[village_i]])[neighbors_same_gender]$indigenous.binary
        neigh_b0600_values           <- V(gr4_fr[[village_i]])[neighbors_same_gender]$b0600
        neigh_d0700_values           <- V(gr4_fr[[village_i]])[neighbors_same_gender]$d0700
        
        # Calculate the mean of neighbor attributes (excluding NAs)
        mean_wealth_neigh    <- mean(neigh_wealth_values, na.rm = TRUE)
        mean_fi_neigh        <- mean(neigh_fi_values, na.rm = TRUE)
        mean_marital_neigh   <- mean(neigh_marital_values, na.rm = TRUE)
        mean_age_neigh       <- mean(neigh_age_values, na.rm = TRUE)
        mean_b0100_neigh     <- mean(neigh_b0100_values, na.rm = TRUE)
        mean_indigenous.binary_neigh <- mean(neigh_indigenous.binary_values, na.rm = TRUE)
        mean_b0600_neigh     <- mean(neigh_b0600_values, na.rm = TRUE)
        mean_d0700_neigh     <- mean(neigh_d0700_values, na.rm = TRUE)
        
        # Compute the degrees for the same-gender neighbors.
        neighbor_degrees <- degree(gr4_fr[[village_i]], neighbors_same_gender, mode = "all")
        neighbor_degrees[is.na(neighbor_degrees)] <- 0
        
        # Compute the mean degree of the neighbors.
        mean_neighbor_degree <- mean(neighbor_degrees, na.rm = TRUE)
        
        # Retrieve the current vertex's own attribute values.
        node_wealth         <- V(gr4_fr[[village_i]])[v]$Wealth4
        node_fi             <- V(gr4_fr[[village_i]])[v]$FI
        node_age            <- as.numeric(V(gr4_fr[[village_i]])[v]$age)
        node_marital        <- V(gr4_fr[[village_i]])[v]$marital_status
        node_b0100          <- V(gr4_fr[[village_i]])[v]$b0100
        node_indigenous.binary <- V(gr4_fr[[village_i]])[v]$indigenous.binary
        node_b0600          <- V(gr4_fr[[village_i]])[v]$b0600
        node_d0700          <- V(gr4_fr[[village_i]])[v]$d0700
        
        # Compute the difference between the vertex's attribute and the mean of its neighbors.
        V(gr4_fr[[village_i]])[v]$avg_neighbors_degree <- mean_neighbor_degree
        
      } else {
        # If there are no same-gender neighbors, assign NA to all neighbor-based attributes.
        V(gr4_fr[[village_i]])[v]$avg_neighbors_degree <- 0
        
      }
    }
  }
}

# ---------------------------------------------------------------------------------------
# Extract vertex attributes into data frames for Wave 4 graphs
# ---------------------------------------------------------------------------------------
# Initialize an empty object to store the vertex attributes data frame for Wave 4 graphs.
dataframe_attributes_fr_4 <- {}

# Loop over each village in villages_4.
for (i in villages_4) {
  
  # Skip village 156.
  if (i == 155 | i == 156) next
  
  # Get the wave 4 graph for the current village.
  g <- gr4_fr[[i]]
  
  # Extract all vertex attributes from the graph and convert to a data frame.
  vertex_attributes <- vertex_attr(g)
  vertex_attributes_df <- as.data.frame(vertex_attributes)
  # Save the vertex names (row names) into a new column.
  vertex_attributes_df$vertex_names <- rownames(vertex_attributes_df)
  
  # Compute overall network-level metrics for the graph.
  network_size <- vcount(g)
  network_density <- edge_density(g)
  number_of_edges <- ecount(g)
  avg_shortest_path <- mean_distance(g, directed = FALSE)
  
  # Append these network-level metrics to each row of the vertex attributes data frame.
  vertex_attributes_df$network_size <- network_size
  vertex_attributes_df$network_density <- network_density
  vertex_attributes_df$number_of_edges <- number_of_edges
  vertex_attributes_df$avg_shortest_path <- avg_shortest_path
  
  # Combine this village's vertex attributes with the overall Wave 4 data frame.
  dataframe_attributes_fr_4 <- rbind(dataframe_attributes_fr_4, vertex_attributes_df)
}
