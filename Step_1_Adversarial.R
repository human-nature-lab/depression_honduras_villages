################################################################################
# This script performs the following steps:
# 1. Load dosage information, filter it, and extract dosage assignments per village.
# 2. Load individual-level data for Wave 1, Wave 3 and Wave 4(respondents), keeping certain variables.
# 3. Load and filter adversarial ("not_get_along") relationships from Wave 1, Wave 3 and Wave 4 connections.
# 4. Merge these connections with individual attributes.
# 5. Build igraph objects for each village and add missing nodes.
# 6. Compute some basic network measures for each graph.
################################################################################

# Load necessary libraries
library(igraph)  # For creating and analyzing network graphs
library(dplyr)   # For data manipulation tasks

################################################################################
# Step 1: Load and process dosage data
################################################################################

# Read the CSV file that contains dosage outcomes. This data includes village codes,
# friend treatment assignment, and dosage levels.
dosage_table <- read.csv("outcomes1.csv")

# Remove duplicate rows and keep only the unique combinations of 'village_code_w1', 
# 'friend_treatment', and 'dosage'
dosage_table <- unique(dosage_table[, c('village_code_w1', 'friend_treatment', 'dosage')])

# Initialize an empty list to hold dosage values for each village (indexed by village number)
# Note: We explicitly exclude index 156 as per instructions later in the code.
dosage <- list()
for (i in c(1:176)) {
  # For each village index (except 156), extract the dosage from dosage_table.
  # Here we assume the row index corresponds to the village code; adjust if needed.
  dosage[[i]] <- dosage_table[i, ]$dosage
}

# Initialize a nested list structure 'arm_index' to group villages by treatment
# arm_index[[1]] corresponds to treatment = 0 and arm_index[[2]] corresponds to treatment = 1.
arm_index <- list(list(), list())

# Define all possible dosage levels for the study
all_dosages <- c(0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.75, 1)

# Populate arm_index: For each treatment (0 or 1) and each dosage level,
# find all village codes (village_code_w1) that match the treatment and dosage.
for (j in 1:2) {
  for (i in 1:8) {
    arm_index[[j]][[i]] <- dosage_table[
      dosage_table$friend_treatment == (j - 1) &  # Filter based on treatment (0 for j=1, 1 for j=2)
        dosage_table$dosage == all_dosages[i],      # Filter based on dosage level
    ]$village_code_w1  # Extract the corresponding village codes
  }
}

################################################################################
# Step 2: Load individual-level data for Wave 1 and Wave 3
################################################################################

# --------------------- Wave 1 ---------------------
# Load the individual-level data for Wave 1 from a CSV file
individuals_wave1 <- read.csv("individuals_wave1.csv")

# Filter the Wave 1 data: keep only rows where the survey was completed (complete == 1)
# and where the data source indicator equals 1.
individuals_wave1 <- individuals_wave1[
  !is.na(individuals_wave1$complete) & 
    individuals_wave1$complete == 1 & 
    individuals_wave1$data_source_w1 == 1, 
]

# Select only the columns of interest:
# respondent_master_id, resp_target, building_id_w1, gender, marital_name, age_at_survey, village_code_w1.
individuals_wave1 <- cbind(
  individuals_wave1$respondent_master_id,
  individuals_wave1$resp_target,
  individuals_wave1$building_id_w1,
  individuals_wave1$gender,
  individuals_wave1$marital_name,
  individuals_wave1$age_at_survey,
  individuals_wave1$village_code_w1
)
# Convert to a data frame and remove any rows containing NA values.
individuals_wave1 <- as.data.frame(individuals_wave1)
individuals_wave1 <- na.omit(individuals_wave1)

# Rename the columns for clarity. The new names help identify the role of each variable.
colnames(individuals_wave1) <- c('Respondent1', 'Treatment', 'Household1', 'gender1', 
                                 'marital_status1', 'age1', 'village1')

# --------------------- Wave 3 ---------------------
# Load the individual-level data for Wave 3
individuals_wave3 <- read.csv("individuals_wave3.csv")

# Filter the Wave 3 data similarly: only complete responses (complete == 1)
# and where the data_source_w3 equals 1.
individuals_wave3 <- individuals_wave3[
  !is.na(individuals_wave3$complete) & 
    individuals_wave3$complete == 1 & 
    individuals_wave3$data_source_w3 == 1, 
]

# Select the specific columns needed for Wave 3.
individuals_wave3 <- cbind(
  individuals_wave3$respondent_master_id,
  individuals_wave3$resp_target,
  individuals_wave3$building_id_w3,
  individuals_wave3$gender,
  individuals_wave3$marital_name,
  individuals_wave3$age_at_survey,
  individuals_wave3$village_code_w3
)
# Convert the data to a data frame and remove rows with missing values.
individuals_wave3 <- as.data.frame(individuals_wave3)
individuals_wave3 <- na.omit(individuals_wave3)

# Rename columns to make the variable names consistent and clear.
colnames(individuals_wave3) <- c('Respondent3', 'Treatment', 'Household3', 'gender', 
                                 'marital_status', 'age', 'village')

################################################################################
# Step 3: Load and process adversarial ("not_get_along") relationships
################################################################################

# Load adversarial connection data for Wave 1 and Wave 3 from CSV files.
# A third file for Wave 4 is also loaded but not used further in this section.
connections_individuals1_adv <- read.csv("health_wave1.csv")
connections_individuals3_adv <- read.csv("health_wave3.csv")
connections_individuals4_adv <- read.csv("health_wave4.csv")  # Not used further here

# Filter the Wave 1 connections: Keep only rows where the relationship type is "not_get_along"
# and where the 'alter_source' indicator equals 1.
connections_individuals1_adv <- connections_individuals1_adv[
  (connections_individuals1_adv$relationship == "not_get_along") & 
    (connections_individuals1_adv$alter_source == 1), 
]

# Filter the Wave 3 connections similarly.
connections_individuals3_adv <- connections_individuals3_adv[
  (connections_individuals3_adv$relationship == "not_get_along") & 
    (connections_individuals3_adv$alter_source == 1), 
]

# For Wave 1, select the key columns: ego (the respondent), alter (the connected individual),
# and the village code. Then rename the columns appropriately.
connections_individuals1_adv <- cbind(
  connections_individuals1_adv$ego, 
  connections_individuals1_adv$alter, 
  connections_individuals1_adv$village_code_w1
)
colnames(connections_individuals1_adv) <- c('Respondent1', 'Alter', 'Village1')

# For Wave 3, do the same but using the appropriate village code variable.
connections_individuals3_adv <- cbind(
  connections_individuals3_adv$ego,
  connections_individuals3_adv$alter,
  connections_individuals3_adv$village_code_w3
)
colnames(connections_individuals3_adv) <- c('Respondent3', 'Alter', 'Village3')

# Save the filtered data in general connection variables for later use.
connections_individuals1 <- connections_individuals1_adv
connections_individuals3 <- connections_individuals3_adv

################################################################################
# Step 4: Merge connections with individual attributes
################################################################################

# First, reduce the individuals data (for both waves) to the first 7 columns only.
# This ensures consistency for merging.
individuals_wave1 <- as.data.frame(individuals_wave1[, 1:7])
colnames(individuals_wave1) <- c('Respondent1', 'Treatment', 'Household1', 'gender', 
                                 'marital_status', 'age', 'village')

individuals_wave3 <- as.data.frame(individuals_wave3[, 1:7])
colnames(individuals_wave3) <- c('Respondent3', 'Treatment', 'Household3', 'gender', 
                                 'marital_status', 'age', 'village')

# Merge the adversarial connection data for Wave 1 with the individual-level data.
# First, merge on Respondent1 to attach attributes for the ego.
connections_individuals1 <- merge(connections_individuals1, individuals_wave1, 
                                  by.x = 'Respondent1', by.y = 'Respondent1')
# Next, merge on Alter to attach the attributes for the alter.
connections_individuals1 <- merge(connections_individuals1, individuals_wave1, 
                                  by.x = 'Alter', by.y = 'Respondent1')
# Remove any rows with missing data after merging.
connections_individuals1 <- na.omit(as.data.frame(connections_individuals1))

# Repeat the same merging process for Wave 3 connections.
connections_individuals3 <- merge(connections_individuals3, individuals_wave3, 
                                  by.x = 'Respondent3', by.y = 'Respondent3')
connections_individuals3 <- merge(connections_individuals3, individuals_wave3, 
                                  by.x = 'Alter', by.y = 'Respondent3')
connections_individuals3 <- na.omit(as.data.frame(connections_individuals3))

################################################################################
# Step 5: Build igraph objects per village and add missing vertices
################################################################################

# Extract unique village codes from the first 3rd column (Village1) of the merged Wave 1 connections.
villages_1_3 <- as.numeric(unique(connections_individuals1[, 3]))

# Initialize empty lists to store connection data and edges for each village.
village_connection1 <- list()
village_connection3 <- list()
edges1 <- list()
edges3 <- list()

# Initialize lists to hold the igraph objects (the network graphs) for Wave 1 and Wave 3.
gr1_adv <- list()
gr3_adv <- list()

# Loop through each unique village code.
for (i in villages_1_3) {
  
  #if (i == 156) next  # Skip village 156 as instructed
  
  # Subset the connections for Wave 3 and Wave 1 based on the current village code.
  village_connection1[[i]] <- connections_individuals1[connections_individuals1$Village1 == i, ]
  
  # Extract the edge lists for the current village: a two-column matrix of connections.

  edges1[[i]] <- village_connection1[[i]][, c('Respondent1', 'Alter')]
  
  # Create undirected graphs from the edge lists using igraph.
  gr1_adv[[i]] <- graph_from_edgelist(as.matrix(edges1[[i]]), directed = FALSE)
  
  # Add missing nodes: For Wave 1, find respondents in individuals_wave1 for village i
  # who are not present in the graph and add them as isolated vertices.
  individuals_i_wave1 <- individuals_wave1[individuals_wave1$village == i, ]$Respondent1
  missing_in_gr1_adv <- unique(setdiff(individuals_i_wave1, V(gr1_adv[[i]])$name))
  if (length(missing_in_gr1_adv) > 0) {
    gr1_adv[[i]] <- add_vertices(gr1_adv[[i]], length(missing_in_gr1_adv), name = missing_in_gr1_adv)
  }
  
  
  
  # Simplify the graphs by removing multiple edges between the same nodes.
  gr1_adv[[i]] <- igraph::simplify(gr1_adv[[i]], remove.multiple = TRUE)

  if(i!=155 | i!=156){
    village_connection3[[i]] <- connections_individuals3[connections_individuals3$Village3 == i, ]
    edges3[[i]] <- village_connection3[[i]][, c('Respondent3', 'Alter')]
    gr3_adv[[i]] <- graph_from_edgelist(as.matrix(edges3[[i]]), directed = FALSE)

    # Do the same for Wave 3.
    individuals_i_wave3 <- individuals_wave3[individuals_wave3$village == i, ]$Respondent3
    missing_in_gr3_adv <- unique(setdiff(individuals_i_wave3, V(gr3_adv[[i]])$name))
    if (length(missing_in_gr3_adv) > 0) {
      gr3_adv[[i]] <- add_vertices(gr3_adv[[i]], length(missing_in_gr3_adv), name = missing_in_gr3_adv)
    }
    gr3_adv[[i]] <- igraph::simplify(gr3_adv[[i]], remove.multiple = TRUE)
  }
  
  # Additional commented-out code is present for potential future expansion.
}

################################################################################
# Step 6: Identify common respondents in Wave 1 and Wave 3 to refine connections
################################################################################

# Prepare Wave 1 and Wave 3 datasets for merging by renaming the respondent columns.
individuals_wave1_1 <- individuals_wave1 %>%
  rename(Respondent = Respondent1, Village1 = village)

individuals_wave3_3 <- individuals_wave3 %>%
  rename(Respondent = Respondent3, Village3 = village)

# Merge the two waves on Respondent to find individuals present in both waves.
merged_df <- merge(individuals_wave1_1, individuals_wave3_3, by = "Respondent")

# Filter the merged data to include only those respondents whose village codes match across waves.
common_elements <- merged_df %>%
  filter(Village1 == Village3) %>%
  dplyr::select(Respondent)

# Use the common respondents list to filter the adversarial connections.
connections_individuals1 <- connections_individuals1 %>%
  filter(Alter %in% common_elements$Respondent & Respondent1 %in% common_elements$Respondent)

connections_individuals3 <- connections_individuals3 %>%
  filter(Alter %in% common_elements$Respondent & Respondent3 %in% common_elements$Respondent)

################################################################################
# Step 7: Compute network metrics like degrees and average neighbor degrees
################################################################################

# Create an empty list to store network metric data frames for each village for Wave 1.
network_data_list <- list()
all_networks_df_1 <- list()

# Loop through each village’s graph in Wave 1 adversarial networks.
for (i in villages_1_3) {
  
  #if (i == 156) next  # Skip village 156 as required
  
  g <- gr1_adv[[i]]
  # If the graph is NULL or has no nodes, skip further calculations.
  if (is.null(g) || vcount(g) == 0) next
  
  # Calculate the degree (number of adversarial connections) for each node in the graph.
  node_degree <- degree(g)
  
  # Initialize a data frame to hold node-level network metrics.
  network_df <- data.frame(
    Node = names(node_degree),
    Adversaries = node_degree,
    stringsAsFactors = FALSE
  )
  
  # Create a new column to hold the average degree of each node's neighbors.
  network_df$Avg_Neighbor_Degree <- numeric(length(node_degree))
  
  # For each node, calculate the average degree of its neighbors.
  for (j in 1:vcount(g)) {
    neighbors_ids <- neighbors(g, j)
    if (length(neighbors_ids) > 0) {
      network_df$Avg_Neighbor_Degree[j] <- mean(node_degree[neighbors_ids])
    } else {
      network_df$Avg_Neighbor_Degree[j] <- 0
    }
  }
  
  # Add the network-level metric: adversarial edge density.
  network_df$Adv_density <- edge_density(g)
  
  # Store this village’s network data frame in the list.
  network_data_list[[i]] <- network_df
}

# Combine all the individual village data frames into one data frame for Wave 1.
all_networks_df_1 <- do.call(rbind, lapply(seq_along(network_data_list), function(i) {
  if (!is.null(network_data_list[[i]])) {
    cbind(Network = i, network_data_list[[i]])
  }
}))

# Repeat the process for Wave 3 adversarial networks.
network_data_list <- list()  # Reset list for Wave 3
all_networks_df_3 <- list()    # Initialize list for combined Wave 3 data

for (i in villages_1_3) {
  if (i==155 | i == 156) next  # Skip village 156
  g <- gr3_adv[[i]]
  if (is.null(g) || vcount(g) == 0) next
  
  # Calculate node degrees for the Wave 3 graph.
  node_degree <- degree(g)
  
  # Create a data frame with node metrics.
  network_df <- data.frame(
    Node = names(node_degree),
    Adversaries = node_degree,
    stringsAsFactors = FALSE
  )
  
  # Initialize columns for average neighbor degrees. In undirected graphs, in-degree
  # equals overall degree.
  network_df$Avg_Neighbor_In_Degree <- numeric(length(node_degree))
  network_df$Avg_Neighbor_Degree <- numeric(length(node_degree))
  
  # Compute the average neighbor degree for each node.
  for (j in 1:vcount(g)) {
    neighbors_ids <- neighbors(g, j)
    if (length(neighbors_ids) > 0) {
      network_df$Avg_Neighbor_In_Degree[j] <- mean(node_degree[neighbors_ids])
      network_df$Avg_Neighbor_Degree[j] <- mean(node_degree[neighbors_ids])
    } else {
      network_df$Avg_Neighbor_In_Degree[j] <- 0
      network_df$Avg_Neighbor_Degree[j] <- 0
    }
  }
  
  # Add the network edge density for the current graph.
  network_df$Adv_density <- edge_density(g)
  
  # Save the current village's network data frame.
  network_data_list[[i]] <- network_df
}

# Combine the Wave 3 data frames into a single data frame.
all_networks_df_3 <- do.call(rbind, lapply(seq_along(network_data_list), function(i) {
  if (!is.null(network_data_list[[i]])) {
    cbind(Network = i, network_data_list[[i]])
  }
}))

################################################################################
# Graph 4
################################################################################

# --------------------- Load and process Wave 4 individual data ---------------------

# Read the individuals data for Wave 4 from a CSV file.
individuals_wave4 <- read.csv("individuals_wave4.csv")

# Filter the Wave 4 individuals:
# - Ensure the questionnaire was completed (complete == 1).
# - Exclude respondents based on various conditions (e.g., moved, new, etc.); here, we 
#   filter based on data_source_w4 == 1. Some filtering options are commented out.
individuals_wave4 <- individuals_wave4[!is.na(individuals_wave4$complete) & 
                                         (individuals_wave4$complete == 1) &
                                         (individuals_wave4$data_source_w4 == 1), ]

# Select specific variables from the dataset:
# respondent_master_id, building_id_w4, building_id_w3, gender, marital_name, age_at_survey, village_code_w4.
individuals_wave4 <- cbind(individuals_wave4$respondent_master_id, 
                           individuals_wave4$building_id_w4, 
                           individuals_wave4$building_id_w3, 
                           individuals_wave4$gender, 
                           individuals_wave4$marital_name, 
                           individuals_wave4$age_at_survey, 
                           individuals_wave4$village_code_w4)

# Rename the columns to standard names.
colnames(individuals_wave4) <- c('Respondent4', 'Household4', 'Household3', 'gender', 'marital_status', 'age', 'village')

# Convert to a data frame and remove rows with any missing values.
individuals_wave4 <- na.omit(as.data.frame(individuals_wave4))

################################################################################
# Calculate adversarial ties (not get along) for wave 3 and wave 4 connections
################################################################################

# --------------------- Load and filter Wave 4 adversarial connections ---------------------

# Load the Wave 4 connections data from the CSV file.
connections_individuals4_adv <- read.csv("health_wave4.csv")

# Filter the connections for adversarial relationships ("not_get_along") and where alter_source equals 1.
connections_individuals4_adv <- connections_individuals4_adv[
  (connections_individuals4_adv$relationship == "not_get_along") & 
    (connections_individuals4_adv$alter_source == 1), 
]

# Select the relevant columns (ego, alter, village code) and rename them accordingly.
connections_individuals4_adv <- cbind(
  connections_individuals4_adv$ego, 
  connections_individuals4_adv$alter, 
  connections_individuals4_adv$village_code_w4
)
colnames(connections_individuals4_adv) <- c('Respondent4', 'Alter', 'Village4')

# Save the filtered connections into a general variable.
connections_individuals4 <- connections_individuals4_adv

# The following merging with individuals_wave3 is commented out, as it appears the focus is on Wave 4.
# Merge individuals data for wave 3 and wave 4 based on Respondent IDs (if needed)
# individuals_wave3 <- merge(individuals_wave3, individuals_wave4, by.x = 'Respondent3', by.y = 'Respondent4')
# individuals_wave4 <- individuals_wave3

################################################################################
# Merge connections with Wave 4 individual attributes
################################################################################

# For consistency, select and rename only the first 7 columns of individuals_wave4.
individuals_wave4 <- as.data.frame(individuals_wave4[, 1:7])
colnames(individuals_wave4) <- c('Respondent4', 'Household4', 'Household3', 'gender', 'marital_status', 'age', 'village')

# Merge the Wave 4 connections with individuals_wave4 using the Respondent4 identifier.
connections_individuals4 <- merge(connections_individuals4, individuals_wave4, by = 'Respondent4')

# Merge again to attach the attributes for the 'Alter' respondent.
connections_individuals4 <- merge(connections_individuals4, individuals_wave4, by.x = 'Alter', by.y = 'Respondent4')

# Remove any rows that contain NA values after merging.
connections_individuals4 <- na.omit(as.data.frame(connections_individuals4))

# Extract unique village codes from the merged Wave 4 connections.
villages_4 <- as.numeric(unique(connections_individuals4[, 3]))

################################################################################
# Build village-specific graphs for Wave 4
################################################################################

# Initialize lists to store connection data and edge lists for Wave 4.
village_connection4 <- list()
edges4 <- list()

# The graphs for the three waves are named by convention:
# gr1_adv_h_in, gr3_adv_h_in, and gr4_adv_h_in.
gr4_adv <- list()

# Create a copy of individuals_wave4 for use in constructing adversarial graphs.
individuals_wave4_adv <- individuals_wave4

# Loop over each village code present in the Wave 4 connections.
for (i in villages_4) {
  # Subset the connections data for the current village.
  village_connection4[[i]] <- connections_individuals4[connections_individuals4$Village4 == i, ]
  
  # Create the edge list (from Respondent4 to Alter) for the current village.
  edges4[[i]] <- village_connection4[[i]][, c('Respondent4', 'Alter')]
  
  # Build an undirected graph using the edge list.
  gr4_adv[[i]] <- graph_from_edgelist(as.matrix(edges4[[i]]), directed = FALSE)
  
  # Identify missing respondents (nodes) in the graph that are present in individuals_wave4.
  individuals_i_wave4 <- individuals_wave4[individuals_wave4$village == i, ]$Respondent4
  missing_in_gr4_adv <- unique(setdiff(individuals_i_wave4, V(gr4_adv[[i]])$name))
  if (length(missing_in_gr4_adv) > 0) {
    # Add missing vertices to the graph.
    gr4_adv[[i]] <- add_vertices(gr4_adv[[i]], length(missing_in_gr4_adv), name = missing_in_gr4_adv)
  }
  
  # Simplify the graph to remove multiple edges between the same nodes.
  gr4_adv[[i]] <- igraph::simplify(gr4_adv[[i]], remove.multiple = TRUE)
  
  # Additional commented-out code can be added here for further processing if needed.
}

################################################################################
# Compute network metrics for Wave 4 networks
################################################################################

# Initialize a list to store network metric data frames for Wave 4.
network_data_list <- list()  # Reset the list
all_networks_df_4 <- list()   # List to store combined Wave 4 network metrics

# Loop through each village in the Wave 4 networks.
for (i in villages_4) {
  if (i==155 | i == 156) next  # Optionally skip village 156
  g <- gr4_adv[[i]]
  if (is.null(g) || vcount(g) == 0) next  # Skip if the graph is empty or null
  
  # Calculate the degree (number of adversarial connections) for each node in the graph.
  node_degree <- degree(g)
  
  # Create a data frame to store network metrics for the current village.
  network_df <- data.frame(
    Node = names(node_degree),
    Adversaries = node_degree,
    stringsAsFactors = FALSE
  )
  
  # Initialize columns for average neighbor in-degree and overall degree.
  network_df$Avg_Neighbor_In_Degree <- numeric(length(node_degree))
  network_df$Avg_Neighbor_Degree <- numeric(length(node_degree))
  
  # Compute the average neighbor degree for each node.
  for (j in 1:vcount(g)) {
    neighbors_ids <- neighbors(g, j)
    if (length(neighbors_ids) > 0) {
      # For undirected graphs, the in-degree is the same as the overall degree.
      network_df$Avg_Neighbor_In_Degree[j] <- mean(node_degree[neighbors_ids])
      network_df$Avg_Neighbor_Degree[j] <- mean(node_degree[neighbors_ids])
    } else {
      network_df$Avg_Neighbor_In_Degree[j] <- 0
      network_df$Avg_Neighbor_Degree[j] <- 0
    }
  }
  
  # Compute and add the edge density of the adversarial network.
  network_df$Adv_density <- edge_density(g)
  
  # Store the network metrics data frame for the current village.
  network_data_list[[i]] <- network_df
}

# Combine the individual village data frames into one for Wave 4.
all_networks_df_4 <- do.call(rbind, lapply(seq_along(network_data_list), function(i) {
  if (!is.null(network_data_list[[i]])) {
    cbind(Network = i, network_data_list[[i]])
  }
}))

