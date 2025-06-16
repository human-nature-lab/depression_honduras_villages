# --------------------------------------------------------------------------------
# This script processes a third set of networks (wave 4) to build aggregated
# networks, extract triangles (three-node cliques), assign edge types based on
# two network sources (friends and adversaries), merge in respondent-level data,
# and finally perform statistical tests comparing depression status.
# --------------------------------------------------------------------------------

# --- PART 1: Aggregate Networks from Two Sources (Wave 4: friends and adversaries) ---

# Initialize an empty list for aggregated networks. The length of this list is
# set to match the number of network objects in the friends network list for wave 4 (gr4_fr).
gr4_h_in_aggr <- vector("list", length(gr4_fr))

# Loop through each network index specified in the vector villages_4.
for (i in villages_4) {
  # Skip processing for network index 156 or if the adversary network for the current
  # village has no edges. This avoids errors or empty graphs.
  if (i == 155 | i == 156) next 
  if (length(E(gr4_adv[[i]])) == 0) next
  
  # Extract edge data from the wave 4 friends network graph for the current village.
  edges_fr <- get.data.frame(gr4_fr[[i]], what = "edges")
  
  # Add a column 'type' with value 1 to indicate that these edges are from the friends network.
  edges_fr['type'] <- 1
  
  # Extract edge data from the wave 4 adversaries network graph for the current village.
  edges_adv <- get.data.frame(gr4_adv[[i]], what = "edges")
  
  # Add a column 'type' with value 2 to indicate adversary connections.
  edges_adv['type'] <- 2
  
  # Combine the two edge dataframes by row-binding.
  edges <- rbind(edges_fr, edges_adv)
  
  # Create an aggregated undirected graph from the combined edges.
  # The resulting graph will have a 'type' attribute for each edge.
  gr4_h_in_aggr[[i]] <- graph_from_data_frame(edges, directed = FALSE)
}

# At this point, gr4_h_in_aggr now contains the aggregated networks with the specified edge attributes.

# --- PART 2: Prepare Respondent Data for Wave 4 ---

# Select the required columns from the individuals dataset for wave 4.
# Here, individuals_wave4_fr is assumed to include the respondent identifier (Respondent4)
# and their depression status (depressed_w4).
selected_df <- individuals_wave4_fr %>% 
  dplyr::select(Respondent4, depressed_w4)

# Initialize an empty list to store triangle (three-node clique) information for each network.
triangles <- list()

# --- PART 3: Extract Triangles and Determine Edge Types in Each Aggregated Network ---

# Loop through each network index defined in villages_4.
for (i in villages_4) {
  # Skip network index 156 or if the adversary network for the current village has no edges.
  if (i == 155 | i == 156) next 
  if (length(E(gr4_adv[[i]])) == 0) next
  print(i)  # Print the current village index for tracking progress
  
  # Convert the wave 4 friends network for the current village to an undirected graph.
  # The mode "collapse" is used to merge multiple edges if they exist.
  g_fr <- as.undirected(
    gr4_fr[[i]],
    mode = c("collapse")
  )
  
  # Similarly, convert the adversaries network to an undirected graph.
  g_adv <- as.undirected(
    gr4_adv[[i]],
    mode = c("collapse")
  )
  
  # --- Extract Triangles (Cliques of size 3) ---
  # Identify all cliques (fully connected subgraphs) of size exactly 3 in the aggregated network.
  cl.tri = cliques(gr4_h_in_aggr[[i]], min = 3, max = 3)
  
  # For each triangle, extract the vertex names (or IDs) as a list.
  df <- lapply(cl.tri, function(x) { V(gr4_h_in_aggr[[i]])$name[x] })
  
  # Convert the list of vertex triplets into a dataframe with 3 columns.
  df2 = data.frame(matrix(unlist(df), ncol = 3, byrow = TRUE))
  
  # Name the columns to indicate the three vertices.
  names(df2) <- c("v1", "v2", "v3")
  
  # Initialize an empty dataframe to store triangle data (with edge type info) for the current network.
  triangles[[i]] <- data.frame()
  
  # Loop through each triangle (each row in df2).
  for (j in 1:dim(df2)[1]) {
    # Extract vertices forming the triangle.
    v1 <- df2[j, 1]
    v2 <- df2[j, 2]
    v3 <- df2[j, 3]
    
    # Initialize indicators for the type of connection between each pair.
    id_1_1 <- 0  # Indicates v1-v2 is connected in friends network
    id_1_2 <- 0  # Indicates v1-v2 is connected in adversaries network
    id_2_1 <- 0  # Indicates v2-v3 is connected in friends network
    id_2_2 <- 0  # Indicates v2-v3 is connected in adversaries network
    id_3_1 <- 0  # Indicates v1-v3 is connected in friends network
    id_3_2 <- 0  # Indicates v1-v3 is connected in adversaries network
    
    # Check connections between vertices in the friends network.
    if (are.connected(g_fr, v1, v2)) { id_1_1 <- 1 }
    if (are.connected(g_fr, v2, v3)) { id_2_1 <- 1 }
    if (are.connected(g_fr, v1, v3)) { id_3_1 <- 1 }
    
    # Check connections between vertices in the adversaries network.
    if (are.connected(g_adv, v1, v2)) { id_1_2 <- 1 }
    if (are.connected(g_adv, v2, v3)) { id_2_2 <- 1 }
    if (are.connected(g_adv, v1, v3)) { id_3_2 <- 1 }
    
    # Initialize temporary dataframes to hold different configurations of edge types.
    df_1 <- data.frame()
    df_2 <- data.frame()
    df_3 <- data.frame()
    df_4 <- data.frame()
    
    # Determine configuration when all three edges are present in the friends network.
    if (id_1_1 == 1 & id_2_1 == 1 & id_3_1 == 1) {
      df_1 <- cbind(df2[j, ], 1, 1, 1)
      # Temporarily assign column names (will be updated later).
      names(df_1) <- c("1", "1", "1")
    }
    # Determine configuration when v1-v2 and v2-v3 are friends, but v1-v3 is adversary.
    if (id_1_1 == 1 & id_2_1 == 1 & id_3_2 == 1) {
      df_2 <- cbind(df2[j, ], 1, 1, 2)
      names(df_2) <- c("1", "1", "1")
    }
    # Determine configuration when v1-v2 and v1-v3 are friends, but v2-v3 is adversary.
    if (id_1_1 == 1 & id_2_2 == 1 & id_3_1 == 1) {
      df_3 <- cbind(df2[j, ], 1, 2, 1)
      names(df_3) <- c("1", "1", "1")
    }
    # Determine configuration when v2-v3 and v1-v3 are friends, but v1-v2 is adversary.
    if (id_1_2 == 1 & id_2_1 == 1 & id_3_1 == 1) {
      df_4 <- cbind(df2[j, ], 2, 1, 1)
      names(df_4) <- c("1", "1", "1")
    }
    
    # Combine any valid configurations for the current triangle and append them to the triangles list.
    triangles[[i]] <- rbind(triangles[[i]], df_1, df_2, df_3, df_4)
  }
  
  # Rename the columns of the triangle dataframe to meaningful names:
  # "v1", "v2", "v3" for vertices and "type_v1_v2", "type_v2_v3", "type_v3_v1" for the connection types.
  names(triangles[[i]]) <- c("v1", "v2", "v3", "type_v1_v2", "type_v2_v3", "type_v3_v1")
  
  # Merge the triangle data with the respondent-level wave 4 dataset.
  # First, merge by matching v1 with Respondent4.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v1", by.y = "Respondent4", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v1 = depressed_w4)
  
  # Next, merge by matching v2 with Respondent4.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v2", by.y = "Respondent4", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v2 = depressed_w4)
  
  # Finally, merge by matching v3 with Respondent4.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v3", by.y = "Respondent4", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v3 = depressed_w4)
}

# Save the triangle information for wave 4 into a new variable.
x_4_c <- triangles

# --- PART 4: Define Functions to Select Specific Nodes from Triangles ---

# Function to return a single node (with its depression status) from a triangle
# based on a specific pattern of edge types. For example, if two edges are friends (1)
# and one edge is adversary (2), the function returns the vertex corresponding to that pattern.
find_node_out_adv <- function(row) {
  if (row["type_v1_v2"] == 1 & row["type_v3_v1"] == 1 & row["type_v2_v3"] == 2) {
    return(data.frame(vertex = row["v1"], depressed = row["depressed_v1"]))
  } else if (row["type_v1_v2"] == 1 & row["type_v2_v3"] == 1 & row["type_v3_v1"] == 2) {
    return(data.frame(vertex = row["v2"], depressed = row["depressed_v2"]))
  } else if (row["type_v2_v3"] == 1 & row["type_v3_v1"] == 1 & row["type_v1_v2"] == 2) {
    return(data.frame(vertex = row["v3"], depressed = row["depressed_v3"]))
  } else {
    return(NULL)
  }
}

# Function to return all three nodes from a triangle (with their depression status)
# when all three edges in the triangle are friends (type 1).
find_node_out_fr <- function(row) {
  if (row["type_v1_v2"] == 1 & row["type_v3_v1"] == 1 & row["type_v2_v3"] == 1) {
    return(data.frame(
      vertex = c(row["v1"], row["v2"], row["v3"]),
      depressed = c(row["depressed_v1"], row["depressed_v2"], row["depressed_v3"])
    ))
  } else {
    return(NULL)
  }
}

# --- PART 5: Apply Functions to Extract Specific Nodes from Triangles Across Networks ---

# Initialize empty objects to store nodes from triangles that match the criteria.
triangles_out_adv <- {}  # For triangles with one adversary edge pattern.
triangles_out_fr <- {}   # For triangles where all edges are friends.

# Loop over each network index defined in villages_4.
for (i in villages_4) {
  # Skip network index 156 or if there are no adversary edges.
  if (i == 155 | i == 156) next 
  if (length(E(gr4_adv[[i]])) == 0) next
  print(i)  # Print current network index for tracking
  
  # Extract the triangle dataframe for the current village.
  df_type_1 <- triangles[[i]]
  
  # Apply the adversary-specific function on each row to extract the relevant node.
  results_out_adv <- apply(df_type_1, 1, find_node_out_adv)
  # Filter out any NULL values.
  results_out_adv <- results_out_adv[!sapply(results_out_adv, is.null)]
  
  if (!is.null(results_out_adv)) {
    # Combine the list of results into a single dataframe.
    results_out_adv <- do.call(rbind, results_out_adv)
    triangles_out_adv <- unique(rbind(triangles_out_adv, results_out_adv))
  }
  
  # Apply the all-friends function to extract nodes from triangles where all edges are friends.
  results_out_fr <- apply(df_type_1, 1, find_node_out_fr)
  results_out_fr <- results_out_fr[!sapply(results_out_fr, is.null)]
  
  if (!is.null(results_out_fr)) {
    results_out_fr <- do.call(rbind, results_out_fr)
    triangles_out_fr <- unique(rbind(triangles_out_fr, results_out_fr))
  }
}

# Save the results into separate variables.
triangles_out_fr <- y_4_c 
triangles_out_adv <- z_4_c 

# Rename columns to standardize the data frames.
colnames(triangles_out_fr) <- c("v4", "depressed_v4")
colnames(triangles_out_adv) <- c("v4", "depressed_v4")

# Convert the 'depressed_v1' column to integer in both data frames for consistent comparisons.
triangles_out_fr <- triangles_out_fr %>% mutate(depressed_v4 = as.integer(depressed_v4))
triangles_out_adv <- triangles_out_adv %>% mutate(depressed_v4 = as.integer(depressed_v4))

# Identify common elements (nodes) that appear in both outputs.
common_elements <- intersect(triangles_out_fr, triangles_out_adv)

# Remove the common elements from the all-friends output (if needed).
triangles_out_fr <- setdiff(triangles_out_fr, common_elements)


# Merge the all-friends output with the wave 4 respondent-level dataset.
joined_data <- left_join(triangles_out_fr, individuals_wave4_fr, by = c("v4" = "Respondent4"))

# Similarly, merge the adversary output with the respondent-level data.
joined_data_1 <- left_join(triangles_out_adv, individuals_wave4_fr, by = c("v4" = "Respondent4"))

# Filter the merged datasets to include only rows where gender equals 0.
# (Here, gender==0 is used as the filtering criterion; adjust as needed based on coding.)
filtered_data <- joined_data %>% filter(gender == 0)
filtered_data_1 <- joined_data_1 %>% filter(gender == 0)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status from both filtered datasets, converting to numeric vectors.
depressed_fr <- as.numeric(filtered_data$depressed_v4)
depressed_adv <- as.numeric(filtered_data_1$depressed_v4)

# Determine the number of respondents in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of respondents without depression (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table.
# Rows represent groups: FR (all-friends triangles) and ADV (adversary triangles).
# Columns represent outcomes: Depressed=1 and Depressed=0.
contingency_table <- matrix(c(count_fr_1, count_fr_0,
                              count_adv_1, count_adv_0),
                            nrow = 2, byrow = TRUE)
rownames(contingency_table) <- c("FR", "ADV")
colnames(contingency_table) <- c("Depressed=1", "Depressed=0")

# Print the contingency table.
contingency_table

# Perform a chi-square test on the contingency table.
chisq_result <- chisq.test(contingency_table)
print(chisq_result)

# Also perform Fisher's exact test (useful for small sample sizes).
print(fisher.test(contingency_table))


# Pearson's Chi-squared test with Yates' continuity correction
# data: contingency_table
# X-squared = 2.6204, df = 1, p-value = 0.1055

# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 0.1011
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.7306927 1.0300021
# sample estimates: odds ratio = 0.8673088


# Filter the merged datasets to include only rows where gender equals 1.
# (Here, gender==1 is used as the filtering criterion; adjust as needed based on coding.)
filtered_data <- joined_data %>% filter(gender == 1)
filtered_data_1 <- joined_data_1 %>% filter(gender == 1)

# --- PART 8: Prepare Data for Statistical Testing ---

# Extract the depression status from both filtered datasets, converting to numeric vectors.
depressed_fr <- as.numeric(filtered_data$depressed_v4)
depressed_adv <- as.numeric(filtered_data_1$depressed_v4)

# Determine the number of respondents in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of respondents without depression (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table.
# Rows represent groups: FR (all-friends triangles) and ADV (adversary triangles).
# Columns represent outcomes: Depressed=1 and Depressed=0.
contingency_table <- matrix(c(count_fr_1, count_fr_0,
                              count_adv_1, count_adv_0),
                            nrow = 2, byrow = TRUE)
rownames(contingency_table) <- c("FR", "ADV")
colnames(contingency_table) <- c("Depressed=1", "Depressed=0")

# Print the contingency table.
contingency_table

# Perform a chi-square test on the contingency table.
chisq_result <- chisq.test(contingency_table)
print(chisq_result)

# Also perform Fisher's exact test (useful for small sample sizes).
print(fisher.test(contingency_table))


# Pearson's Chi-squared test with Yates' continuity correction
# data: contingency_table
# X-squared = 0.85009, df = 1, p-value = 0.3565

# Also perform Fisher's exact test (useful for small sample sizes).
# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 0.3438
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.6521432 1.1629401
# sample estimates: odds ratio = 0.8680528