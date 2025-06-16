# --------------------------------------------------------------------------------
# This script processes a second set of networks (wave 3) to build aggregated
# networks, extract triangles (three-node cliques), assign edge types based on
# two network sources (friends and adversaries), merge in respondent-level data,
# and finally perform statistical tests comparing depression status.
# --------------------------------------------------------------------------------

# --- PART 1: Aggregate Networks from Two Sources (Wave 3: friends and adversaries) ---

# Initialize an empty list for aggregated networks. The list length is set to 
# match the length of the friends network list for wave 3 (gr3_fr).
gr3_h_in_aggr <- vector("list", length(gr3_fr))

# Loop through each network index defined in villages_1_3.
for (i in villages_1_3) {
  
  # Skip processing for network index 156 or if the adversary network for index i 
  # has no edges. This prevents errors when there are no adversary connections.
  if (i==155 | i == 156) next 
  if (length(E(gr3_adv[[i]])) == 0) next
  
  # Extract edge data from the friends network graph for the current village.
  # get.data.frame returns a dataframe representing the edges.
  edges_fr <- get.data.frame(gr3_fr[[i]], what = "edges")
  
  # Add a column 'type' with value 1 to denote that these edges come from the 
  # friends network.
  edges_fr['type'] <- 1
  
  # Extract edge data from the adversaries network graph for the current village.
  edges_adv <- get.data.frame(gr3_adv[[i]], what = "edges")
  
  # Add a column 'type' with value 2 to denote adversary connections.
  edges_adv['type'] <- 2
  
  # Combine the two dataframes (friends and adversaries) by row-binding.
  edges <- rbind(edges_fr, edges_adv)
  
  # Create an aggregated undirected network from the combined edges using 
  # graph_from_data_frame. The resulting graph will have an attribute 'type'
  # for each edge.
  gr3_h_in_aggr[[i]] <- graph_from_data_frame(edges, directed = FALSE)
}

# At this point, gr3_h_in_aggr contains the aggregated networks (friends + adversaries)
# for each village (except index 156 or those with no adversary edges).

# --- PART 2: Prepare Respondent Data for Wave 3 ---

# Select the required columns from the individuals dataset for wave 3.
# This dataset (individuals_wave3_fr) is assumed to include respondent identifiers
# and their depression status (depressed_w3). The column 'Respondent3' uniquely
# identifies respondents in wave 3.
selected_df <- individuals_wave3_fr %>% 
  dplyr::select(Respondent3, depressed_w3)

# Initialize an empty list to store triangles (three-node cliques) for each network.
triangles <- list()

# --- PART 3: Extract Triangles and Determine Edge Types in Each Aggregated Network ---

# Loop through each network index defined in villages_1_3.
for (i in villages_1_3) {
  
  # Skip index 156 or if the adversary network for index i has no edges.
  if (i == 155 | i == 156) next 
  if(length(E(gr3_adv[[i]])) == 0) next
  
  # Print the current index to monitor progress.
  print(i)
  
  # Convert the friends network for the current village to an undirected graph.
  # The mode "collapse" is used to combine multiple edges if present.
  g_fr <- as.undirected(
    gr3_fr[[i]],
    mode = c("collapse")
  )
  
  # Similarly, convert the adversary network for the current village to an undirected graph.
  g_adv <- as.undirected(
    gr3_adv[[i]],
    mode = c("collapse")
  )
  
  # --- Extract Three-Node Cliques (Triangles) ---
  # Identify all cliques (fully connected subgraphs) of size exactly 3 in the 
  # aggregated network for the current village.
  cl.tri = cliques(gr3_h_in_aggr[[i]], min = 3, max = 3)
  
  # For each triangle, extract the vertex names (or IDs) as a list.
  df <- lapply(cl.tri, function(x) { V(gr3_h_in_aggr[[i]])$name[x] })
  
  # Convert the list of vertex triplets into a dataframe with three columns.
  df2 = data.frame(matrix(unlist(df), ncol = 3, byrow = TRUE))
  
  # Rename the columns to indicate vertices.
  names(df2) <- c("v1", "v2", "v3")
  
  # Initialize an empty dataframe to store triangle data (with edge type info)
  # for the current network index.
  triangles[[i]] <- data.frame()
  
  # Loop over each triangle (row in df2).
  for (j in 1:dim(df2)[1]) {
    
    # Extract the three vertices forming the triangle.
    v1 <- df2[j, 1]
    v2 <- df2[j, 2]
    v3 <- df2[j, 3]
    
    # Initialize indicators to capture the type of connection between each pair.
    # For each pair, we have one variable for friends (ending with _1) and one for adversaries (_2).
    id_1_1 <- 0  # Indicator for v1-v2 in friends network
    id_1_2 <- 0  # Indicator for v1-v2 in adversaries network
    id_2_1 <- 0  # Indicator for v2-v3 in friends network
    id_2_2 <- 0  # Indicator for v2-v3 in adversaries network
    id_3_1 <- 0  # Indicator for v1-v3 in friends network
    id_3_2 <- 0  # Indicator for v1-v3 in adversaries network
    
    # Check for connection between v1 and v2 in the friends network.
    if (are.connected(g_fr, v1, v2)) {
      id_1_1 <- 1
    }
    # Check for connection between v1 and v2 in the adversaries network.
    if (are.connected(g_adv, v1, v2)) {
      id_1_2 <- 1
    }
    # Check for connection between v2 and v3 in the friends network.
    if (are.connected(g_fr, v2, v3)) {
      id_2_1 <- 1
    }
    # Check for connection between v2 and v3 in the adversaries network.
    if (are.connected(g_adv, v2, v3)) {
      id_2_2 <- 1
    }
    # Check for connection between v1 and v3 in the friends network.
    if (are.connected(g_fr, v1, v3)) {
      id_3_1 <- 1
    }
    # Check for connection between v1 and v3 in the adversaries network.
    if (are.connected(g_adv, v1, v3)) {
      id_3_2 <- 1
    }
    
    # Initialize temporary dataframes for each possible configuration.
    df_1 <- data.frame()
    df_2 <- data.frame()
    df_3 <- data.frame()
    df_4 <- data.frame()
    
    # --- Determine Edge Type Configuration for the Triangle ---
    # If all three edges are present in the friends network.
    if (id_1_1 == 1 & id_2_1 == 1 & id_3_1 == 1) {
      df_1 <- cbind(df2[j, ], 1, 1, 1)
      # Rename the new columns temporarily (will be corrected later).
      names(df_1) <- c("1", "1", "1")
    }
    # If edges between v1-v2 and v2-v3 are friends, and edge v1-v3 is adversary.
    if (id_1_1 == 1 & id_2_1 == 1 & id_3_2 == 1) {
      df_2 <- cbind(df2[j, ], 1, 1, 2)
      names(df_2) <- c("1", "1", "1")
    }
    # If edges between v1-v2 and v1-v3 are friends, and edge v2-v3 is adversary.
    if (id_1_1 == 1 & id_2_2 == 1 & id_3_1 == 1) {
      df_3 <- cbind(df2[j, ], 1, 2, 1)
      names(df_3) <- c("1", "1", "1")
    }
    # If edges between v2-v3 and v1-v3 are friends, and edge v1-v2 is adversary.
    if (id_1_2 == 1 & id_2_1 == 1 & id_3_1 == 1) {
      df_4 <- cbind(df2[j, ], 2, 1, 1)
      names(df_4) <- c("1", "1", "1")
    }
    
    # Combine any valid configurations for the current triangle and 
    # append them to the triangles list for the current network index.
    triangles[[i]] <- rbind(triangles[[i]], df_1, df_2, df_3, df_4)
  }
  
  # Rename columns of the triangles dataframe to meaningful names:
  # "v1", "v2", "v3" for vertices and "type_v1_v2", "type_v2_v3", "type_v3_v1"
  # for the type of connection between each vertex pair.
  names(triangles[[i]]) <- c("v1", "v2", "v3", "type_v1_v2", "type_v2_v3", "type_v3_v1")
  
  # Merge the triangle data with respondent-level information from selected_df.
  # First merge by matching v1 to Respondent3, then rename the depression column.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v1", by.y = "Respondent3", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v1 = depressed_w3)
  
  # Merge the information for vertex v2.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v2", by.y = "Respondent3", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v2 = depressed_w3)
  
  # Merge the information for vertex v3.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v3", by.y = "Respondent3", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v3 = depressed_w3)
}

# Save the triangle information into a new variable for further processing.
x_3_c <- triangles

# --- PART 4: Define Functions to Select Specific Nodes from Triangles ---

# Function to return a single node (with its depression status) from a triangle
# based on a specific pattern of edge types. The criteria check which edge is 
# of a different type (adversary) while the others are friends.
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
# when all edges in the triangle are of type friends (1).
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

# Initialize empty objects to store nodes from triangles that meet the criteria.
triangles_out_adv <- {}  # For triangles with one adversary edge pattern.
triangles_out_fr <- {}   # For triangles with all friends edges.

# Loop over each network index in villages_1_3.
for (i in villages_1_3) {
  # Skip index 156 or networks with no adversary edges.
  if (i==155 | i == 156) next 
  if (length(E(gr3_adv[[i]])) == 0) next
  print(i)
  
  # Extract the triangle dataframe for the current network.
  df_type_1 <- triangles[[i]]
  
  # Apply the adversary-specific function to each row in the triangle dataframe.
  results_out_adv <- apply(df_type_1, 1, find_node_out_adv)
  # Remove any NULL values (rows that did not match the criteria).
  results_out_adv <- results_out_adv[!sapply(results_out_adv, is.null)]
  
  if (!is.null(results_out_adv)) {
    # Combine the valid results into one dataframe.
    results_out_adv <- do.call(rbind, results_out_adv)
    # Append and keep unique rows.
    triangles_out_adv <- unique(rbind(triangles_out_adv, results_out_adv))
  }
  
  # Apply the all-friends function to each row.
  results_out_fr <- apply(df_type_1, 1, find_node_out_fr)
  results_out_fr <- results_out_fr[!sapply(results_out_fr, is.null)]
  
  if (!is.null(results_out_fr)) {
    results_out_fr <- do.call(rbind, results_out_fr)
    triangles_out_fr <- unique(rbind(triangles_out_fr, results_out_fr))
  }
}

# In the subsequent code, triangles_out_fr and triangles_out_adv are overwritten by 
# variables y_3 and z_3 respectively. (This might be intentional if y_3 and z_3 are
# externally defined or precomputed wave 3 triangle outputs.)
triangles_out_fr <- y_3_c 
triangles_out_adv <- z_3_c

# Rename columns to standardize the data frames.
colnames(triangles_out_fr) <- c("v3", "depressed_v3")
colnames(triangles_out_adv) <- c("v3", "depressed_v3")

# Convert the 'depressed_v1' column to integer in both data frames for consistent comparisons.
triangles_out_fr <- triangles_out_fr %>% mutate(depressed_v3 = as.integer(depressed_v3))
triangles_out_adv <- triangles_out_adv %>% mutate(depressed_v3 = as.integer(depressed_v3))

# Identify common elements (nodes) that appear in both outputs.
common_elements <- intersect(triangles_out_fr, triangles_out_adv)

# Remove the common elements from the all-friends output (if needed).
triangles_out_fr <- setdiff(triangles_out_fr, common_elements)

# --- PART 6: Merge with Individual-Level Data and Filter by Gender ---

# Merge the all-friends output with the individual-level wave 3 dataset.
joined_data <- left_join(triangles_out_fr, individuals_wave3_fr, by = c("v3" = "Respondent3"))

# Similarly, merge the adversary output.
joined_data_1 <- left_join(triangles_out_adv, individuals_wave3_fr, by = c("v3" = "Respondent3"))



# Filter the merged datasets to include only rows where gender equals 0.
# (Note: In this wave, gender==0 denote a specific group such as women.)
filtered_data <- joined_data %>% filter(gender == 0)
filtered_data_1 <- joined_data_1 %>% filter(gender == 0)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status (depressed) column from the filtered datasets,
# converting these columns to numeric vectors.
depressed_fr <- as.numeric(filtered_data$depressed_v3)
depressed_adv <- as.numeric(filtered_data_1$depressed_v3)

# Determine the number of respondents in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Calculate the count of respondents without depression (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table where:
# - Rows represent groups (FR for all-friends triangles, ADV for adversary triangles)
# - Columns represent depression status (Depressed=1 and Depressed=0)
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

# Also perform Fisher's exact test, which is suitable for small sample sizes.
print(fisher.test(contingency_table))





# Pearson's Chi-squared test with Yates' continuity correction
# data: contingency_table
# X-squared = 42.123, df = 1, p-value = 8.571e-11

# Also perform Fisher's exact test, which is suitable for small sample sizes.
# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 9.181e-11
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.5989154 0.7617411
# sample estimates: odds ratio = 0.6754836




# Filter the merged datasets to include only rows where gender equals 1.
# (Note: In this wave, gender==0 denote a specific group such as men.)
filtered_data <- joined_data %>% filter(gender == 1)
filtered_data_1 <- joined_data_1 %>% filter(gender == 1)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status (depressed) column from the filtered datasets,
# converting these columns to numeric vectors.
depressed_fr <- as.numeric(filtered_data$depressed_v3)
depressed_adv <- as.numeric(filtered_data_1$depressed_v3)

# Determine the number of respondents in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Calculate the count of respondents without depression (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table where:
# - Rows represent groups (FR for all-friends triangles, ADV for adversary triangles)
# - Columns represent depression status (Depressed=1 and Depressed=0)
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

# Also perform Fisher's exact test, which is suitable for small sample sizes.
print(fisher.test(contingency_table))


# Pearson's Chi-squared test with Yates' continuity correction
# data: contingency_table
# X-squared = 10.1, df = 1, p-value = 0.001482

# Also perform Fisher's exact test, which is suitable for small sample sizes.
# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 0.001427
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.6022602 0.8899585
# sample estimates: odds ratio = 0.7313754