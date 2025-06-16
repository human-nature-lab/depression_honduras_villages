# -------------------------------------------------------------
# This script processes a series of network graphs to build
# aggregated networks, extract triangles (three-node cliques),
# assign edge types based on two network sources, and finally
# conduct statistical tests (chi-square and Fisher's exact) on
# depression status associated with the nodes in these triangles.
# -------------------------------------------------------------

# --- PART 1: Aggregate Networks from Two Sources (friends and adversaries) ---

# Assuming gr1_fr is your input graph (a list of network graphs for friends)
# and gr1_adv is a similar list of networks representing adversary relations.

# Convert the graph to an undirected graph
# (Later on, the aggregated network is built as undirected.)

# Initialize an empty list for aggregated networks.
# The length is set to the same as gr1_fr (i.e., one network per index).
gr1_h_in_aggr <- vector("list", length(gr1_fr))

# Loop through each network index contained in villages_1_3
for (i in villages_1_3) {
  
  # Skip network index 156 (perhaps a known problematic or excluded network)
  #if(i == 156) next
  
  # Extract edge data from the friends network graph for village i.
  # get.data.frame returns a dataframe of the edges.
  edges_fr <- get.data.frame(gr1_fr[[i]], what = "edges")
  
  # Add a column 'type' to indicate this edge comes from the friends network.
  # Here, 1 is used to denote friends.
  edges_fr['type'] <- 1
  
  # Extract edge data from the adversaries network graph for village i.
  edges_adv <- get.data.frame(gr1_adv[[i]], what = "edges")
  
  # Add a column 'type' to denote that these edges come from the adversary network.
  # Here, 2 is used to denote adversaries.
  edges_adv['type'] <- 2
  
  # Combine the two edge dataframes by row-binding them.
  # This creates a single dataframe that includes all edges from both networks.
  edges <- rbind(edges_fr, edges_adv)
  
  # Create an aggregated undirected graph from the combined edges.
  # graph_from_data_frame builds the graph from the dataframe of edges.
  gr1_h_in_aggr[[i]] <- igraph::graph_from_data_frame(edges, directed = FALSE)
}

# At this point, gr1_h_in_aggr now contains the aggregated (combined) networks
# with the specified edge type attributes for each village.

# --- PART 2: Prepare Additional Data and Initialize Storage for Triangles ---

# Selecting the required columns from the individuals dataset
# individuals_wave1_fr is assumed to contain respondent-level data.
selected_df <- individuals_wave1_fr %>% 
  dplyr::select(Respondent1, depressed_w1)  # Keep respondent identifier and depression status

# Initialize an empty list to hold the triangles (three-node cliques) for each network.
triangles <- list()

# --- PART 3: Loop Through Each Network to Process Triangles ---

# For each network index in villages_1_3, process the networks:
for (i in villages_1_3) {
  
  # Skip network index 156 as before.
  #if(i == 156) next
  
  # Print current network index for monitoring progress.
  print(i)
  
  # Convert the friends network to an undirected graph.
  # The mode "collapse" combines multiple edges if present.
  g_fr <- as.undirected(
    gr1_fr[[i]],
    mode = c("collapse")
  )
  
  # Similarly, convert the adversaries network to an undirected graph.
  g_adv <- as.undirected(
    gr1_adv[[i]],
    mode = c("collapse")
  )
  
  # --- Extract Triangles (Cliques of size 3) ---
  # Find all cliques (fully connected subgraphs) of size exactly 3 in the aggregated network.
  cl.tri = cliques(gr1_h_in_aggr[[i]], min = 3, max = 3)
  
  # For each triangle, extract the names (or ids) of the vertices.
  df <- lapply(cl.tri, function(x) { V(gr1_h_in_aggr[[i]])$name[x] })
  
  # Convert the list of vertex triples into a dataframe with 3 columns.
  df2 = data.frame(matrix(unlist(df), ncol = 3, byrow = TRUE))
  
  # Rename the columns to indicate vertices.
  names(df2) <- c("v1", "v2", "v3")
  
  # Initialize an empty dataframe to store triangles with associated edge types.
  triangles[[i]] <- data.frame()
  
  # Loop through each row (triangle) in the dataframe df2.
  for(j in 1:dim(df2)[1]){
    
    # Extract vertices of the current triangle.
    v1 <- df2[j, 1]
    v2 <- df2[j, 2]
    v3 <- df2[j, 3]
    
    # Initialize indicators for connection types between pairs of vertices.
    # id_x_y corresponds to the connection between two nodes: 1 for friends, 2 for adversaries.
    id_1_1 <- 0  # For edge between v1 and v2 in friends network
    id_1_2 <- 0  # For edge between v1 and v2 in adversaries network
    id_2_1 <- 0  # For edge between v2 and v3 in friends network
    id_2_2 <- 0  # For edge between v2 and v3 in adversaries network
    id_3_1 <- 0  # For edge between v1 and v3 in friends network
    id_3_2 <- 0  # For edge between v1 and v3 in adversaries network
    
    # Check if v1 and v2 are connected in the friends network.
    if (are.connected(g_fr, v1, v2)) {
      id_1_1 <- 1
    }
    # Check if v1 and v2 are connected in the adversaries network.
    if (are.connected(g_adv, v1, v2)) {
      id_1_2 <- 1
    }
    # Check if v2 and v3 are connected in the friends network.
    if (are.connected(g_fr, v2, v3)) {
      id_2_1 <- 1
    }
    # Check if v2 and v3 are connected in the adversaries network.
    if (are.connected(g_adv, v2, v3)) {
      id_2_2 <- 1
    }
    # Check if v1 and v3 are connected in the friends network.
    if (are.connected(g_fr, v1, v3)) {
      id_3_1 <- 1
    }
    # Check if v1 and v3 are connected in the adversaries network.
    if (are.connected(g_adv, v1, v3)) {
      id_3_2 <- 1
    }
    
    # Initialize temporary dataframes to store edge-type combinations.
    df_1 <- data.frame()
    df_2 <- data.frame()
    df_3 <- data.frame()
    df_4 <- data.frame()
    
    # For each combination of edge types, if the edges exist, bind the triangle info
    # along with an indicator of which network provided which edge.
    # Combination 1: All three edges are from the friends network.
    if(id_1_1 == 1 & id_2_1 == 1 & id_3_1 == 1) {
      df_1 <- cbind(df2[j, ], 1, 1, 1)
      # Rename the newly added columns arbitrarily (later corrected)
      names(df_1) <- c("1", "1", "1")
    }
    # Combination 2: Two friends edges (v1-v2 and v2-v3) and one adversary edge (v1-v3)
    if(id_1_1 == 1 & id_2_1 == 1 & id_3_2 == 1) {
      df_2 <- cbind(df2[j, ], 1, 1, 2)
      names(df_2) <- c("1", "1", "1")
    }
    # Combination 3: Friends edge for v1-v2, adversary edge for v2-v3, and friends edge for v1-v3
    if(id_1_1 == 1 & id_2_2 == 1 & id_3_1 == 1) {
      df_3 <- cbind(df2[j, ], 1, 2, 1)
      names(df_3) <- c("1", "1", "1")
    }
    # Combination 4: Adversary edge for v1-v2, friends edges for v2-v3 and v1-v3.
    if(id_1_2 == 1 & id_2_1 == 1 & id_3_1 == 1) {
      df_4 <- cbind(df2[j, ], 2, 1, 1)
      names(df_4) <- c("1", "1", "1")
    }
    
    # Append all the found configurations (if any) for this triangle to the list element.
    triangles[[i]] <- rbind(triangles[[i]], df_1, df_2, df_3, df_4)
  }
  
  # Rename the columns of the triangle dataframe to meaningful names:
  # "v1", "v2", "v3" for the vertices; and "type_v1_v2", "type_v2_v3", "type_v3_v1" for the connection types.
  names(triangles[[i]]) <- c("v1", "v2", "v3", "type_v1_v2", "type_v2_v3", "type_v3_v1")
  
  # Merge the triangle data with respondent depression information.
  # First, merge using v1; then rename the depression column.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v1", by.y = "Respondent1", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v1 = depressed_w1)
  
  # Merge using v2 and then rename.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v2", by.y = "Respondent1", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v2 = depressed_w1)
  
  # Merge using v3 and then rename.
  triangles[[i]] <- merge(triangles[[i]], selected_df, by.x = "v3", by.y = "Respondent1", all.x = TRUE)
  triangles[[i]] <- triangles[[i]] %>% 
    rename(depressed_v3 = depressed_w1)
}

# After the loop, 'triangles' is a list where each element corresponds to a village network,
# containing the triangles with vertex ids, edge types, and depression status for each vertex.
x <- triangles

# For debugging or checking, get unique pairs of vertex v1 and its depression status.
unique(bind_rows(x)[c('v1','depressed_v1')])

# --- PART 4: Define Functions to Select Specific Nodes from Triangles ---

# The function below checks a triangle (given as a row of data) and returns a single node 
# (with its depression status) if the pattern of edge types matches the first specified criterion.
find_node_out_adv <- function(row) {
  if (row["type_v1_v2"] == 1 & row["type_v3_v1"] == 1 & row["type_v2_v3"] == 2) {
    # If edges v1-v2 and v3-v1 are friends (type 1) and edge v2-v3 is adversary (type 2),
    # return v1's information.
    return(data.frame(vertex = row["v1"], depressed = row["depressed_v1"]))
  } else if (row["type_v1_v2"] == 1 & row["type_v2_v3"] == 1 & row["type_v3_v1"] == 2) {
    # If edges v1-v2 and v2-v3 are friends and edge v3-v1 is adversary, return v2's info.
    return(data.frame(vertex = row["v2"], depressed = row["depressed_v2"]))
  } else if (row["type_v2_v3"] == 1 & row["type_v3_v1"] == 1 & row["type_v1_v2"] == 2) {
    # If edges v2-v3 and v3-v1 are friends and edge v1-v2 is adversary, return v3's info.
    return(data.frame(vertex = row["v3"], depressed = row["depressed_v3"]))
  } else {
    # If none of the above conditions are met, return NULL.
    return(NULL)
  }
}

# The function below returns all three nodes from a triangle if all edges are friends.
# It returns a dataframe with three rows (one for each vertex) containing vertex id and depression status.
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

# Copy triangles to x (for convenience).
x_c <- triangles

# Initialize storage for output nodes for the adversary pattern and the all-friends pattern.
triangles_out_adv <- {}  # Will store nodes meeting adversary criteria.
triangles_out_fr <- {}   # Will store nodes from triangles with all friends.

# Loop through each network in villages_1_3.
for(i in villages_1_3){
  #if(i == 156) next  # Again, skip index 156.
  print(i)
  
  # Extract the triangle dataframe for the current network.
  df_type_1 <- triangles[[i]]
  
  # Apply the adversary-specific function on each row of the dataframe.
  results_out_adv <- apply(df_type_1, 1, find_node_out_adv)
  
  # Remove any NULL values resulting from rows that do not meet the criteria.
  results_out_adv <- results_out_adv[!sapply(results_out_adv, is.null)]
  
  if(!is.null(results_out_adv)){
    # Combine the list of results into a single dataframe.
    results_out_adv <- do.call(rbind, results_out_adv)
    # Append to the aggregated output, removing duplicates.
    triangles_out_adv <- unique(rbind(triangles_out_adv, results_out_adv))
  }
  
  # Similarly, apply the all-friends function on each row.
  results_out_fr <- apply(df_type_1, 1, find_node_out_fr)
  results_out_fr <- results_out_fr[!sapply(results_out_fr, is.null)]
  
  if(!is.null(results_out_fr)){
    results_out_fr <- do.call(rbind, results_out_fr)
    triangles_out_fr <- unique(rbind(triangles_out_fr, results_out_fr))
  }
}

# Save the outputs to variables for further processing.
triangles_out_fr <- y_c 
triangles_out_adv <- z_c 

# Rename columns to standardize the data frames.
colnames(triangles_out_fr) <- c("v1", "depressed_v1")
colnames(triangles_out_adv) <- c("v1", "depressed_v1")

# Convert the 'depressed_v1' column to integer in both data frames for consistent comparisons.
triangles_out_fr <- triangles_out_fr %>% mutate(depressed_v1 = as.integer(depressed_v1))
triangles_out_adv <- triangles_out_adv %>% mutate(depressed_v1 = as.integer(depressed_v1))

# Identify common elements (nodes) that appear in both outputs.
common_elements <- intersect(triangles_out_fr, triangles_out_adv)

# Remove the common elements from the all-friends output (if needed).
triangles_out_fr <- setdiff(triangles_out_fr, common_elements)

# Optionally, one could merge common_elements into triangles_out_adv (line commented out).
# triangles_out_adv <- rbind(triangles_out_adv, common_elements)

# --- PART 6: Merge with Individual-Level Data and Filter by Gender ---


# Merge the fully friends triangles with the individuals dataset using v1 as the key.
joined_data <- left_join(triangles_out_fr, individuals_wave1_fr, by = c("v1" = "Respondent1"))

# Merge the adversary triangles with the individuals dataset.
joined_data_1 <- left_join(triangles_out_adv, individuals_wave1_fr, by = c("v1" = "Respondent1"))

# Filter the merged data to include only rows where gender equals 0.
# (Assuming gender is coded such that 0 represents a specific group, e.g., women.)
filtered_data <- joined_data %>% filter(gender == 0)
filtered_data_1 <- joined_data_1 %>% filter(gender == 0)

# --- PART 7: Prepare Data for Statistical Testing (Chi-Square and Fisher's Test) ---

# Extract the 'depressed' column from the filtered dataframes,
# converting them to numeric vectors.
# Extract the depression status from the filtered datasets and convert to numeric.
depressed_fr <- as.numeric(filtered_data$depressed_w1)
depressed_adv <- as.numeric(filtered_data_1$depressed_w1)

# Determine the lengths (number of individuals) in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of individuals with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of individuals without depression (coded as 0).
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table.
# Rows correspond to group (FR = all friends triangles, ADV = adversary triangle),
# and columns represent depression status.
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

# Also perform Fisher's exact test (often used when sample sizes are small).
print(fisher.test(contingency_table))

#FOR WOMEN

# Pearson's Chi-squared test with Yates' continuity correction
# X-squared = 22.935, df = 1, p-value = 1.676e-06
#
# Fisher's Exact Test for Count Data
# p-value = 1.642e-06
# Alternative hypothesis: true odds ratio is not equal to 1
# 95% confidence interval: [0.7559057, 0.8900656]
# Sample estimate (odds ratio): 0.820214
#
# Both tests indicate strong evidence against the null hypothesis,
# suggesting a significant association between the variables.




## Merge the fully friends triangles with the individuals dataset using v1 as the key.
joined_data <- left_join(triangles_out_fr, individuals_wave1_fr, by = c("v1" = "Respondent1"))

# Merge the adversary triangles with the individuals dataset.
joined_data_1 <- left_join(triangles_out_adv, individuals_wave1_fr, by = c("v1" = "Respondent1"))

# Filter the merged data to include only rows where gender equals 0.
# (Assuming gender is coded such that 0 represents a specific group, e.g., women.)
filtered_data <- joined_data %>% filter(gender == 1)
filtered_data_1 <- joined_data_1 %>% filter(gender == 1)

# --- PART 7: Prepare Data for Statistical Testing (Chi-Square and Fisher's Test) ---

# Extract the 'depressed' column from the filtered dataframes,
# converting them to numeric vectors.
# Extract the depression status from the filtered datasets and convert to numeric.
depressed_fr <- as.numeric(filtered_data$depressed_w1)
depressed_adv <- as.numeric(filtered_data_1$depressed_w1)

# Determine the lengths (number of individuals) in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of individuals with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of individuals without depression (coded as 0).
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table.
# Rows correspond to group (FR = all friends triangles, ADV = adversary triangle),
# and columns represent depression status.
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

# Also perform Fisher's exact test (often used when sample sizes are small).
print(fisher.test(contingency_table))


# Pearson's Chi-squared test with Yates' continuity correction
# X-squared = 6.9932, df = 1, p-value = 0.008182
#
# Fisher's Exact Test for Count Data
# p-value = 0.008004
# Alternative hypothesis: true odds ratio is not equal to 1
# 95% confidence interval: [0.7803408, 0.9644613]
# Sample estimate (odds ratio): 0.8672981
#
# Fisher’s exact test is also performed, which is often used when sample sizes are small.
# Both tests suggest a statistically significant association.
