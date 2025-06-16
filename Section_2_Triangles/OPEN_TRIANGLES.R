# --------------------------------------------------------------------------------
# This script processes a set of wave 1 networks to extract "open triangles"
# (i.e., triangles that are not fully closed by adversary edges), merge in respondent‐
# level depression data, and then compare depression status between groups.
# --------------------------------------------------------------------------------

# --- PART 1: Aggregate Networks from Two Sources (Friends and Adversaries) ---

# Initialize an empty list for aggregated networks.
# The list is set to have the same length as gr1_fr, which contains the friends network objects.
gr1_h_in_aggr <- vector("list", length(gr1_fr))

# Loop through each network index provided in villages_1_3.
for (i in villages_1_3) {
  # Skip the network if index equals 156 (a known exclusion)...
  #if (i == 156) next
  # ...or if the adversary network for this index has no edges.
  if (length(E(gr1_adv[[i]])) == 0) next
  
  # Extract the edge list from the friends network at index i.
  edges_fr <- get.data.frame(gr1_fr[[i]], what = "edges")
  
  # Add a new column named 'type' to denote that these edges come from the friends network.
  # Here, 1 is used to represent friends.
  edges_fr['type'] <- 1
  
  # Extract the edge list from the adversary network at index i.
  edges_adv <- get.data.frame(gr1_adv[[i]], what = "edges")
  
  # Add a column 'type' with value 2 to denote that these edges come from the adversary network.
  edges_adv['type'] <- 2
  
  # Combine the friends and adversary edge data into a single dataframe.
  edges <- rbind(edges_fr, edges_adv)
  
  # Create an aggregated undirected graph from the combined edges.
  # as.undirected() is applied with mode "collapse" to merge any duplicate edges.
  gr1_h_in_aggr[[i]] <- as.undirected(
    graph_from_data_frame(edges, directed = FALSE),
    mode = c("collapse")
  )
}

# Now, gr1_h_in_aggr contains the aggregated networks with an edge attribute 'type'
# that distinguishes friends (1) from adversaries (2).

# --- PART 2: Prepare Respondent-Level Data ---

# Select only the required columns (Respondent1 and depressed_w1) from the individuals data.
# This data will later be merged with triangle information.
selected_df <- individuals_wave1_fr %>% 
  dplyr::select(Respondent1, depressed_w1)

# --- PART 3: Extract Open Triangles from Aggregated Networks ---

# Initialize an empty list to store "open triangles" for each network.
# Here, an "open triangle" is defined as a set of three vertices that form a path of length 2,
# with the absence of certain adversary connections.
open_triangles <- list()

# Loop through each network index in villages_1_3.
for (i in villages_1_3) {
  # Skip index 156 and any network where the adversary network has no edges.
  #if (i == 156) next
  if (length(E(gr1_adv[[i]])) == 0) next
  
  print(i)  # Print the current index to monitor progress.
  
  # Create undirected versions of the friends and adversary networks for the current index.
  g_fr <- as.undirected(gr1_fr[[i]], mode = c("collapse"))
  g_adv <- as.undirected(gr1_adv[[i]], mode = c("collapse"))
  
  # For the aggregated network, ensure it is undirected with "collapse" mode.
  G <- as.undirected(gr1_h_in_aggr[[i]], mode = "collapse")
  
  # --- Extracting Open Triangles ---
  # The following block iterates over every vertex in G.
  # For each vertex v, it finds all neighbors (v1), then for each neighbor v1 it:
  #   - Retrieves the neighbors of v1 (v2) and ensures uniqueness.
  #   - Filters those v2 that are at a shortest-path distance of 2 from v.
  #   - For each such vertex (vv2), it returns a sorted vector of vertices (v, v1, vv2).
  # Finally, unique() is used to eliminate duplicate triangles.
  openTriList <- unique(
    do.call(c, lapply(as_ids(V(G)), function(v) {
      do.call(c, lapply(as_ids(neighbors(G, v)), function(v1) {
        # Get the neighbors of v1.
        v2 <- as_ids(neighbors(G, v1))
        v2 <- unique(v2)  # Remove duplicates.
        # Keep only those vertices that are at distance 2 from v.
        v2 <- v2[shortest.paths(G, v, v2) == 2]
        if (length(v2) != 0) {
          # For each candidate vertex, sort the triplet (to ensure uniqueness) and return.
          lapply(v2, function(vv2) {
            sorted_vertices <- sort(c(v, v1, vv2))
            return(sorted_vertices)
          })
        } else {
          list()  # Return an empty list if no candidate is found.
        }
      }))
    }))
  )
  
  # Convert the list of open triangles into a dataframe.
  # Each row will represent an open triangle with three vertices.
  df2 = data.frame(matrix(do.call(rbind, openTriList), ncol = 3, byrow = TRUE))
  
  # Assign names to the columns for clarity.
  names(df2) <- c("v1", "v2", "v3")
  
  # Initialize an empty dataframe for storing open triangles that meet a condition.
  open_triangles[[i]] <- data.frame()
  
  # Loop over each row (triangle) in the dataframe.
  for (j in 1:dim(df2)[1]) {
    # Extract the three vertices of the triangle.
    v1 <- df2[j, 1]
    v2 <- df2[j, 2]
    v3 <- df2[j, 3]
    
    # Initialize indicators for adversary connections between pairs.
    # (Here we are only interested in adversary edges.)
    id_1_2 <- 0  # For edge between v1 and v2 in the adversary network.
    id_3_2 <- 0  # For edge between v1 and v3 in the adversary network.
    
    # Check if there is an adversary connection between v1 and v2.
    if (are.connected(g_adv, v1, v2)) {
      id_1_2 <- 1
    }
    # Check if there is an adversary connection between v1 and v3.
    if (are.connected(g_adv, v1, v3)) {
      id_3_2 <- 1
    }
    
    # Initialize an empty dataframe to store the triangle if it meets the condition.
    df_1 <- data.frame()
    
    # If neither v1-v2 nor v1-v3 are connected by adversary edges, then keep the triangle.
    if (id_1_2 == 0 & id_3_2 == 0) {
      df_1 <- df2[j,]
      open_triangles[[i]] <- rbind(open_triangles[[i]], df_1)
    }
  }
  
  # Rename the columns for the open triangles dataframe.
  names(open_triangles[[i]]) <- c("v1", "v2", "v3")
  
  # Merge the open triangles with the respondent-level data based on v1.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df, 
                               by.x = "v1", by.y = "Respondent1", all.x = TRUE)
  # Rename the depression column for vertex v1.
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v1 = depressed_w1)
  
  # Merge with respondent data for vertex v2.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df, 
                               by.x = "v2", by.y = "Respondent1", all.x = TRUE)
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v2 = depressed_w1)
  
  # Merge with respondent data for vertex v3.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df, 
                               by.x = "v3", by.y = "Respondent1", all.x = TRUE)
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v3 = depressed_w1)
}

# Save the open triangles list for later use.
x1_open <- open_triangles

# Create a unique dataframe of vertices (v1) and their depression status from the open triangles.
y1_open <- unique(bind_rows(x1_open)[c("v1", "depressed_v1")])

# --- PART 4: Prepare Data for Intersection with Other Triangle Sets ---

# Here, the code assumes that there already exist two other dataframes,
# triangles_out_fr and triangles_out_adv, which represent nodes from different triangle
# configurations (e.g., fully closed triangles from friends networks and triangles with adversary edges).

# For consistency, assign new variable names:
triangles_out_fr <- y_c  # From fully friends triangles.
triangles_out_adv <- z_c # From triangles with adversary edges.

# Rename columns to standardize the data frames.
colnames(triangles_out_fr) <- c("v1", "depressed_v1")
colnames(triangles_out_adv) <- c("v1", "depressed_v1")

# Convert the 'depressed_v1' column to integer in both data frames for consistent comparisons.
triangles_out_fr <- triangles_out_fr %>% mutate(depressed_v1 = as.integer(depressed_v1))
triangles_out_adv <- triangles_out_adv %>% mutate(depressed_v1 = as.integer(depressed_v1))
y1_open <- y1_open %>% mutate(depressed_v1 = as.integer(depressed_v1))

# --- PART 5: Determine Common and Unique Elements Across Triangle Sets ---

# Compute intersections between the different triangle sets.
common_elements_1 <- intersect(triangles_out_fr, y1_open)
#common_elements_2 <- intersect(triangles_out_adv, y1_open)
#common_elements_3 <- intersect(triangles_out_fr, triangles_out_adv)

# Identify common elements (nodes) that appear in both outputs.
common_elements <- intersect(triangles_out_fr, triangles_out_adv)

# Compute set differences:#
#   - triangles_out_open: elements in y4_open not found in triangles_out_fr.
triangles_out_fr <- setdiff(triangles_out_fr, common_elements)
#   - Replace triangles_out_adv with those elements in y4_open not in triangles_out_adv.
x <- setdiff(y1_open, common_elements_1)
triangles_out_adv <- setdiff(x, triangles_out_adv)
#triangles_out_adv <- setdiff(triangles_out_adv , common_elements_2)



# --- PART 6: Merge with Respondent-Level Data and Filter by Gender ---

# Merge the fully friends triangles with the individuals dataset using v1 as the key.
joined_data <- left_join(triangles_out_fr, individuals_wave1_fr, by = c("v1" = "Respondent1"))

# Merge the adversary triangles with the individuals dataset.
joined_data_1 <- left_join(triangles_out_adv, individuals_wave1_fr, by = c("v1" = "Respondent1"))

# Filter the merged data to include only rows where gender equals 0.
filtered_data <- joined_data %>% filter(gender == 0)
filtered_data_1 <- joined_data_1 %>% filter(gender == 0)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status from the filtered datasets and convert to numeric.
depressed_fr <- as.numeric(filtered_data$depressed_w1)
depressed_adv <- as.numeric(filtered_data_1$depressed_w1)

# Determine the number of respondents in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count how many respondents are depressed (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of respondents not depressed (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table.
# Rows represent the groups: FR (fully friends triangles) and ADV (adversary triangles).
# Columns represent the depression status: Depressed=1 and Depressed=0.
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

# Also perform Fisher's exact test, which is particularly useful for small sample sizes.
print(fisher.test(contingency_table))


# Pearson's Chi-squared test with Yates' continuity correction
# data: contingency_table
# X-squared = 2.0994, df = 1, p-value = 0.1474

# Also perform Fisher's exact test, which is particularly useful for small sample sizes.
# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 0.1361
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.7232364 1.0488531
# sample estimates: odds ratio = 0.8707044

# Filter the merged data to include only rows where gender equals 0.
filtered_data <- joined_data %>% filter(gender == 1)
filtered_data_1 <- joined_data_1 %>% filter(gender == 1)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status from the filtered datasets and convert to numeric.
depressed_fr <- as.numeric(filtered_data$depressed_w1)
depressed_adv <- as.numeric(filtered_data_1$depressed_w1)

# Determine the number of respondents in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count how many respondents are depressed (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of respondents not depressed (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table.
# Rows represent the groups: FR (fully friends triangles) and ADV (adversary triangles).
# Columns represent the depression status: Depressed=1 and Depressed=0.
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

# Also perform Fisher's exact test, which is particularly useful for small sample sizes.
print(fisher.test(contingency_table))


# Pearson's Chi-squared test with Yates' continuity correction
# data: contingency_table
# X-squared = 2.0491, df = 1, p-value = 0.1523

# Also perform Fisher's exact test, which is particularly useful for small sample sizes.
# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 0.1389
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.5858974 1.0863247
# sample estimates: odds ratio = 0.7957783