# --------------------------------------------------------------------------------
# This script processes a set of wave 4 networks to extract “open triangles”
# from aggregated networks that combine friends and adversary edges. It then
# merges these open triangle data with respondent-level depression data,
# computes intersections and set differences with other triangle sets, and finally
# performs statistical tests (chi-square and Fisher's exact tests) to compare depression
# status between groups (filtered by gender). Separate results are provided for females and males.
# --------------------------------------------------------------------------------

# --- PART 1: Aggregate Networks from Two Sources (Friends and Adversaries) for Wave 4 ---

# Initialize an empty list for aggregated networks.
# The list is set to have the same length as the number of friends network objects in gr4_fr.
gr4_h_in_aggr <- vector("list", length(gr4_fr))

# Loop through each network index in villages_4.
for (i in villages_4) {
  # Skip network index 156 (an exclusion criterion).
  print(i)
  if (i==155 | i == 156) next 
  # Also skip if the adversary network for index i has no edges.
  
  if (is.null(gr4_adv[[i]])) next  
  if(length(E(gr4_adv[[i]])) == 0) next
  
  # Extract the edge list (dataframe) from the friends network at index i.
  edges_fr <- get.data.frame(gr4_fr[[i]], what = "edges")
  # Add a column 'type' with value 1 to indicate these edges come from the friends network.
  edges_fr['type'] <- 1
  
  # Extract the edge list from the adversary network at index i.
  edges_adv <- get.data.frame(gr4_adv[[i]], what = "edges")
  # Add a column 'type' with value 2 to indicate these edges come from the adversary network.
  edges_adv['type'] <- 2
  
  # Combine the two edge dataframes (friends and adversaries) by row-binding them.
  edges <- rbind(edges_fr, edges_adv)
  
  # Create an aggregated graph from the combined edges.
  # The graph is made undirected and duplicate edges are collapsed.
  gr4_h_in_aggr[[i]] <- as.undirected(
    graph_from_data_frame(edges, directed = FALSE),
    mode = c("collapse")
  )
}

# Now, gr4_h_in_aggr contains the aggregated networks for wave 4 with a 'type' attribute
# that distinguishes friends (1) from adversaries (2).

# --- PART 2: Prepare Respondent-Level Data for Wave 4 ---

# Select only the required columns from the individuals data for wave 4.
# 'Respondent4' identifies the respondent and 'depressed_w4' indicates their depression status.
selected_df <- individuals_wave4_fr %>% 
  dplyr::select(Respondent4, depressed_w4)

# --- PART 3: Extract Open Triangles from the Aggregated Networks ---

# Initialize an empty list to store the open triangles for each network.
open_triangles <- list()

# Loop through each network index in villages_4.
for (i in villages_4) {
  # Skip index 156 and any network where the adversary network has no edges.
  if (i==155 | i == 156) next 
  if (is.null(gr4_adv[[i]])) next  
  if(length(E(gr4_adv[[i]])) == 0) next
  
  print(i)  # Print the current index to monitor progress.
  
  # Convert the friends network for index i to an undirected graph (collapse mode).
  g_fr <- as.undirected(gr4_fr[[i]], mode = c("collapse"))
  # Convert the adversary network similarly.
  g_adv <- as.undirected(gr4_adv[[i]], mode = c("collapse"))
  
  # Ensure the aggregated network is undirected with collapse mode.
  G <- as.undirected(gr4_h_in_aggr[[i]], mode = "collapse")
  
  # --- Extracting Open Triangles ---
  # For every vertex in G:
  #   - For each vertex v, loop through its neighbors (v1).
  #   - For each neighbor v1, get its neighbors (v2) and ensure uniqueness.
  #   - Select only those v2 for which the shortest path from v equals 2.
  #   - For each such candidate (vv2), sort the triplet (v, v1, vv2) to ensure a unique representation.
  # The output is a list of candidate open triangles.
  openTriList <- unique(
    do.call(c, lapply(as_ids(V(G)), function(v) {
      do.call(c, lapply(as_ids(neighbors(G, v)), function(v1) {
        # Retrieve neighbors of v1 and remove duplicate vertices.
        v2 <- as_ids(neighbors(G, v1))
        v2 <- unique(v2)
        # Keep only vertices that are exactly at distance 2 from v.
        v2 <- v2[shortest.paths(G, v, v2) == 2]
        if (length(v2) != 0) {
          # For each candidate, return a sorted vector of vertices forming the triangle.
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
  # Each row represents an open triangle with three vertices.
  df2 = data.frame(matrix(do.call(rbind, openTriList), ncol = 3, byrow = TRUE))
  
  # Name the columns to represent the three vertices.
  names(df2) <- c("v1", "v2", "v3")
  
  # Initialize an empty dataframe for filtered open triangles for the current network.
  open_triangles[[i]] <- data.frame()
  
  # Loop over each row (triangle) in the dataframe.
  for(j in 1:dim(df2)[1]){
    # Extract the three vertices.
    v1 <- df2[j, 1]
    v2 <- df2[j, 2]
    v3 <- df2[j, 3]
    
    # Initialize indicators for adversary connections.
    # We check for adversary edges on the v1-v2 and v1-v3 pairs.
    id_1_2 <- 0  # For edge between v1 and v2 in the adversary network.
    id_3_2 <- 0  # For edge between v1 and v3 in the adversary network.
    
    # If v1 and v2 are connected in the adversary network, set the indicator to 1.
    if(are.connected(g_adv, v1, v2)){
      id_1_2 <- 1
    }
    # If v1 and v3 are connected in the adversary network, set the indicator to 1.
    if(are.connected(g_adv, v1, v3)){
      id_3_2 <- 1
    }
    
    # Initialize a temporary dataframe.
    df_1 <- data.frame()
    
    # If neither edge (v1-v2 nor v1-v3) exists in the adversary network,
    # then keep the triangle as an "open triangle".
    if(id_1_2 == 0 & id_3_2 == 0){
      df_1 <- df2[j, ]
      open_triangles[[i]] <- rbind(open_triangles[[i]], df_1)
    }
  }
  
  # Rename the columns of the open triangles dataframe.
  names(open_triangles[[i]]) <- c("v1", "v2", "v3")
  
  # --- Merge Open Triangles with Respondent-Level Data ---
  # Merge open triangles with selected respondent data using vertex v1.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df, 
                               by.x = "v1", by.y = "Respondent4", all.x = TRUE)
  # Rename the depression column for vertex v1.
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v1 = depressed_w4)
  
  # Merge respondent data for vertex v2.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df, 
                               by.x = "v2", by.y = "Respondent4", all.x = TRUE)
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v2 = depressed_w4)
  
  # Merge respondent data for vertex v3.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df, 
                               by.x = "v3", by.y = "Respondent4", all.x = TRUE)
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v3 = depressed_w4)
}

# Save the open triangles for wave 4 in x4_open.
x4_open <- open_triangles

# Create a unique set of records from all open triangles (using vertex v1 and its depression status).
y4_open <- unique(bind_rows(x4_open)[c("v1", "depressed_v1")])

# Standardize the column names for the open triangles data.
colnames(y4_open) <- c("v1", "depressed_w4")

# --- PART 4: Prepare Other Triangle Sets for Intersection ---
# The code assumes the existence of two other dataframes:
#   triangles_out_fr: nodes from fully closed (friends-only) triangles.
#   triangles_out_adv: nodes from triangles that contain adversary edges.
# These are stored in y_4 and z_4 respectively.
triangles_out_fr <- y_4_c   # Fully friends triangles for wave 4.
triangles_out_adv <- z_4_c # Triangles with adversary edges for wave 4.

# Standardize the column names for these dataframes.
colnames(triangles_out_fr) <- c("v1", "depressed_w4")
colnames(triangles_out_adv) <- c("v1", "depressed_w4")
#colnames(y4_open) <- c("v1", "depressed_w4")

# Convert the 'depressed_w4' column to integer in each dataframe to ensure data type consistency.
triangles_out_fr <- triangles_out_fr %>% mutate(depressed_w4 = as.integer(depressed_w4))
triangles_out_adv <- triangles_out_adv %>% mutate(depressed_w4 = as.integer(depressed_w4))
y4_open <- y4_open %>% mutate(depressed_w4 = as.integer(depressed_w4))

# --- PART 5: Determine Common and Unique Elements Across Triangle Sets ---

 --- PART 5: Determine Common and Unique Elements Across Triangle Sets ---

# Compute intersections between the different triangle sets.
common_elements_1 <- intersect(triangles_out_fr, y4_open)
#common_elements_2 <- intersect(triangles_out_adv, y1_open)
#common_elements_3 <- intersect(triangles_out_fr, triangles_out_adv)

# Compute set differences:#
#   - triangles_out_open: elements in y4_open not found in triangles_out_fr.
triangles_out_fr <- setdiff(triangles_out_fr, triangles_out_adv)
#   - Replace triangles_out_adv with those elements in y4_open not in triangles_out_adv.
x <- setdiff(y4_open, common_elements_1)
triangles_out_adv <- setdiff(x, triangles_out_adv)
#triangles_out_adv <- setdiff(triangles_out_adv , common_elements_2)
# --- PART 6: Merge with Respondent-Level Data and Filter by Gender ---

# Remove the common elements from both sets.
#triangles_out_fr <- setdiff(triangles_out_fr, common_elements_3)
#triangles_out_adv <- setdiff(triangles_out_adv, common_elements_3)

# Merge the fully friends triangles (after set differences) with the individuals dataset for wave 4.
joined_data <- left_join(triangles_out_fr, individuals_wave4_fr, by = c("v1" = "Respondent4"))
# Merge the adversary triangles similarly.
joined_data_1 <- left_join(triangles_out_adv, individuals_wave4_fr, by = c("v1" = "Respondent4"))

# Filter the merged datasets to include only rows where gender equals 1.
# (Here gender==1 is used; adjust as needed if gender coding differs.)
filtered_data <- joined_data %>% filter(gender == 0)
filtered_data_1 <- joined_data_1 %>% filter(gender == 0)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status columns from the filtered data.
# Note: After merging, the depression column may have a suffix (e.g., .x).
depressed_fr <- as.numeric(filtered_data$depressed_w4.x)
depressed_adv <- as.numeric(filtered_data_1$depressed_w4.x)

# Compute sample sizes for each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of respondents without depression (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table:
#   - Rows: FR (fully friends triangles) and ADV (triangles with adversary edges)
#   - Columns: Depressed=1 and Depressed=0
contingency_table <- matrix(c(count_fr_1, count_fr_0,
                              count_adv_1, count_adv_0),
                            nrow = 2, byrow = TRUE)
rownames(contingency_table) <- c("FR", "ADV")
colnames(contingency_table) <- c("Depressed=1", "Depressed=0")

# Print the contingency table.
contingency_table

# --- PART 8: Statistical Testing ---

# Perform a chi-square test on the contingency table.
chisq_result <- chisq.test(contingency_table)
print(chisq_result)

# Perform Fisher's exact test (useful when sample sizes are small).
print(fisher.test(contingency_table))




# Filter the merged datasets to include only rows where gender equals 1.
# (Here gender==1 is used; adjust as needed if gender coding differs.)
filtered_data <- joined_data %>% filter(gender == 1)
filtered_data_1 <- joined_data_1 %>% filter(gender == 1)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status columns from the filtered data.
# Note: After merging, the depression column may have a suffix (e.g., .x).
depressed_fr <- as.numeric(filtered_data$depressed_w4.x)
depressed_adv <- as.numeric(filtered_data_1$depressed_w4.x)

# Compute sample sizes for each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Count the number of respondents without depression (coded as 0) in each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table:
#   - Rows: FR (fully friends triangles) and ADV (triangles with adversary edges)
#   - Columns: Depressed=1 and Depressed=0
contingency_table <- matrix(c(count_fr_1, count_fr_0,
                              count_adv_1, count_adv_0),
                            nrow = 2, byrow = TRUE)
rownames(contingency_table) <- c("FR", "ADV")
colnames(contingency_table) <- c("Depressed=1", "Depressed=0")

# Print the contingency table.
contingency_table

# --- PART 8: Statistical Testing ---

# Perform a chi-square test on the contingency table.
chisq_result <- chisq.test(contingency_table)
print(chisq_result)

# Perform Fisher's exact test (useful when sample sizes are small).
print(fisher.test(contingency_table))