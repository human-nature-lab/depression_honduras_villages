# --------------------------------------------------------------------------------
# This script processes a set of wave 3 networks to extract “open triangles”
# from aggregated networks that combine friends and adversary edges. It then
# merges these open triangle data with respondent-level depression data,
# performs intersections and set differences with other triangle sets, and finally
# conducts statistical tests (chi-square, Fisher's exact test, and a t-test)
# to compare depression status between groups.
# --------------------------------------------------------------------------------

# --- PART 1: Aggregate Networks from Two Sources (Friends and Adversaries) for Wave 3 ---

# Initialize an empty list for aggregated networks.
# The list length is set to the same as the number of friends network objects in gr3_fr.
gr3_h_in_aggr <- vector("list", length(gr3_fr))

# Loop through each network index specified in villages_1_3.
for (i in villages_1_3) {
  # Skip index 156 (an exclusion criterion).
  if (i==155 | i == 156) next  
  # Also skip if the adversary network for index i has no edges.
  if (length(E(gr3_adv[[i]])) == 0) next
  
  # Extract the edge list (dataframe) from the friends network at index i.
  edges_fr <- get.data.frame(gr3_fr[[i]], what = "edges")
  # Add a column 'type' with value 1 to indicate that these edges are from the friends network.
  edges_fr['type'] <- 1
  
  # Extract the edge list from the adversary network at index i.
  edges_adv <- get.data.frame(gr3_adv[[i]], what = "edges")
  # Add a column 'type' with value 2 to indicate that these edges are from the adversary network.
  edges_adv['type'] <- 2
  
  # Combine the friends and adversary edge dataframes by row-binding them.
  edges <- rbind(edges_fr, edges_adv)
  
  # Create an aggregated graph from the combined edges.
  # The graph is made undirected and any duplicate edges are collapsed.
  gr3_h_in_aggr[[i]] <- as.undirected(
    graph_from_data_frame(edges, directed = FALSE),
    mode = c("collapse")
  )
}

# Now, gr3_h_in_aggr contains the aggregated networks for wave 3 with edge attribute 'type'
# that distinguishes friends (1) from adversaries (2).

# --- PART 2: Prepare Respondent-Level Data for Wave 3 ---

# Select the required columns from the individuals dataset for wave 3.
# 'Respondent3' identifies the respondent and 'depressed_w3' indicates depression status.
selected_df <- individuals_wave3_fr %>% 
  dplyr::select(Respondent3, depressed_w3)

# --- PART 3: Extract Open Triangles from the Aggregated Networks ---

# Initialize an empty list to store the open triangles for each network.
open_triangles <- list()

# Loop through each network index in villages_1_3.
for (i in villages_1_3) {
  # Skip index 156 and any network where the adversary network has no edges.
  if (i==155 | i == 156) next  
  if (length(E(gr3_adv[[i]])) == 0) next
  
  print(i)  # Print current index for tracking progress.
  
  # Convert the friends network for index i to an undirected graph with collapsed edges.
  g_fr <- as.undirected(gr3_fr[[i]], mode = c("collapse"))
  # Similarly, convert the adversary network for index i.
  g_adv <- as.undirected(gr3_adv[[i]], mode = c("collapse"))
  
  # For the aggregated network, ensure it is undirected with collapse mode.
  G <- as.undirected(gr3_h_in_aggr[[i]], mode = "collapse")
  
  # --- Extracting Open Triangles ---
  # For each vertex v in G, the code examines its neighbors (v1), and then for each v1,
  # retrieves the neighbors (v2). Only those vertices v2 at a shortest-path distance of 2
  # from v are kept. For each such triple (v, v1, vv2), the vertices are sorted to ensure
  # uniqueness. The result is a list of candidate open triangles.
  openTriList <- unique(
    do.call(c, lapply(as_ids(V(G)), function(v) {
      do.call(c, lapply(as_ids(neighbors(G, v)), function(v1) {
        # Get the neighbors of v1 and remove duplicates.
        v2 <- as_ids(neighbors(G, v1))
        v2 <- unique(v2)
        # Filter vertices such that the shortest path from v is exactly 2.
        v2 <- v2[shortest.paths(G, v, v2) == 2]
        if (length(v2) != 0) {
          # For each qualifying vertex, return the sorted vector of vertices forming the triangle.
          lapply(v2, function(vv2) {
            sorted_vertices <- sort(c(v, v1, vv2))
            return(sorted_vertices)
          })
        } else {
          list()  # Return an empty list if none are found.
        }
      }))
    }))
  )
  
  # Convert the list of open triangles into a dataframe.
  # Each row corresponds to an open triangle with three vertices.
  df2 = data.frame(matrix(do.call(rbind, openTriList), ncol = 3, byrow = TRUE))
  
  # Name the columns to represent the three vertices.
  names(df2) <- c("v1", "v2", "v3")
  
  # Initialize an empty dataframe to hold the filtered open triangles for the current network.
  open_triangles[[i]] <- data.frame()
  
  # Loop over each row (triangle) in the dataframe.
  for (j in 1:dim(df2)[1]) {
    # Extract the vertices of the current triangle.
    v1 <- df2[j, 1]
    v2 <- df2[j, 2]
    v3 <- df2[j, 3]
    
    # Initialize indicators for adversary edges between specific vertex pairs.
    # We check for adversary connections on the v1-v2 and v1-v3 edges.
    id_1_2 <- 0  # For the edge between v1 and v2 in the adversary network.
    id_3_2 <- 0  # For the edge between v1 and v3 in the adversary network.
    
    # Check if there is an adversary connection between v1 and v2.
    if (are.connected(g_adv, v1, v2)) {
      id_1_2 <- 1
    }
    # Check if there is an adversary connection between v1 and v3.
    if (are.connected(g_adv, v1, v3)) {
      id_3_2 <- 1
    }
    
    # Initialize a temporary dataframe.
    df_1 <- data.frame()
    
    # If neither the v1-v2 nor the v1-v3 edge exists in the adversary network,
    # then consider the triangle as an “open triangle” and keep it.
    if (id_1_2 == 0 & id_3_2 == 0) {
      df_1 <- df2[j, ]
      open_triangles[[i]] <- rbind(open_triangles[[i]], df_1)
    }
  }
  
  # Rename the columns for the open triangles dataframe.
  names(open_triangles[[i]]) <- c("v1", "v2", "v3")
  
  # --- Merge Open Triangles with Respondent-Level Data ---
  # Merge using vertex v1: join open triangles with selected_df using Respondent3 as key.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df,
                               by.x = "v1", by.y = "Respondent3", all.x = TRUE)
  # Rename the depression status column for v1.
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v1 = depressed_w3)
  
  # Merge using vertex v2.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df,
                               by.x = "v2", by.y = "Respondent3", all.x = TRUE)
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v2 = depressed_w3)
  
  # Merge using vertex v3.
  open_triangles[[i]] <- merge(open_triangles[[i]], selected_df,
                               by.x = "v3", by.y = "Respondent3", all.x = TRUE)
  open_triangles[[i]] <- open_triangles[[i]] %>% rename(depressed_v3 = depressed_w3)
}

# Save the open triangles for wave 3 in x3_open.
x3_open <- open_triangles

# Create a unique set of records (by v1 and its depression status) from all open triangles.
y3_open <- unique(bind_rows(x3_open)[c("v1", "depressed_v1")])

# Standardize the column names for the open triangles data.
colnames(y3_open) <- c("v1", "depressed_w3")

# --- PART 4: Prepare Other Triangle Sets for Intersection ---

# Here we assume that two other dataframes exist: triangles_out_fr and triangles_out_adv,
# which represent nodes extracted from fully closed (friends-only) triangles and triangles with
# adversary edges, respectively.

triangles_out_fr <- y_3_c   # Fully friends triangles for wave 3.
triangles_out_adv <- z_3_c   # Triangles with adversary edges for wave 3.

# Standardize column names for consistency.
colnames(triangles_out_fr) <- c("v1", "depressed_w3")
colnames(triangles_out_adv) <- c("v1", "depressed_w3")

# Convert the 'depressed_w3' column to integer in both data frames (to ensure consistent data types).
triangles_out_fr <- triangles_out_fr %>% mutate(depressed_w3 = as.integer(depressed_w3))
triangles_out_adv <- triangles_out_adv %>% mutate(depressed_w3 = as.integer(depressed_w3))
y3_open <- y3_open %>% mutate(depressed_w3 = as.integer(depressed_w3))

# --- PART 5: Determine Common and Unique Elements Across Triangle Sets ---

# Compute intersections between the different triangle sets.
common_elements_1 <- intersect(triangles_out_fr, y3_open)
#common_elements_2 <- intersect(triangles_out_adv, y1_open)
#common_elements_3 <- intersect(triangles_out_fr, triangles_out_adv)

# Compute set differences:#
#   - triangles_out_open: elements in y4_open not found in triangles_out_fr.
triangles_out_fr <- setdiff(triangles_out_fr, triangles_out_adv)
#   - Replace triangles_out_adv with those elements in y4_open not in triangles_out_adv.
x <- setdiff(y3_open, common_elements_1)
triangles_out_adv <- setdiff(x, triangles_out_adv)
#triangles_out_adv <- setdiff(triangles_out_adv , common_elements_2)


# Merge the fully friends triangles (after set difference) with the individuals dataset for wave 3.
joined_data <- left_join(triangles_out_fr, individuals_wave3_fr, by = c("v1" = "Respondent3"))
# Merge the adversary triangles similarly.
joined_data_1 <- left_join(triangles_out_adv, individuals_wave3_fr, by = c("v1" = "Respondent3"))

# Filter the merged data to include only those respondents with gender == 0.
filtered_data <- joined_data %>% filter(gender == 0)
filtered_data_1 <- joined_data_1 %>% filter(gender == 0)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status columns from both filtered datasets.
# Note: The column names may have a suffix (e.g., .x) resulting from the merge.
depressed_fr <- as.numeric(filtered_data$depressed_w3.x)
depressed_adv <- as.numeric(filtered_data_1$depressed_w3.x)

# Determine the sample sizes (number of respondents) in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Calculate the number of respondents without depression (coded as 0) for each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table:
# - Rows: FR (fully friends triangles) and ADV (triangles with adversary edges)
# - Columns: Depressed=1 and Depressed=0
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
# X-squared = 2.115, df = 1, p-value = 0.1459

# Also perform Fisher's exact test, which is particularly useful for small sample sizes.
# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 0.1426
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.7192352 1.0490495
# sample estimates: odds ratio = 0.8683909

# Filter the merged data to include only those respondents with gender == 0.
filtered_data <- joined_data %>% filter(gender == 1)
filtered_data_1 <- joined_data_1 %>% filter(gender == 1)

# --- PART 7: Prepare Data for Statistical Testing ---

# Extract the depression status columns from both filtered datasets.
# Note: The column names may have a suffix (e.g., .x) resulting from the merge.
depressed_fr <- as.numeric(filtered_data$depressed_w3.x)
depressed_adv <- as.numeric(filtered_data_1$depressed_w3.x)

# Determine the sample sizes (number of respondents) in each group.
n_fr <- length(depressed_fr)
n_adv <- length(depressed_adv)

# Count the number of respondents with depression (coded as 1) in each group.
count_fr_1 <- sum(na.omit(depressed_fr))
count_adv_1 <- sum(na.omit(depressed_adv))

# Calculate the number of respondents without depression (coded as 0) for each group.
count_fr_0 <- n_fr - count_fr_1
count_adv_0 <- n_adv - count_adv_1

# Construct a 2x2 contingency table:
# - Rows: FR (fully friends triangles) and ADV (triangles with adversary edges)
# - Columns: Depressed=1 and Depressed=0
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
# X-squared = 0.35671, df = 1, p-value = 0.5503

# Also perform Fisher's exact test, which is particularly useful for small sample sizes.
# Fisher's Exact Test for Count Data
# data: contingency_table
# p-value = 0.4993
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval: 0.6387502 1.2595786
# sample estimates: odds ratio = 0.8929676