# =======================================================================
# VARIABLE DICTIONARY
#
# Dosage and Treatment Variables:
# -----------------------------------------------------------------------
# dosage_data         : Data frame loaded from "outcomes1.csv" that contains
#                       the following columns:
#                         - village_code_w1: Unique identifier for a village in Wave 1.
#                         - friend_treatment: Indicator (0 or 1) whether the respondent
#                                             received friend treatment.
#                         - dosage: Numeric value representing the treatment dosage.
#
# dosage_list         : A list where each element (for indices 1 to 176, except 156)
#                       holds the dosage value from dosage_data for that index.
#
# dosage_levels       : A numeric vector specifying the possible dosage levels:
#                       c(0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.75, 1).
#
# arm_index           : A nested list that maps each treatment group (first-level,
#                       corresponding to friend_treatment values 0 and 1) and each
#                       dosage level (second-level, corresponding to the values in
#                       dosage_levels) to the associated village_code_w1.
#
# =======================================================================
# Individual-Level Data (Survey Responses):
# -----------------------------------------------------------------------
# individuals_wave1_raw: Raw data loaded from "individuals_wave1.csv" containing
#                        survey responses from Wave 1.
#
# individuals_wave1    : Processed Wave 1 data frame after filtering and column
#                        selection. It originally contains columns from the raw file
#                        and is later renamed to:
#                          - Respondent1: Unique respondent ID.
#                          - Treatment: Treatment assignment indicator.
#                          - Household1: Household identifier (building_id_w1).
#                          - gender1: Respondent's gender.
#                          - marital_status1: Marital status (as text, later recoded).
#                          - age1: Age at the time of survey.
#                          - village1: Village code.
#                        Additional variables merged later include depression,
#                        education (e.g., b0100, b0600), food insecurity (FI), individual
#                        wealth (d0700), indigenous status (indigenous.binary), and village
#                        characteristics.
#
# individuals_wave3_raw: Raw survey data for Wave 3 loaded from "individuals_wave3.csv".
#
# individuals_wave3    : Processed Wave 3 data frame (columns renamed to):
#                          - Respondent3, Treatment, Household3, gender, marital_status,
#                            age, village.
#                        It is later merged with additional characteristics such as:
#                          - Depression (depressed_w3 computed from recoding c0300 and c0400),
#                          - Education (b0100, b0600),
#                          - Indigenous status,
#                          - Food insecurity (FI) and individual wealth (d0700).
#
# individuals_wave4_raw: Raw survey data for Wave 4 loaded from "individuals_wave4.csv".
#
# individuals_wave4    : Processed Wave 4 data frame (columns renamed to):
#                          - Respondent4, Household4, Household3, gender, marital_status,
#                            age, village.
#                        It is further merged with psychological characteristics (e.g.,
#                        depressed_w4, gad2_binary, ptsd_binary, c0160_binary), education,
#                        food insecurity, individual wealth, and indigenous status.
#
# =======================================================================
# Network (Friendship) Data:
# -----------------------------------------------------------------------
# connections_w1_fr     : Raw friendship network data for Wave 1 loaded from "health_wave1.csv".
# connections_w3_fr     : Raw friendship network data for Wave 3 loaded from "health_wave3.csv".
# connections_w4_fr     : Raw friendship network data for Wave 4 loaded from "health_wave4.csv".
#
# connections_individuals1 : Filtered and processed friendship connections for Wave 1,
#                            merged with individual-level data.
# connections_individuals3 : Similar processed connections for Wave 3.
# connections_individuals4 : Similar processed connections for Wave 4.
#
# villages_1_3         : Numeric vector of unique village codes extracted from
#                        connections_individuals3 (common set used for Waves 1 & 3).
#
# villages_4           : Numeric vector of unique village codes extracted from
#                        Wave 4 connection data.
#
# village_connection1   : A list where each element contains the subset of Wave 1
#                        connections for a given village.
#
# village_connection3   : A list for Wave 3 connections by village.
#
# edges1                : List of edge lists (matrices with two columns: Respondent1 and Alter)
#                        for Wave 1, organized by village.
#
# edges3                : Similar list of edge lists for Wave 3.
#
# gr1                   : List of igraph objects (network graphs) constructed from edges1
#                        for Wave 1.
#
# gr3                   : List of igraph objects constructed from edges3 for Wave 3.
#
# gr1_fr, gr3_fr        : Copies of gr1 and gr3 (used for further friend-related network analysis).
#
# For Wave 4:
#   connections_individuals4_fr: Processed friendship network data for Wave 4 (after filtering).
#   gr4                   : List of igraph objects for Wave 4 created from the edge lists.
#   gr4_fr                : Copy of gr4 for further friend-related network analysis.
#
# =======================================================================
# Additional Merged Datasets and Attributes:
# -----------------------------------------------------------------------
# psycho_characteristics  : Data frame(s) containing depression. Loaded from RData objects (e.g., resp_w1_parental_depr,
#                           resp_w3_parental_depr, resp_w4_parental_depr).
#
# education             : Data frame loaded from "hondras_demog_w1_to_w4_marios_2024-02-13.csv"
#                         that contains education information. The variable b0100 (education level)
#                         is recoded to numeric values (e.g., "1st grade" becomes 1, "More than secondary"
#                         becomes 13). Also includes b0200 and b0600 (religion), which are recoded.
#
# indiv_char            : Data frame of additional individual characteristics (e.g., d0100, d0200, d0700,
#                         e0300, c1100, etc.) loaded from CSV files such as "individuals_1.csv".
#
# households_wave1      : Household-level data for Wave 1 loaded from "household_wave1.csv"
#                         with columns:
#                           - building_id, household_wealth_index_w1, building_latitude,
#                             building_longitude.
#                         Later renamed to: Household, Wealth1, Latitude, Longitude.
#
# Similarly, households_wave4 (for Wave 4) is loaded from "honduras_households_WAVE4_v1.csv"
# and contains Household and Wealth4.
#
# =======================================================================
# Computed Graph and Node Attributes:
# -----------------------------------------------------------------------
# For each graph (across Waves 1, 3, and 4), the following are computed and/or assigned:
#
# Node-specific attributes:
#   depressed_w1, depressed_w3, depressed_w4  : Binary indicators of depression,
#                                                 computed from recoded survey items.
#
#   wealth (Wealth1, Wealth3, Wealth4)           : Individual-level wealth from household data.
#
#   FI                                          : Food insecurity measure (binary after recoding).
#
#   marital_status                              : Re-coded marital status (typically 1 for partnered,
#                                                 0 for unpartnered).
#
#   age, gender, b0100, indigenous.binary, b0600, d0700:
#         Additional individual-level attributes (e.g., education level, religion,
#         indigenous status, individual wealth).
#
# Neighbor-based attributes (computed on the network graphs):
#   depressed_w?_neigh          : Count of neighboring nodes with a depression indicator of 1.
#   not_depressed_w?_neigh      : Count of neighboring nodes with a depression indicator of 0.
#   intransitivity_values       : For each vertex, the proportion of open triads computed
#                                 using custom functions (calculate_social_intransitivity and
#                                 intransitivity_index).
#
#   For other attributes (Wealth, FI, marital status, age, b0100, indigenous.binary, b0600, d0700):
#     - *_neigh_mean: Mean value of the attribute among neighbors (often filtered by same gender).
#     - *_diff      : Difference between the vertex's value and the neighbors' mean.
#
# Data frames combining vertex attributes and network-level summary metrics:
#   dataframe_attributes_fr_1: Contains vertex attributes from Wave 1 graphs along with:
#                              - network_size (number of vertices),
#                              - network_density,
#                              - number_of_edges,
#                              - avg_shortest_path.
#
#   dataframe_attributes_fr_3: Similar for Wave 3 graphs.
#
#   dataframe_attributes_fr_4: Similar for Wave 4 graphs.
#
# =======================================================================
# Temporary and Loop Variables:
# -----------------------------------------------------------------------
# In loops, additional temporary variables are used:
#
#   node_data_1, node_data_3, node_data_4: Subsets of individual-level data corresponding
#                                          to vertices in the graph (used to assign node attributes).
#
#   vertex_name: The name (or ID) of the current vertex being processed.
#
#   v, vertex_id: Loop counters/iterators over vertices in a graph.
#
#   intransitivity_values: Numeric vector storing the intransitivity measure for each node
#                          in a graph (computed using the intransitivity_index function).
#
#   neighbors_v, neigh_seq, neighbors_same_gender: Temporary variables for neighbor nodes and
#                                                   their filtered subsets.
#
#   node_degree: Degree (number of connections) of a node.
#
#   network_size, network_density, number_of_edges, avg_shortest_path:
#         Graph-level metrics computed using igraph functions (vcount, edge_density, ecount,
#         mean_distance).
#
# =======================================================================
# End of Variable Dictionary
# =======================================================================




# Adversarial



################################################################################
# Dictionary of Variables and Objects
#
# dosage_table:
#   - Description: Data frame loaded from "outcomes1.csv" containing dosage outcomes.
#   - Key Columns: 
#       * village_code_w1: Village identifier for Wave 1.
#       * friend_treatment: Treatment indicator (0 or 1).
#       * dosage: Dosage level assigned.
#
# dosage:
#   - Description: A list storing the dosage value for each village (indexed by village number),
#                  excluding village index 156.
#
# arm_index:
#   - Description: A nested list used to group village codes by treatment and dosage level.
#   - Structure: arm_index[[1]] for treatment = 0; arm_index[[2]] for treatment = 1.
#
# all_dosages:
#   - Description: A vector of all possible dosage levels (0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.75, 1).
#
# individuals_wave1:
#   - Description: Data frame of individual-level respondent data for Wave 1.
#   - Filtering: Only includes rows where 'complete' == 1 and data_source_w1 == 1.
#   - Selected Variables: respondent_master_id, resp_target, building_id_w1, gender, 
#                         marital_name, age_at_survey, village_code_w1.
#   - Renamed Columns: Respondent1, Treatment, Household1, gender1, marital_status1, age1, village1.
#
# individuals_wave3:
#   - Description: Data frame of individual-level respondent data for Wave 3.
#   - Filtering: Only includes rows where 'complete' == 1 and data_source_w3 == 1.
#   - Selected Variables: respondent_master_id, resp_target, building_id_w3, gender,
#                         marital_name, age_at_survey, village_code_w3.
#   - Renamed Columns: Respondent3, Treatment, Household3, gender, marital_status, age, village.
#
# connections_individuals1_adv:
#   - Description: Data frame containing adversarial ("not_get_along") relationships from Wave 1.
#   - Filtering: Only rows with relationship == "not_get_along" and alter_source == 1.
#   - Selected Variables (after subsetting): ego, alter, village_code_w1.
#   - Renamed Columns: Respondent1, Alter, Village1.
#
# connections_individuals3_adv:
#   - Description: Data frame containing adversarial ("not_get_along") relationships from Wave 3.
#   - Filtering: Only rows with relationship == "not_get_along" and alter_source == 1.
#   - Selected Variables (after subsetting): ego, alter, village_code_w3.
#   - Renamed Columns: Respondent3, Alter, Village3.
#
# connections_individuals1 & connections_individuals3:
#   - Description: General variables storing the filtered adversarial connections for Waves 1 and 3.
#
# Merging Steps (Step 4):
#   - The individual-level data (for both waves) is merged with the corresponding connections
#     data so that each connection (ego–alter pair) has the additional attributes (e.g., age,
#     gender, marital status) from the individuals data.
#
# gr1_adv:
#   - Description: A list of igraph objects representing adversarial networks for each village
#                  from Wave 1.
#
# gr3_adv:
#   - Description: A list of igraph objects representing adversarial networks for each village
#                  from Wave 3.
#
# network_data_list:
#   - Description: A temporary list to store per-village network metric data frames.
#
# all_networks_df_1:
#   - Description: A combined data frame with computed network metrics (e.g., degree, average
#                  neighbor degree, edge density) for Wave 1 adversarial networks.
#
# all_networks_df_3:
#   - Description: A combined data frame with computed network metrics for Wave 3 adversarial networks.
#
# individuals_wave4:
#   - Description: Data frame of individual-level respondent data for Wave 4.
#   - Filtering: Only includes rows with complete responses (complete == 1) and data_source_w4 == 1.
#   - Selected Variables: respondent_master_id, building_id_w4, building_id_w3, gender,
#                         marital_name, age_at_survey, village_code_w4.
#   - Renamed Columns: Respondent4, Household4, Household3, gender, marital_status, age, village.
#
# connections_individuals4_adv:
#   - Description: Data frame containing adversarial ("not_get_along") relationships for Wave 4.
#   - Filtering: Only rows with relationship == "not_get_along" and alter_source == 1.
#   - Selected Variables: ego, alter, village_code_w4.
#   - Renamed Columns: Respondent4, Alter, Village4.
#
# connections_individuals4:
#   - Description: General variable storing the filtered adversarial connections for Wave 4.
#
# gr4_adv:
#   - Description: A list of igraph objects representing adversarial networks for each village
#                  from Wave 4.
#
# all_networks_df_4:
#   - Description: A combined data frame with computed network metrics for Wave 4 adversarial networks.
#
# Common Respondents (Step 6):
#   - Description: Identifies respondents present in both Wave 1 and Wave 3 (by matching
#                  village codes) and filters the connections to include only those common
#                  respondents.
#
# Computed Network Metrics (Step 7):
#   - Variables computed include:
#       * node_degree: Degree (number of adversarial ties) for each node.
#       * Avg_Neighbor_Degree: Average degree of neighboring nodes.
#       * Adv_density: Edge density of the network.
#
################################################################################
