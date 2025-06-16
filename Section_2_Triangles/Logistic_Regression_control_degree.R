# Load required libraries
library(igraph)
library(dplyr)
library(lme4)

# The following objects are assumed to be in your workspace:
# gr1_fr          : list of friends network graphs (one per village)
# gr1_adv         : list of adversary network graphs (one per village)
# individuals_wave1_fr : data frame with at least two columns:
#                        - Respondent1 (the individual’s ID/name)
#                        - depressed_w1 (depression status; e.g., 0/1)
# villages_1_3    : vector of indices (villages) to process
# vertices_degree  : we control with friends

# Initialize a list to hold per-village results.
results_list <- list()

# Loop through each village/network index.
for(i in villages_1_3) {
  
  print(i)
  
  # Skip network 156 or any village with no adversary edges (per your earlier exclusions)
  #if(i == 156) next
  if(length(E(gr1_adv[[i]])) == 0) next
  
  # Create undirected versions of the friends and adversary networks
  # (Using "collapse" mode to combine any duplicate edges)
  g_fr  <- as.undirected(gr1_fr[[i]], mode = "collapse")
  g_adv <- as.undirected(gr1_adv[[i]], mode = "collapse")
  
  # Get the names (IDs) of all vertices in the friends network.
  # (Assumes that vertices have a 'name' attribute.)
  vertices <- V(g_fr)$name
  vertices_degree <- degree(g_fr)
  
  # Initialize a data frame with one row per individual and three indicator columns.
  df_village <- data.frame(vertex = vertices,
                           vertices_degree,
                           closed_triangle = 0,  # Indicator for two friends that are friends
                           open_triangle = 0,    # Indicator for two friends that are not connected
                           enemy_triangle = 0,   # Indicator for two friends that are connected as enemies
                           stringsAsFactors = FALSE)
  
  # Process each individual in the current village.
  for(v in vertices) {
    # Get the friend neighbors of v in the friends network
    nbrs <- as_ids(neighbors(g_fr, v))
    
    # If the individual has fewer than 2 friends, no triangle can form.
    if(length(nbrs) < 2) next
    
    # Get all unique pairs of friend neighbors
    # Each pair is a candidate for forming a triangle with v.
    pairs <- combn(nbrs, 2, simplify = FALSE)
    
    # Check each pair of neighbors
    for(pair in pairs) {
      n1 <- pair[1]
      n2 <- pair[2]
      
      # 1. Closed triangle of friends: the two neighbors are friends (in g_fr)
      if(are.connected(g_fr, n1, n2)) {
        df_village[df_village$vertex == v, "closed_triangle"] <- 1
      } else {
        # 2. Open triangle: the two neighbors are not directly connected by a friend edge.
        df_village[df_village$vertex == v, "open_triangle"] <- 1
      }
      
      # 3. Closed triangle with enemy tie: the two neighbors are connected in the adversary network.
      if(are.connected(g_adv, n1, n2)) {
        df_village[df_village$vertex == v, "enemy_triangle"] <- 1
      }
    }
  }
  
  # Merge with the individuals data to get depression status.
  # Here we assume individuals_wave1_fr has columns "Respondent1" and "depressed_w1".
  df_village <- merge(df_village, 
                      individuals_wave1_fr %>% select(Respondent1, depressed_w1),
                      by.x = "vertex", by.y = "Respondent1", 
                      all.x = TRUE)
  # Optionally, rename depressed_w1 to "depressed" for clarity.
  df_village <- df_village %>% rename(depressed = depressed_w1)
  
  # Store the village-level results.
  results_list[[i]] <- df_village
}

# Combine the results from all villages into one data frame.
final_results <- bind_rows(results_list) %>% distinct()

library(dplyr)

# --- Step 1: Combine results from all villages ---
final_results <- bind_rows(results_list) %>% distinct()

# --- Step 2: Add a "none" indicator (individuals with no triangle type) ---
final_results <- final_results %>%
  mutate(none = if_else(closed_triangle == 0 & open_triangle == 0 & enemy_triangle == 0, 1, 0))

# --- Step 3: Create a single categorical variable for triangle type ---
# Priority: enemy > closed > open > none
final_results <- final_results %>%
  mutate(triangle_type = case_when(
    enemy_triangle == 1 ~ "enemy",
    closed_triangle == 1 ~ "closed",
    open_triangle   == 1 ~ "open",
    none == 1         ~ "none"
  ))

# --- Step 4: Set "enemy" as the reference level ---
final_results$triangle_type <- factor(final_results$triangle_type,
                                      levels = c("enemy", "closed", "open", "none"))

# --- Step 5: Run the logistic regression ---
# Here, depressed is assumed to be a binary variable (0/1) where 1 indicates depression.
model <- glm(depressed ~ open_triangle + enemy_triangle + none + vertices_degree, data = final_results_1, family = binomial)

# View the model summary; negative estimates for closed or open (relative to enemy)
# indicate lower log-odds (and odds) of depression.
summary(model)


model_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = final_results_1,
  family = binomial(link = "logit"),
  nAGQ   = 0)

## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_null, model)

##— R^2 computation

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model)        # from performance



#save as 
final_results_1 <- final_results


### WAVE 3

# The following objects are assumed to be in your workspace:
# gr3_fr          : list of friends network graphs (one per village)
# gr3_adv         : list of adversary network graphs (one per village)
# individuals_wave3_fr : data frame with at least two columns:
#                        - Respondent3 (the individual’s ID/name)
#                        - depressed_w3 (depression status; e.g., 0/1)
# villages_1_3    : vector of indices (villages) to process
# vertices_degree  : we control with friends

# Initialize a list to hold per-village results.
results_list <- list()

# Loop through each village/network index.
for(i in villages_1_3) {
  
  print(i)
  
  # Skip network 156 or any village with no adversary edges (per your earlier exclusions)
  if(i == 155 | i == 156) next
  #if(length(E(gr3_adv[[i]])) == 0) next
  
  # Create undirected versions of the friends and adversary networks
  # (Using "collapse" mode to combine any duplicate edges)
  g_fr  <- as.undirected(gr3_fr[[i]], mode = "collapse")
  g_adv <- as.undirected(gr3_adv[[i]], mode = "collapse")
  
  # Get the names (IDs) of all vertices in the friends network.
  # (Assumes that vertices have a 'name' attribute.)
  vertices <- V(g_fr)$name
  vertices_degree <- degree(g_fr)
  
  # Initialize a data frame with one row per individual and three indicator columns.
  df_village <- data.frame(vertex = vertices,
                           vertices_degree,
                           closed_triangle = 0,  # Indicator for two friends that are friends
                           open_triangle = 0,    # Indicator for two friends that are not connected
                           enemy_triangle = 0,   # Indicator for two friends that are connected as enemies
                           stringsAsFactors = FALSE)
  
  # Process each individual in the current village.
  for(v in vertices) {
    # Get the friend neighbors of v in the friends network
    nbrs <- as_ids(neighbors(g_fr, v))
    
    # If the individual has fewer than 2 friends, no triangle can form.
    if(length(nbrs) < 2) next
    
    # Get all unique pairs of friend neighbors
    # Each pair is a candidate for forming a triangle with v.
    pairs <- combn(nbrs, 2, simplify = FALSE)
    
    # Check each pair of neighbors
    for(pair in pairs) {
      n1 <- pair[1]
      n2 <- pair[2]
      
      # 1. Closed triangle of friends: the two neighbors are friends (in g_fr)
      if(are.connected(g_fr, n1, n2)) {
        df_village[df_village$vertex == v, "closed_triangle"] <- 1
      } else {
        # 2. Open triangle: the two neighbors are not directly connected by a friend edge.
        df_village[df_village$vertex == v, "open_triangle"] <- 1
      }
      
      # 3. Closed triangle with enemy tie: the two neighbors are connected in the adversary network.
      if(are.connected(g_adv, n1, n2)) {
        df_village[df_village$vertex == v, "enemy_triangle"] <- 1
      }
    }
  }
  
  # Merge with the individuals data to get depression status.
  # Here we assume individuals_wave1_fr has columns "Respondent3" and "depressed_w3".
  df_village <- merge(df_village, 
                      individuals_wave3_fr %>% select(Respondent3, depressed_w3),
                      by.x = "vertex", by.y = "Respondent3", 
                      all.x = TRUE)
  # Optionally, rename depressed_w1 to "depressed" for clarity.
  df_village <- df_village %>% rename(depressed = depressed_w3)
  
  # Store the village-level results.
  results_list[[i]] <- df_village
}

# Combine the results from all villages into one data frame.
final_results <- bind_rows(results_list) %>% distinct()

library(dplyr)

# --- Step 1: Combine results from all villages ---
final_results <- bind_rows(results_list) %>% distinct()

# --- Step 2: Add a "none" indicator (individuals with no triangle type) ---
final_results <- final_results %>%
  mutate(none = if_else(closed_triangle == 0 & open_triangle == 0 & enemy_triangle == 0, 1, 0))

# --- Step 3: Create a single categorical variable for triangle type ---
# Priority: enemy > closed > open > none
final_results <- final_results %>%
  mutate(triangle_type = case_when(
    enemy_triangle == 1 ~ "enemy",
    closed_triangle == 1 ~ "closed",
    open_triangle   == 1 ~ "open",
    none == 1         ~ "none"
  ))

# --- Step 4: Set "enemy" as the reference level ---
final_results$triangle_type <- factor(final_results$triangle_type,
                                      levels = c("enemy", "closed", "open", "none"))

# --- Step 5: Run the logistic regression ---
# Here, depressed is assumed to be a binary variable (0/1) where 1 indicates depression.
model <- glm(depressed ~ open_triangle + enemy_triangle + none + vertices_degree, data = final_results_3, family = binomial)

# View the model summary; negative estimates for closed or open (relative to enemy)
# indicate lower log-odds (and odds) of depression.
summary(model)



model_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = final_results_3,
  family = binomial(link = "logit"),
  nAGQ   = 0)

## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_null, model)

##— R^2 computation

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model)        # from performance


#save as 
final_results_3 <- final_results


## WAVE 4

# The following objects are assumed to be in your workspace:
# gr4_fr          : list of friends network graphs (one per village)
# gr4_adv         : list of adversary network graphs (one per village)
# individuals_wave4_fr : data frame with at least two columns:
#                        - Respondent4 (the individual’s ID/name)
#                        - depressed_w4 (depression status; e.g., 0/1)
# villages_4    : vector of indices (villages) to process
# vertices_degree  : we control with friends

# Initialize a list to hold per-village results.
results_list <- list()

# Loop through each village/network index.
for(i in villages_4) {
  
  print(i)
  
  # Skip network 156 or any village with no adversary edges (per your earlier exclusions)
  #if(i == 156) next
  #if(length(E(gr4_adv[[i]])) == 0) next
  
  # Create undirected versions of the friends and adversary networks
  # (Using "collapse" mode to combine any duplicate edges)
  g_fr  <- as.undirected(gr4_fr[[i]], mode = "collapse")
  # Check if the adversary network exists; if not, create an empty graph.
  if (is.null(gr4_adv[[i]])) {
    # If the friends network exists, use its vertices to create an empty graph.
    if (!is.null(gr4_fr[[i]])) {
      vertex_names <- V(gr4_fr[[i]])$name
      g_adv <- make_empty_graph(n = length(vertex_names), directed = FALSE)
      V(g_adv)$name <- vertex_names
    } else {
      # Otherwise, create an empty graph with zero vertices.
      g_adv <- make_empty_graph(directed = FALSE)
    }
  } else {
    g_adv <- as.undirected(gr4_adv[[i]], mode = "collapse")
  }  
  # Get the names (IDs) of all vertices in the friends network.
  # (Assumes that vertices have a 'name' attribute.)
  vertices <- V(g_fr)$name
  vertices_degree <- degree(g_fr)
  
  # Initialize a data frame with one row per individual and three indicator columns.
  df_village <- data.frame(vertex = vertices,
                           vertices_degree,
                           closed_triangle = 0,  # Indicator for two friends that are friends
                           open_triangle = 0,    # Indicator for two friends that are not connected
                           enemy_triangle = 0,   # Indicator for two friends that are connected as enemies
                           stringsAsFactors = FALSE)
  
  # Process each individual in the current village.
  for(v in vertices) {
    # Get the friend neighbors of v in the friends network
    nbrs <- as_ids(neighbors(g_fr, v))
    
    # If the individual has fewer than 2 friends, no triangle can form.
    if(length(nbrs) < 2) next
    
    # Get all unique pairs of friend neighbors
    # Each pair is a candidate for forming a triangle with v.
    pairs <- combn(nbrs, 2, simplify = FALSE)
    
    # Check each pair of neighbors
    for(pair in pairs) {
      n1 <- pair[1]
      n2 <- pair[2]
      
      # 1. Closed triangle of friends: the two neighbors are friends (in g_fr)
      if(are.connected(g_fr, n1, n2)) {
        df_village[df_village$vertex == v, "closed_triangle"] <- 1
      } else {
        # 2. Open triangle: the two neighbors are not directly connected by a friend edge.
        df_village[df_village$vertex == v, "open_triangle"] <- 1
      }
      
      # 3. Closed triangle with enemy tie: the two neighbors are connected in the adversary network.
      if(are.connected(g_adv, n1, n2)) {
        df_village[df_village$vertex == v, "enemy_triangle"] <- 1
      }
    }
  }
  
  # Merge with the individuals data to get depression status.
  # Here we assume individuals_wave1_fr has columns "Respondent4" and "depressed_w4".
  df_village <- merge(df_village, 
                      individuals_wave4_fr %>% select(Respondent4, depressed_w4),
                      by.x = "vertex", by.y = "Respondent4", 
                      all.x = TRUE)
  # Optionally, rename depressed_w1 to "depressed" for clarity.
  df_village <- df_village %>% rename(depressed = depressed_w4)
  
  # Store the village-level results.
  results_list[[i]] <- df_village
}

# Combine the results from all villages into one data frame.
final_results <- bind_rows(results_list) %>% distinct()

library(dplyr)

# --- Step 1: Combine results from all villages ---
final_results <- bind_rows(results_list) %>% distinct()

# --- Step 2: Add a "none" indicator (individuals with no triangle type) ---
final_results <- final_results %>%
  mutate(none = if_else(closed_triangle == 0 & open_triangle == 0 & enemy_triangle == 0, 1, 0))

# --- Step 3: Create a single categorical variable for triangle type ---
# Priority: enemy > closed > open > none
final_results <- final_results %>%
  mutate(triangle_type = case_when(
    enemy_triangle == 1 ~ "enemy",
    closed_triangle == 1 ~ "closed",
    open_triangle   == 1 ~ "open",
    none == 1         ~ "none"
  ))

# --- Step 4: Set "enemy" as the reference level ---
final_results$triangle_type <- factor(final_results$triangle_type,
                                      levels = c("enemy", "closed", "open", "none"))

# --- Step 5: Run the logistic regression ---
# Here, depressed is assumed to be a binary variable (0/1) where 1 indicates depression.
model <- glm(depressed ~ open_triangle + enemy_triangle + none + vertices_degree, data = final_results_4, family = binomial)

# View the model summary; negative estimates for closed or open (relative to enemy)
# indicate lower log-odds (and odds) of depression.
summary(model)

model_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = final_results_4,
  family = binomial(link = "logit"),
  nAGQ   = 0)

## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_null, model)

##— R^2 computation

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model)        # from performance

#save as 
final_results_4 <- final_results


final_results <- rbind(final_results_1, final_results_3, final_results_4)

## RESULTS

## Divide by gender and get results

individuals_wave1_raw <- read.csv("individuals_wave1.csv")
individuals_wave3_raw <- read.csv("individuals_wave3.csv")
individuals_wave4_raw <- read.csv("individuals_wave4.csv")


# select only the respondent_master_id and gender columns
subset_1 = individuals_wave1_raw[,c('respondent_master_id', 'gender')]
subset_3 = individuals_wave3_raw[,c('respondent_master_id', 'gender')]
subset_4 = individuals_wave4_raw[,c('respondent_master_id', 'gender')]

# 1. Bind your three subsets
combined <- rbind(subset_1, subset_3, subset_4)

# 2a. If you only care about the IDs (as a simple vector):
unique_ids <- unique(combined$respondent_master_id)
print(unique_ids)

# 2b. If you want a data.frame of each respondent_master_id with the gender from its first appearance:
unique_rows <- na.omit(combined[!duplicated(combined$respondent_master_id), ])


unique_rows$gender <- ifelse(
  unique_rows$gender %in% c("man", "male"),
  1,
  0
)


df_male <- subset(final_results,
             vertex %in% unique_rows$respondent_master_id[unique_rows$gender == 1])


# Fit a logistic mixed effects model with random intercepts for vertex.
# Here, depressed is a binary outcome (0/1) and triangle_type is a categorical predictor.
model_male <- glmer(
  depressed ~ open_triangle+enemy_triangle+none + vertices_degree + (1 | vertex),
  data = df_male,
  family = binomial(link = "logit"),
  nAGQ = 0  # Using Laplace approximation
)

# Display the model summary
summary(model_male)


model_male_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = df_male,
  family = binomial(link = "logit"),
  nAGQ   = 0)


# Random effects:
#  Groups   Name        Variance  Std.Dev.
#  vertex   (Intercept) 0.7421    0.8615
# Number of obs: 19717, groups: vertex, 11647
#
# Fixed effects:
#                   Estimate   Std. Error   z value   Pr(>|z|)
# (Intercept)      -1.123078     0.088425   -12.701   < 2e-16 ***
# open_triangle     0.085849     0.091583     0.937   0.3486    
# enemy_triangle    0.102921     0.052296     1.968   0.0491  * 
# none             -0.010400     0.116877    -0.089   0.9291    
# vertices_degree   0.009719     0.005608     1.733   0.0831   .
#
# Explanation:
# 1. (Intercept):
#    - The intercept (–1.12308) represents the baseline log-odds of depression for the 
#      reference group (“closed” triangles) at vertices_degree = 0. A large negative 
#      value indicates low baseline odds (p < 2e-16).
#
# 2. open_triangle:
#    - The coefficient (0.08585) indicates that, compared to closed triangles, males in an 
#      **open** triangle have log-odds of depression **0.08585 units higher**, but this 
#      effect is not statistically significant (p = 0.3486).
#
# 3. enemy_triangle:
#    - The coefficient (0.10292) shows that, relative to closed triangles, males in an 
#      **enemy** triangle have log-odds of depression **0.10292 units higher**, a 
#      marginally significant effect (p = 0.0491, *).
#
# 4. none:
#    - The coefficient (–0.01040) suggests that males with **no** triangle have log-odds 
#      **0.01040 units lower** than closed triangles, but this difference is far from 
#      significant (p = 0.9291).
#
# 5. vertices_degree:
#    - The coefficient (0.00972) means each additional unit of vertices_degree is associated 
#      with a **0.00972** increase in the log-odds of depression, a small positive effect 
#      that is only marginally significant (p = 0.0831, .).


## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_male_null, model_male)

# Likelihood ratio test (male models):
# Data: df_male
# Models:
#   model_male_null: depressed ~ 1 + (1 | vertex)
#   model_male:      depressed ~ open_triangle + enemy_triangle + none + vertices_degree + (1 | vertex)
#                npar   AIC   BIC   logLik deviance  Chisq Df  Pr(>Chisq)
# model_male_null    2 23227 23243  -11612    23223
# model_male         6 23213 23261  -11601    23201 21.616  4 0.0002389 ***
#
# Interpretation:
#  - The likelihood ratio test compares the null model (intercept + random effect) 
#    to the full model including all predictors.
#  - χ²(4) = 21.616, p = 0.00024 indicates that adding open_triangle, enemy_triangle, 
#    none, and vertices_degree leads to a significantly better fit.
#  - Thus, these predictors collectively explain a meaningful portion of variance in depression status for males.


##— R^2 computation

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model_male)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model_male)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model_male)        # from performance


# R² (male model; Nakagawa & Schielzeth, 2013)
#                R2m        R2c
# theoretical    0.0013983  0.1852032
# delta          0.0010300  0.1364218
#
# Explanation:
# 1. Marginal R² (R2m):
#    - Proportion of variance explained by fixed effects alone.
#    - Theoretical: 0.0013983 (~0.14%); Delta: 0.0010300 (~0.10%).
#    - Indicates that open_triangle, enemy_triangle, none, and vertices_degree together 
#      explain only a tiny fraction of variability in depression status.
#
# 2. Conditional R² (R2c):
#    - Proportion of variance explained by both fixed and random effects.
#    - Theoretical: 0.1852032 (~18.5%); Delta: 0.1364218 (~13.6%).
#    - Shows that including the random effect of vertex substantially increases 
#      explained variance, highlighting the importance of between-vertex differences.
#
# 3. Theoretical vs. Delta:
#    - Theoretical R² uses model-implied variances directly.
#    - Delta R² applies a bias-correction based on observed data.
#    - Both measures corroborate that random effects contribute significantly more 
#      to explaining variance than the fixed predictors alone.



df_female <- subset(final_results,
                  vertex %in% unique_rows$respondent_master_id[unique_rows$gender == 0])


# Fit a logistic mixed effects model with random intercepts for vertex.
# Here, depressed is a binary outcome (0/1) and triangle_type is a categorical predictor.
model_female <- glmer(
  depressed ~ open_triangle+enemy_triangle+none + vertices_degree + (1 | vertex),
  data = df_female,
  family = binomial(link = "logit"),
  nAGQ = 0  # Using Laplace approximation
)

# Display the model summary
summary(model_female)



model_female_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = df_female,
  family = binomial(link = "logit"),
  nAGQ   = 0)

## FEMALE

# Random effects:
#  Groups   Name        Variance  Std.Dev.
#  vertex   (Intercept) 0.8030    0.8961
# Number of obs: 30677, groups: vertex, 15671
#
# Fixed effects:
#                   Estimate  Std. Error  z value   Pr(>|z|)
# (Intercept)      -0.75517     0.05778   -13.069   < 2e-16 ***
# open_triangle     0.39612     0.05948     6.659   2.75e-11 ***
# enemy_triangle    0.19184     0.03467     5.533   3.14e-08 ***
# none              0.11345     0.07760     1.462   0.144      
# vertices_degree   0.03406     0.00466     7.307   2.73e-13 ***
#
# Explanation:
# 1. (Intercept):
#    - The intercept (–0.75517) represents the baseline log-odds of depression for the 
#      reference group (“closed” triangles) when vertices_degree = 0. Its large negative 
#      value indicates relatively low baseline odds (p < 2e-16).
#
# 2. open_triangle:
#    - The coefficient (0.39612) shows that, compared to closed triangles, females in an 
#      **open** triangle have log-odds of depression **0.39612 units higher**, a highly 
#      significant increase (p = 2.75e-11).
#
# 3. enemy_triangle:
#    - The coefficient (0.19184) indicates that, relative to closed triangles, females in 
#      an **enemy** triangle have log-odds of depression **0.19184 units higher**, also 
#      highly significant (p = 3.14e-08).
#
# 4. none:
#    - The coefficient (0.11345) suggests that females with **no** triangle have log-odds 
#      **0.11345 units higher** than closed triangles, but this difference is not statistically 
#      significant (p = 0.144).
#
# 5. vertices_degree:
#    - The coefficient (0.03406) means each additional unit of vertices_degree is associated 
#      with a **0.03406** increase in the log-odds of depression (p = 2.73e-13), indicating a 
#      small but highly significant positive effect of network degree.


## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_female_null, model_female)


# Likelihood ratio test (female models):
# Data: df_female
# Models:
#   model_female_null: depressed ~ 1 + (1 | vertex)
#   model_female:      depressed ~ open_triangle + enemy_triangle + none + vertices_degree + (1 | vertex)
#                   npar    AIC    BIC   logLik deviance   Chisq Df  Pr(>Chisq)
# model_female_null    2 41040 41057  -20518   41036
# model_female         6 40850 40900  -20419   40838 197.85  4  < 2.2e-16 ***
#
# Interpretation:
#  - The likelihood ratio test compares the null model (intercept + random effect) 
#    to the full model including open_triangle, enemy_triangle, none, and vertices_degree.
#  - χ²(4) = 197.85, p < 2.2e-16 indicates that the full model fits the data 
#    significantly better than the null model.
#  - Therefore, these predictors together contribute a highly significant improvement 
#    in explaining depression status for females.


## R^2

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model_female)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model_female)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model_female)        # from performance


# R² (female model; Nakagawa & Schielzeth, 2013)
#                R2m        R2c
# theoretical    0.00715596  0.20195460
# delta          0.00598719  0.16896970
#
# Tjur’s coefficient of discrimination (logistic; performance::r2_tjur):
# Tjur’s R²    0.1765537
#
# Other fit indices (performance::r2):
# Marginal R²   0.007
# Conditional R² 0.202
#
# Explanation:
# 1. Marginal R² (R2m):
#    - Theoretical: ~0.72%; Delta: ~0.60%.
#    - Indicates the fixed effects (open_triangle, enemy_triangle, none, vertices_degree)
#      explain less than 1% of the variance in depression status for females.
#
# 2. Conditional R² (R2c):
#    - Theoretical: ~20.2%; Delta: ~16.9%.
#    - Shows that including the random effect of vertex boosts explained variance substantially,
#      underscoring the importance of between-vertex heterogeneity.
#
# 3. Tjur’s R²:
#    - 0.1766 (~17.7%), reflecting the difference in mean predicted probability
#      between depressed and non-depressed groups.
#    - Offers an intuitive measure of discrimination: higher values indicate better separation.
#
# 4. performance::r2:
#    - Marginal: ~0.7%; Conditional: ~20.2%, consistent with Nakagawa–Schielzeth estimates.
#    - Reinforces that random effects contribute the bulk of explained variance.



## Both Male and Female


# Fit a logistic mixed effects model with random intercepts for vertex.
# Here, depressed is a binary outcome (0/1) and triangle_type is a categorical predictor.
model <- glmer(
  depressed ~ open_triangle+enemy_triangle+none + vertices_degree + (1 | vertex),
  data = final_results,
  family = binomial(link = "logit"),
  nAGQ = 0  # Using Laplace approximation
)

# Display the model summary
summary(model)



### ALL


# Random effects:
#  Groups   Name        Variance  Std.Dev.
#  vertex   (Intercept) 0.8589    0.9268
# Number of obs: 50394, groups: vertex, 27318
#
# Fixed effects:
#                  Estimate   Std. Error   z value   Pr(>|z|)
# (Intercept)     -0.919498     0.049167   -18.701   < 2e-16 ***
# open_triangle    0.204263     0.051113     3.996   6.43e-05 ***
# enemy_triangle   0.119228     0.030719     3.881   0.000104 ***
# none             0.091834     0.064996     1.413   0.157682    
# vertices_degree  0.018852     0.003567     5.286   1.25e-07 ***
#
# Correlation of Fixed Effects:
#                (Intr)  opn_tr  enmy_t   none
# open_triangle   -0.882                    
# enemy_triangle   0.069    0.011             
# none            -0.738    0.677  -0.033      
# vertices_degree -0.176   -0.236  -0.421   0.082
#
# Explanation:
# 1. (Intercept):
#    - The intercept (–0.91950) represents the baseline log-odds of depression for the 
#      reference group, which here is the “closed triangle” category (all dummy indicators = 0) 
#      at vertices_degree = 0. A negative value indicates low baseline odds.
#
# 2. open_triangle:
#    - The coefficient (0.20426) indicates that, compared to closed triangles, individuals in 
#      an **open** triangle have log-odds of depression **0.20426 units higher** (p < 0.001), 
#      suggesting significantly greater odds in open structures.
#
# 3. enemy_triangle:
#    - The coefficient (0.11923) shows that, relative to closed triangles, those in an **enemy** 
#      triangle have log-odds of depression **0.11923 units higher** (p < 0.001), also a significant effect.
#
# 4. none:
#    - The coefficient (0.09183) indicates that individuals with **no** triangle have log-odds 
#      of depression **0.09183 units higher** than closed triangles, but this difference is not 
#      statistically significant (p = 0.1577).
#
# 5. vertices_degree:
#    - The coefficient (0.01885) means that each additional unit of vertices_degree is associated 
#      with a **0.01885** increase in the log-odds of depression (p < 0.001), indicating a small 
#      but highly significant positive relationship with degree.


model_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = final_results,
  family = binomial(link = "logit"),
  nAGQ   = 0)

## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_null, model)


# Likelihood ratio test (full dataset: final_results)
# Data: final_results
# Models:
#   model_null: depressed ~ 1 + (1 | vertex)
#   model:      depressed ~ open_triangle + enemy_triangle + none + vertices_degree + (1 | vertex)
#           npar    AIC    BIC   logLik deviance   Chisq Df  Pr(>Chisq)
# model_null    2 65085 65102  -32540   65081
# model         6 64933 64986  -32460   64921 159.88  4  < 2.2e-16 ***
#
# Interpretation:
#  - The likelihood ratio test compares the null model (intercept + random effect) 
#    to the full model including open_triangle, enemy_triangle, none, and vertices_degree.
#  - χ²(4) = 159.88, p < 2.2e-16 indicates the full model provides a highly significant 
#    improvement in fit over the null.
#  - Therefore, these predictors collectively explain a significant amount of variability 
#    in depression status when considering all participants.


## R^2

##— marginal / conditional R² (Nakagawa & Schielzeth, 2013) —─
r.squaredGLMM(model)
#      R2m        R2c
#  [fixed]  [fixed+random]


##— Tjur’s coefficient of discrimination (logistic only) ——––
r2_tjur(model)   # from performance


##— McFadden, Cox & Snell, Nagelkerke, … (all at once) ——––—
r2(model)        # from performance


# R² (full model; Nakagawa & Schielzeth, 2013)
#                R2m        R2c
# theoretical    0.00358645  0.20987860
# delta          0.00291078  0.17033840
#
# Explanation:
# 1. Marginal R² (R2m):
#    - Theoretical: ~0.36%; Delta: ~0.29%.
#    - Indicates that the fixed effects (open_triangle, enemy_triangle, none, vertices_degree)
#      explain only a small fraction of the variance in depression status across all participants.
#
# 2. Conditional R² (R2c):
#    - Theoretical: ~21.0%; Delta: ~17.0%.
#    - Shows that including the random effect of vertex substantially increases the total
#      variance explained, highlighting the importance of between-vertex heterogeneity in the full dataset.

