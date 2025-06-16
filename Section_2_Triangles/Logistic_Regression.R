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
  depressed ~ open_triangle+enemy_triangle+none + (1 | vertex),
  data = df_male,
  family = binomial(link = "logit"),
  nAGQ = 0  # Using Laplace approximation
)

# Display the model summary
summary(model_male)


# Number of obs: 19717, groups: vertex, 11647
#
# Fixed effects:
#                   Estimate   Std. Error   z value   Pr(>|z|)
# (Intercept)      -1.09992      0.08740    -12.585    < 2e-16 ***
# open_triangle     0.12032      0.08937      1.346     0.17823    
# enemy_triangle    0.14219      0.04710      3.019     0.00254  ** 
# none             -0.02451      0.11658     -0.210     0.83348    
#
# Explanation:
# 1. (Intercept):
#    - The intercept (–1.09992) represents the baseline log-odds of depression for the
#      reference group (“closed” triangles) at vertices_degree = 0. A large negative value
#      indicates low baseline odds (p < 2e-16).
#
# 2. open_triangle:
#    - The coefficient (0.12032) indicates that, compared to closed triangles, males in an
#      **open** triangle have log-odds of depression **0.12032 units higher**, but this
#      effect is not statistically significant (p = 0.1782).
#
# 3. enemy_triangle:
#    - The coefficient (0.14219) shows that, relative to closed triangles, males in an
#      **enemy** triangle have log-odds of depression **0.14219 units higher**, a
#      significant effect (p = 0.00254, **).
#
# 4. none:
#    - The coefficient (–0.02451) suggests that males with **no** triangle have log-odds
#      **0.02451 units lower** than closed triangles, but this difference is not
#      statistically significant (p = 0.8335).


model_male_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = df_male,
  family = binomial(link = "logit"),
  nAGQ   = 0)

## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_male_null, model_male)
# Likelihood ratio test (male models):
# Data: df_male
# Models:
#   model_male_null: depressed ~ 1 + (1 | vertex)
#   model_male:      depressed ~ open_triangle + enemy_triangle + none + (1 | vertex)
#               npar   AIC   BIC   logLik deviance  Chisq Df   Pr(>Chisq)
# model_male_null    2 23227 23243  -11612   23223
# model_male         5 23215 23254  -11602   23205 18.25  3 0.0003905 ***
#
# Interpretation:
#  - The likelihood ratio test compares the null model (intercept + random effect)
#    to the model including the three triangle predictors.
#  - χ²(3) = 18.25, p = 0.00039 indicates that adding open_triangle, enemy_triangle,
#    and none leads to a significantly better fit.
#  - Therefore, these three predictors collectively explain a significant portion of
#    the variance in depression status for males.

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
# theoretical    0.00116661  0.1848200
# delta          0.00085923  0.1361226
#
# Explanation:
# 1. Marginal R² (R2m):
#    - Proportion of variance explained by fixed effects alone.
#    - Theoretical: ~0.12%; Delta: ~0.09%.
#    - Indicates that open_triangle, enemy_triangle, and none explain a very small fraction 
#      of variability in depression status for males.
#
# 2. Conditional R² (R2c):
#    - Proportion of variance explained by both fixed and random effects.
#    - Theoretical: ~18.5%; Delta: ~13.6%.
#    - Shows that including the random effect of vertex substantially increases explained variance,
#      underscoring the importance of between-vertex heterogeneity.
#
# 3. Theoretical vs. Delta:
#    - Theoretical R² is based on model-implied variance components.
#    - Delta R² applies an observed-data correction.
#    - Both measures highlight that random effects contribute far more to explained variance than
#      the fixed predictors alone.



df_female <- subset(final_results,
                    vertex %in% unique_rows$respondent_master_id[unique_rows$gender == 0])


# Fit a logistic mixed effects model with random intercepts for vertex.
# Here, depressed is a binary outcome (0/1) and triangle_type is a categorical predictor.
model_female <- glmer(
  depressed ~ open_triangle+enemy_triangle+none + (1 | vertex),
  data = df_female,
  family = binomial(link = "logit"),
  nAGQ = 0  # Using Laplace approximation
)

# Display the model summary
summary(model_female)


## FEMALE
# Number of obs: 30677, groups: vertex, 15671
#
# Fixed effects:
#               Estimate   Std. Error   z value   Pr(>|z|)
# (Intercept)   -0.75518      0.05778   -13.070   < 2e-16 ***
# open_triangle  0.39611      0.05948     6.659   2.75e-11 ***
# enemy_triangle 0.19110      0.03467     5.511   3.57e-08 ***
# none           0.11345      0.07759     1.462     0.144    
#
# Correlation of Fixed Effects:
#              (Intr)  opn_tr  enmy_t
# open_triangle -0.962                   
# enemy_triangle -0.006  -0.107          
# none          -0.734   0.713   0.001   
#
# Explanation:
# 1. (Intercept):
#    - The intercept (–0.75518) represents the baseline log-odds of depression for the 
#      reference category (“closed” triangles) when all predictors are zero. Its negative 
#      value indicates low baseline odds (p < 2e-16).
#
# 2. open_triangle:
#    - Coefficient = 0.39611. Females in an **open** triangle have log-odds of depression 
#      **0.39611 units higher** than those in the reference group, a highly significant 
#      increase (p = 2.75e-11).
#
# 3. enemy_triangle:
#    - Coefficient = 0.19110. Females in an **enemy** triangle have log-odds of depression 
#      **0.19110 units higher** than the reference, also highly significant (p = 3.57e-08).
#
# 4. none:
#    - Coefficient = 0.11345. Females with **no** triangle have log-odds **0.11345 units 
#      higher** than the reference, but this effect is not statistically significant 
#      (p = 0.144).


model_female_null <- glmer(
  depressed ~ 1 + (1 | vertex),
  data   = df_female,
  family = binomial(link = "logit"),
  nAGQ   = 0)

## likelihood-ratio (χ²) test ─────────────────────────────────────
anova(model_female_null, model_female)

# Likelihood ratio test (female models):
# Data: df_female
# Models:
#   model_female_null: depressed ~ 1 + (1 | vertex)
#   model_female:      depressed ~ open_triangle + enemy_triangle + none + (1 | vertex)
#                   npar   AIC   BIC   logLik deviance   Chisq Df  Pr(>Chisq)
# model_female_null    2 41040 41057  -20518    41036
# model_female         5 40912 40953  -20451    40902 134.31  3  < 2.2e-16 ***
#
# Interpretation:
#  - The likelihood ratio test compares the null model (intercept + random effect)
#    to the model including the three triangle predictors.
#  - χ²(3) = 134.31, p < 2.2e-16 indicates that adding open_triangle, enemy_triangle,
#    and none yields a significantly better fit.
#  - Thus, these predictors together explain a substantial portion of variance in depression
#    status for females beyond the random vertex effect.

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
# theoretical    0.00478771  0.20434120
# delta          0.00400770  0.17105020
#
# Explanation:
# 1. Marginal R² (R2m):
#    - Proportion of variance explained by fixed effects alone.
#    - Theoretical: ~0.48%; Delta: ~0.40%.
#    - Indicates that open_triangle, enemy_triangle, and none together explain less 
#      than 0.5% of the variability in depression status for females.
#
# 2. Conditional R² (R2c):
#    - Proportion of variance explained by both fixed and random effects.
#    - Theoretical: ~20.4%; Delta: ~17.1%.
#    - Shows that including the random effect of vertex substantially increases 
#      the variance explained, highlighting the importance of between-vertex differences.


## Both Male and Female


# Fit a logistic mixed effects model with random intercepts for vertex.
# Here, depressed is a binary outcome (0/1) and triangle_type is a categorical predictor.
model <- glmer(
  depressed ~ open_triangle+enemy_triangle+none +  (1 | vertex),
  data = final_results,
  family = binomial(link = "logit"),
  nAGQ = 0  # Using Laplace approximation
)

# Display the model summary
summary(model)



### ALL

# Random effects:
#  Groups   Name        Variance  Std.Dev.
#  vertex   (Intercept) 0.8631    0.9290
# Number of obs: 50394, groups: vertex, 27318
#
# Fixed effects:
#                   Estimate   Std. Error   z value   Pr(>|z|)
# (Intercept)      -0.87386      0.04842   -18.048   < 2e-16 ***
# open_triangle     0.26765      0.04969     5.386   7.20e-08 ***
# enemy_triangle    0.18744      0.02787     6.726   1.75e-11 ***
# none              0.06375      0.06481     0.984     0.325    
#
# Correlation of Fixed Effects:
#                (Intr)  opn_tr  enmy_t
# open_triangle  -0.966                   
# enemy_triangle -0.006  -0.100          
# none           -0.738   0.718   0.001   
#
# Explanation:
# 1. (Intercept):
#    - The intercept (–0.87386) is the baseline log-odds of depression for the reference 
#      group (“closed” triangles) at the mean level of vertices_degree. A large negative  
#      value signals low baseline odds (p < 2e-16).
#
# 2. open_triangle:
#    - Coefficient = 0.26765. Individuals in an **open** triangle have log-odds of depression 
#      **0.26765 units higher** than those in closed triangles, a highly significant effect 
#      (p = 7.20e-08).
#
# 3. enemy_triangle:
#    - Coefficient = 0.18744. Individuals in an **enemy** triangle have log-odds of depression 
#      **0.18744 units higher** than closed triangles, also highly significant (p = 1.75e-11).
#
# 4. none:
#    - Coefficient = 0.06375. Individuals with **no** triangle exhibit log-odds  
#      **0.06375 units higher** than closed triangles, but this is not statistically significant 
#      (p = 0.325).


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
#   model:      depressed ~ open_triangle + enemy_triangle + none + (1 | vertex)
#           npar   AIC   BIC   logLik deviance  Chisq Df  Pr(>Chisq)
# model_null    2 65085 65102  -32540    65081
# model         5 64964 65008  -32477    64954 126.85  3  < 2.2e-16 ***
#
# Interpretation:
#  - The likelihood ratio test compares the null model (intercept + random effect)
#    to the model including the three triangle predictors.
#  - χ²(3) = 126.85, p < 2.2e-16 indicates that adding open_triangle, enemy_triangle,
#    and none yields a highly significant improvement in fit.
#  - Thus, these three predictors together explain a significant portion of variability
#    in depression status across all participants.


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
# theoretical    0.00280674  0.21004960
# delta          0.00227806  0.17048410
#
# Explanation:
# 1. Marginal R² (R2m):
#    - Proportion of variance explained by fixed effects alone.
#    - Theoretical: ~0.28%; Delta: ~0.23%.
#    - Indicates that open_triangle, enemy_triangle, and none explain only a small fraction 
#      of variance in depression status across all participants.
#
# 2. Conditional R² (R2c):
#    - Proportion of variance explained by both fixed and random effects.
#    - Theoretical: ~21.0%; Delta: ~17.0%.
#    - Demonstrates that including the random effect of vertex substantially increases 
#      the total variance explained, underscoring the importance of between-vertex heterogeneity.
