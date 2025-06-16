****Code for depression analysis in Honduras
################################################################################
# README  – Reproducing “Depression in Friendly and Adversarial Social Networks
#            in Isolated Honduran Villages”
#
# This commented block describes every major step (and key R packages) used to
# transform raw survey + network data into the results presented in the paper
# and its appendix (part of the data will be available upon submission on Zenodo, in communication with Liza Nicoll - liza.nicoll@yale.edu).
#
# Environment ------------------------------------------------------------------
# • R 4.4.2 (2025-03-24 build)                  
# • Core CRAN packages
#     tidyverse      # data wrangling / plotting
#     data.table     # fast I/O for 50k-row panel
#     igraph         # sociocentric network objects + metrics  
#     lme4           # (g)lmer() mixed-effects logistic models
#     lmerTest       # Satterthwaite dfs + p-values
#     performance    # r2_nakagawa(), model checks
#     MuMIn          # pseudo-R² tables
# • Optional visual packages
#     ggraph, cowplot, gridExtra – network/forest plots
#
# Data inputs ------------------------------------------------------------------
# 1. Survey panel (3 waves: 2015-16, 2019, 2022-23) with 27,183 unique adults
#    and 49,987 person-wave observations. Variables include demographics,
#    PHQ-2 items, household IDs, village IDs, etc. :contentReference
# 2. Signed sociocentric adjacency lists per village produced in Trellis
#    Software field tablets, covering both friendship (+) and adversary (–)
#    nominations. :contentReference
#
# Data preparation -------------------------------------------------------------
# • Import survey CSVs → merge into long panel          
# • Recode PHQ-2 ≥ 2  ➜  depression == 1                 
# • Identify postpartum rows: child_age_months < 6.
# • Build igraph objects village-by-village:
#     g <- graph_from_data_frame(edges, vertices = people, directed = FALSE)
# • Per-ego network metrics:
#     deg_pos   <- degree(g, mode = "all", weights = edge$type == "friend")
#     deg_neg   <- degree(g, mode = "all", weights = edge$type == "enemy")
#     intrans   <- transitivity(g, type = "localundirected", isolates = "zero")
# • Triad labelling
#     triads <- motifs(g, size = 3)
#     classify as balanced / incomplete / negative (friend-friend-enemy) and
#     flag egos appearing in ≥1 of each motif type. 
# • Village-level metrics:
#     dens_pos <- edge_density(induced_subgraph(g, E(g)[type == "friend"]))
#     dens_neg <- edge_density(induced_subgraph(g, E(g)[type == "enemy"]))
#
# Modelling --------------------------------------------------------------------
# 1. Main depression models (stratified by gender)
#    fit <- glmer(
#       depression ~ age + married + educ + religion + indigenous + food_insec +
#                   hh_dep_prop + deg_pos + deg_neg + friends_dep_prop +
#                   intrans + mean_friends_deg + dens_pos + dens_neg +
#                   wave + (1|respondent_id) + (1|household_id) + (1|village_id),
#       family = binomial, data = dplyr::filter(dat, gender == "Female")
#    )
#    – Repeat for males.  Goodness-of-fit: r2_nakagawa(), LRT vs null.  
#
# 2. Triad-specific model
#    triad_fit <- glmer(
#       depression ~ triad_type + deg_pos + (1|respondent_id),
#       family = binomial, data = triad_df, subset = gender == "Male"
#    )                                   
#
# 3. Post-partum subset
#    pp_fit <- glmer(
#       pp_depression ~ age + food_insec + hh_dep_prop + friends_dep_prop +
#                       deg_neg + dens_neg + intrans +
#                       (1|respondent_id) + (1|village_id),
#       family = binomial, data = subset(dat, postpartum == 1 & gender == "Female")
#    )                               
#
# Tables & Figures -------------------------------------------------------------
# • tab_model/performance::table_model() for mixed-effects OR tables (Tables 2–7)
# • ggplot2::geom_pointrange() for forest plots (Figures 3 & 5)
# • ggraph() + geom_edge_link(aes(colour = type)) to illustrate triadic motifs
#   (Figure 4 panels A–B) using the same igraph objects.
#
# Reproducibility --------------------------------------------------------------
# • sessionInfo() captured at end of script for appendix.******
Please cite paper "Depression in Friendly and Adversarial Social Networks in Isolated Honduran Villages". 
This code belongs to HNL Yale University.
################################################################################**
