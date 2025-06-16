# Save output to a PNG file
png("/WORKAREA/work/HONDURAS_GATES/M_PAPAMICHALIS/Depression/Code/Results/test1.png", 
    width = 1800, height = 1800, res = 150)

############################################
# 1. SET UP THE OVERALL LAYOUT
############################################
# Layout matrix:
# - Row 1: Panel 1 (network plot) spanning all 6 columns.
# - Row 2: Panels 2, 3, 4 (each spanning 2 columns).
# - Row 3: Panel 5 (combined motif count barplot, Panel C) centered in columns 2-5.
# - Row 4: Panels 6 and 7 (fractional barplots, Panels D1 and D2) each spanning 3 columns.
layout_matrix <- rbind(
  rep(1, 6),             # Row 1: Panel 1 (spans 6 columns)
  c(2, 2, 3, 3, 4, 4),    # Row 2: Panels 2, 3, 4 (each 2 columns wide)
  c(5, 5, 5, 6, 6, 6),    # Row 3: Panels 5 and 6 (each 3 columns wide)
  c(7, 7, 7, 8, 8, 8)     # Row 4: Panels 7 and 8 (each 3 columns wide)
)
layout(layout_matrix)

############################################
# 2. PANEL 1: TOP PLOT (Network: Depression/Gender)
############################################
par(mar = c(0, 0, 1, 0))
if (!require("igraph")) {
  install.packages("igraph")
  library(igraph)
}
plot(
  gr3_fr[[63]],
  layout = layout_with_fr(gr3_fr[[63]]),
  main = "Depression/Gender",
  vertex.shape = ifelse(V(gr3_fr[[63]])$gender == 1, "circle", "square"),
  vertex.color = ifelse(V(gr3_fr[[63]])$depressed_w3 == 1, "black", "white"),
  vertex.size = 7,
  vertex.label = NA,
  edge.arrow.mode = 0
)
mtext("(A)", side = 3, adj = 0, line = -1, cex = 1.2)
par(xpd = TRUE)
legend(
  "topright",
  inset = c(+0.255, +0.1),
  legend = c("Depressed", "Not depressed"),
  pch = 21,
  pt.cex = 1.5,
  pt.bg = c("black", "white"),
  title = "",
  bty = "n"
)
legend(
  "topright",
  inset = c(+0.18, +0.1),
  legend = c("Women", "Men"),
  pch = c(21, 22),
  pt.cex = 1.5,
  title = "",
  bty = "n"
)
par(xpd = FALSE)

############################################
# 3. PANELS 2-4: THE THREE IGAPH (UNION/TRIAD) PLOTS (B1, B2, B3)
############################################
par(mar = c(0, 0, 1, 0))
# --- Prepare and merge graphs ---
g  <- igraph::simplify(gr3_fr[[63]], remove.multiple = TRUE)
g1 <- igraph::simplify(gr3_adv[[63]], remove.multiple = TRUE)

# Color edges for each graph
g_green <- g
E(g_green)$color <- "black"
g_red <- g1
E(g_red)$color <- "orange"

# Merge the two graphs (using igraph::union)
g_merged <- igraph::union(g_green, g_red, byname = TRUE)

# Combine color attributes if edges appear in both graphs
if (!is.null(E(g_merged)$color_1) && !is.null(E(g_merged)$color_2)) {
  edge_colors <- character(ecount(g_merged))
  for (i in seq_len(ecount(g_merged))) {
    c1 <- E(g_merged)$color_1[i]
    c2 <- E(g_merged)$color_2[i]
    if (!is.na(c1) && !is.na(c2)) {
      edge_colors[i] <- "purple"  # Both colors present
    } else if (!is.na(c1)) {
      edge_colors[i] <- c1
    } else if (!is.na(c2)) {
      edge_colors[i] <- c2
    } else {
      edge_colors[i] <- "gray"
    }
  }
  E(g_merged)$color <- edge_colors
}

# Define vertex colors (here both conditions give "gray")
vertex_colors <- ifelse(V(g_merged)$depressed_w3 == 1, "gray", "gray")

# Use layout from original graph g, remapped for g_merged
l1 <- layout_with_fr(g)
l_merged <- l1[match(V(g_merged)$name, V(g)$name), ]

# --- Build undirected graph for triad checks ---
g_und <- as.undirected(g_merged, mode = "collapse")
E(g_und)$color <- E(g_merged)$color

nV <- vcount(g_und)
hasGreenClosed <- logical(nV)  # Flag for all-green closed triads
hasRedClosed   <- logical(nV)  # Flag for closed triads with 2 green + 1 red
hasOpen        <- logical(nV)  # Flag for open triads

# Helper function: neighbors connected to v by a GREEN edge
neighbors_green <- function(graph, v) {
  nbrs <- neighbors(graph, v, mode = "all")
  gNbrs <- c()
  for (w in nbrs) {
    e <- E(graph)[ v %--% w ]
    if (length(e) > 0 && e$color == "black") {
      gNbrs <- c(gNbrs, w)
    }
  }
  return(gNbrs)
}

# Detect triads for each node
for (v in seq_len(nV)) {
  gv <- neighbors_green(g_und, v)
  if (length(gv) < 2) next
  pair_idx <- combn(gv, 2)
  for (col_i in seq_len(ncol(pair_idx))) {
    j <- pair_idx[1, col_i]
    k <- pair_idx[2, col_i]
    e_jk <- E(g_und)[ j %--% k ]
    if (length(e_jk) == 0) {
      hasOpen[v] <- TRUE
    } else {
      col_jk <- e_jk$color
      if (col_jk == "black") {
        hasGreenClosed[v] <- TRUE
      } else if (col_jk == "orange") {
        hasRedClosed[v] <- TRUE
      } else if (col_jk == "purple") {
        hasGreenClosed[v] <- TRUE
      }
    }
  }
}

# --- Assign each node a category based on triad structure ---
# Category rules:
#  - Category 3: node has a 2-green+1-red triad
#  - Category 1: node has all-green triad and NO open triad
#  - Category 2: node has open triad (and no red triad, and no green closed triad)
category <- rep(NA, nV)
for (v in seq_len(nV)) {
  if (hasRedClosed[v]) {
    category[v] <- 3
  } else {
    if (hasGreenClosed[v] && !hasOpen[v]) {
      category[v] <- 1
    } else if (hasOpen[v] && !hasGreenClosed[v]) {
      category[v] <- 2
    }
  }
}

# --- Highlight edges for each category ---
edges_cat1 <- rep(FALSE, ecount(g_merged))  # For category 1 (all-green closed triads)
edges_cat2 <- rep(FALSE, ecount(g_merged))  # For category 2 (open triads)
edges_cat3 <- rep(FALSE, ecount(g_merged))  # For category 3 (2-green+1-red triads)

for (v in seq_len(nV)) {
  gv <- neighbors_green(g_und, v)
  if (length(gv) < 2) next
  pair_idx <- combn(gv, 2)
  for (col_i in seq_len(ncol(pair_idx))) {
    j <- pair_idx[1, col_i]
    k <- pair_idx[2, col_i]
    e_jk_und <- E(g_und)[ j %--% k ]
    triad_has_edge_jk <- (length(e_jk_und) > 0)
    col_jk <- if (triad_has_edge_jk) e_jk_und$color else NA
    if (!is.na(category[v]) && category[v] == 1) {
      if (triad_has_edge_jk && col_jk %in% c("black", "purple")) {
        e_vj <- E(g_merged)[ v %--% j ]
        e_vk <- E(g_merged)[ v %--% k ]
        e_jk <- E(g_merged)[ j %--% k ]
        edges_cat1[e_vj] <- TRUE
        edges_cat1[e_vk] <- TRUE
        edges_cat1[e_jk] <- TRUE
      }
    }
    if (!is.na(category[v]) && category[v] == 2) {
      if (!triad_has_edge_jk) {
        e_vj <- E(g_merged)[ v %--% j ]
        e_vk <- E(g_merged)[ v %--% k ]
        edges_cat2[e_vj] <- TRUE
        edges_cat2[e_vk] <- TRUE
      }
    }
    if (!is.na(category[v]) && category[v] == 3) {
      if (triad_has_edge_jk && col_jk == "orange") {
        e_vj <- E(g_merged)[ v %--% j ]
        e_vk <- E(g_merged)[ v %--% k ]
        e_jk <- E(g_merged)[ j %--% k ]
        edges_cat3[e_vj] <- TRUE
        edges_cat3[e_vk] <- TRUE
        edges_cat3[e_jk] <- TRUE
      }
    }
  }
}

# --- Plot the three igraph panels ---
# Panel B1: All-green closed triads
edge_col_plot1 <- ifelse(edges_cat1, E(g_merged)$color, "gray")
plot(
  g_merged, layout = l_merged,
  main = "Balanced triads",
  vertex.size = 5,
  edge.arrow.size = 0.4,
  vertex.label = NA,
  vertex.color = vertex_colors,
  edge.color = edge_col_plot1
)
mtext("(B1)", side = 3, adj = 0, line = 0.2, cex = 1.2)

# Panel B2: Open triads
edge_col_plot2 <- ifelse(edges_cat2, E(g_merged)$color, "gray")
plot(
  g_merged, layout = l_merged,
  main = "Incomplete Triads",
  vertex.size = 5,
  edge.arrow.size = 0.4,
  vertex.label = NA,
  vertex.color = vertex_colors,
  edge.color = edge_col_plot2
)
mtext("(B2)", side = 3, adj = 0, line = 0.2, cex = 1.2)

# Panel B3: 2-green-1-red triads
edge_col_plot3 <- ifelse(edges_cat3, E(g_merged)$color, "gray")
plot(
  g_merged, layout = l_merged,
  main = "Negative Triads",
  vertex.size = 5,
  edge.arrow.size = 0.4,
  vertex.label = NA,
  vertex.color = vertex_colors,
  edge.color = edge_col_plot3
)
mtext("(B3)", side = 3, adj = 0, line = 0.2, cex = 1.2)

############################################
# 4. PANEL 5: COMBINED BARPLOT (Panel C) - MOTIF COUNTS
############################################
# This plot will appear centered in row 3.
par(mar = c(3, 2, 2, 1))
# Define counts for the three bars:
#   Green motif: 41198, Light Green motif: 44920, Enemy triangle (red): 8216
motif_counts <- c("Balanced Triads" = 16169, "Incomplete Triads" = 17874, "Negative Triads" = 3029)
motif_colors <- c("black", "gray", "orange")
bp <- barplot(
  motif_counts,
  col = motif_colors,
  ylim = c(0, max(motif_counts) * 1.2),
  main = "Men",
  ylab = "Number of Men"
)
text(x = bp, y = motif_counts, labels = motif_counts, pos = 3, cex = 0.8)
mtext("(C1)", side = 3, adj = 0, line = 0.4, cex = 1.2)


# Define counts for the three bars:
#   Green motif: 41198, Light Green motif: 44920, Enemy triangle (red): 8216
motif_counts <- c("Balanced Triads" = 25007, "Incomplete Triads" = 27022, "Negative Triads" = 5167)
motif_colors <- c("black", "gray", "orange")
bp <- barplot(
  motif_counts,
  col = motif_colors,
  ylim = c(0, max(motif_counts) * 1.2),
  main = "Women",
  ylab = "Number of Women"
)
text(x = bp, y = motif_counts, labels = motif_counts, pos = 3, cex = 0.8)
mtext("(C2)", side = 3, adj = 0, line = 0.4, cex = 1.2)


############################################
# 5. PANELS 6-7: FRACTIONAL BARPLOTS (Panels D1 and D2)
############################################
# Define the matrices for the fractional values:
# Panel D: Fractional barplot
par(mar = c(4, 2, 1, 1))
# Numeric values for plotting
motif_counts <- c("Balanced Triads" = 28.62, 
                  "Incomplete Triads" = 28.54, 
                  "Negative Triads" = 31.20)

# Character labels with percentage symbols
motif_labels <- c("Balanced Triads" = "28.62%", 
                  "Incomplete Triads" = "28.54%", 
                  "Negative Triads" = "31.20%")

motif_colors <- c("black", "gray", "orange")

bp <- barplot(
  motif_counts,
  col = motif_colors,
  ylim = c(0, max(motif_counts) * 1.2),
  main = "Depression Prevalence among Men",
  ylab = "Depression Prevalence"
)

# Add percentage labels on the bars
text(x = bp, y = motif_counts, labels = motif_labels, pos = 3, cex = 0.8)
mtext("(D1)", side = 3, adj = 0, line = 0.4, cex = 1.2)


motif_counts <- c("Balanced Triads" = 42.24, 
                  "Incomplete Triads" = 42.58, 
                  "Negative Triads" = 46.76)

# Character labels with percentage symbols
motif_labels <- c("Balanced Triads" = "42.24%", 
                  "Incomplete Triads" = "42.58%", 
                  "Negative Triads" = "46.76%")

motif_colors <- c("black", "gray", "orange")

bp <- barplot(
  motif_counts,
  col = motif_colors,
  ylim = c(0, max(motif_counts) * 1.2),
  main = "Depression Prevalence among Women",
  ylab = "Depression Prevalence"
)

# Add percentage labels on the bars
text(x = bp, y = motif_counts, labels = motif_labels, pos = 3, cex = 0.8)
mtext("(D2)", side = 3, adj = 0, line = 0.4, cex = 1.2)


dev.off()
