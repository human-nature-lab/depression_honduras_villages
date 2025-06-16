load("Depression/Code/Results/network_data.RData")

png("/WORKAREA/work/HONDURAS_GATES/M_PAPAMICHALIS/Depression/Code/Results/test1.png", width = 1800, height = 1800, res = 150)


############################################
# 1. SET UP THE OVERALL LAYOUT
############################################
# We create a layout matrix with 4 rows and 6 columns.
# Row 1: panel 1 spans all 6 columns.
# Row 2: panels 2, 3, and 4 each span 2 columns.
# Row 3: panels 5 and 6 each span 3 columns.
# Row 4: panels 7 and 8 each span 3 columns.
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
# Adjust margins as needed
par(mar = c(0, 0, 1, 0))

# Plot Panel 1 (network plot)
if (!require("igraph")) {
  install.packages("igraph")
  library(igraph)
}

# Plot the network (using layout_with_fr since layout.sphere is not built-in)
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

# Add label (A) in Panel 1.
# (For Panel (A), we use a slightly lower line value so that it is a bit further down.)
mtext("(A)", side = 3, adj = 0, line = -1, cex = 1.2)

# Optionally, if you want to add legends on Panel 1, include them here:
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
  legend = c("Female", "Male"),
  pch = c(21, 22),
  pt.cex = 1.5,
  title = "",
  bty = "n"
)
par(xpd = FALSE)

############################################
# 3. PANELS 2-4: THE THREE IGRAPH (Union/Triad) PLOTS
############################################
par(mar = c(0, 0, 1, 0))

# ------------------------------------------------------------------
# 1) PREPARE YOUR TWO GRAPHS (g and g1) AND MERGE THEM
# ------------------------------------------------------------------
# Example placeholders; you have them from your code:
g  <- igraph::simplify(gr3_fr[[63]], remove.multiple = TRUE)
g1 <- igraph::simplify(gr3_adv[[63]],    remove.multiple = TRUE)

# Color edges
g_green <- g
E(g_green)$color <- "green"

g_red <- g1
E(g_red)$color <- "red"

# Merge (igraph::union). If you get an error about 'byname = TRUE',
# you can use `graph.igraph::union(...)` instead:
g_merged <- igraph::union(g_green, g_red, byname=TRUE)

# Combine color attributes if edges appear in both graphs
if (!is.null(E(g_merged)$color_1) && !is.null(E(g_merged)$color_2)) {
  edge_colors <- character(ecount(g_merged))
  for (i in seq_len(ecount(g_merged))) {
    c1 <- E(g_merged)$color_1[i]
    c2 <- E(g_merged)$color_2[i]
    if (!is.na(c1) && !is.na(c2)) {
      # If the edge is in both, pick 'purple' or something else:
      edge_colors[i] <- "purple"
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

# We'll assume there's a 'gender' attribute to color nodes:
vertex_colors <- ifelse(V(g_merged)$depressed_w3 == 1, "gray", "gray")

# For layout, reuse your original layout from g, mapped to g_merged's order
l1 <- layout_with_fr(g) 
l_merged <- l1[match(V(g_merged)$name, V(g)$name), ]

# ------------------------------------------------------------------
# 2) BUILD AN UNDIRECTED GRAPH JUST FOR TRIAD CHECKS
# ------------------------------------------------------------------
g_und <- as.undirected(g_merged, mode="collapse")
E(g_und)$color <- E(g_merged)$color  # keep color

nV <- vcount(g_und)

# Flags for each node:
hasGreenClosed <- logical(nV)  # all-green closed triad
hasRedClosed   <- logical(nV)  # closed triad with 2 green edges + 1 red edge
hasOpen        <- logical(nV)  # open triad from v (two green edges, missing j--k)

# Helper function: neighbors connected to 'v' by a GREEN edge
neighbors_green <- function(graph, v) {
  nbrs <- neighbors(graph, v, mode="all")
  gNbrs <- c()
  for (w in nbrs) {
    e <- E(graph)[ v %--% w ]
    if (length(e) > 0 && e$color == "green") {
      gNbrs <- c(gNbrs, w)
    }
  }
  return(gNbrs)
}

# Detect triads
for (v in seq_len(nV)) {
  gv <- neighbors_green(g_und, v)
  if (length(gv) < 2) next
  
  # Check pairs in gv
  pair_idx <- combn(gv, 2)
  for (col_i in seq_len(ncol(pair_idx))) {
    j <- pair_idx[1, col_i]
    k <- pair_idx[2, col_i]
    e_jk <- E(g_und)[ j %--% k ]
    
    if (length(e_jk) == 0) {
      # No edge => open triad
      hasOpen[v] <- TRUE
    } else {
      # Edge j--k exists; check color
      col_jk <- e_jk$color
      if (col_jk == "green") {
        # all-green closed triad
        hasGreenClosed[v] <- TRUE
      } else if (col_jk == "red") {
        # 2-green-1-red closed triad
        hasRedClosed[v] <- TRUE
      } else if (col_jk == "purple") {
        # If 'purple' means it's both green+red,
        # we treat it as green so it forms an all-green triad
        hasGreenClosed[v] <- TRUE
      }
      # If color is something else (e.g. "gray"), we might ignore
    }
  }
}

# ------------------------------------------------------------------
# 3) ASSIGN EACH NODE TO A CATEGORY WITH THESE RULES:
#     - If node has a 2-green+1-red triad => category = 3
#     - else if node has all-green triad and NO open triad => cat=1
#     - else if node has open triad and NO 2-green+1-red => cat=2
#     - else NA
# ------------------------------------------------------------------
category <- rep(NA, nV)

for (v in seq_len(nV)) {
  if (hasRedClosed[v]) {
    # Plot 3
    category[v] <- 3
  } else {
    # No red triad
    if (hasGreenClosed[v] && !hasOpen[v]) {
      # Plot 1: purely all-green (no open triad)
      category[v] <- 1
    } else if (hasOpen[v] && !hasGreenClosed[v]) {
      # Plot 2: has open triad, no red triad, AND no green closed triad
      category[v] <- 2
    }
  }
}

# ------------------------------------------------------------------
# 4) HIGHLIGHT EDGES IN EACH CATEGORY FOR PLOTTING
# ------------------------------------------------------------------
edges_cat1 <- rep(FALSE, ecount(g_merged))  # highlight for Plot 1
edges_cat2 <- rep(FALSE, ecount(g_merged))  # highlight for Plot 2
edges_cat3 <- rep(FALSE, ecount(g_merged))  # highlight for Plot 3

for (v in seq_len(nV)) {
  gv <- neighbors_green(g_und, v)
  if (length(gv) < 2) next
  
  # All pairs in gv
  pair_idx <- combn(gv, 2)
  for (col_i in seq_len(ncol(pair_idx))) {
    j <- pair_idx[1, col_i]
    k <- pair_idx[2, col_i]
    
    e_jk_und <- E(g_und)[ j %--% k ]  # in the undirected version
    triad_has_edge_jk <- (length(e_jk_und) > 0)
    col_jk <- if (triad_has_edge_jk) e_jk_und$color else NA
    
    # IMPORTANT: Check for NA in category[v]
    #            so we don't do if(category[v] == 1) with category[v]=NA
    if (!is.na(category[v]) && category[v] == 1) {
      # 1) If category[v] == 1 => highlight *all-green closed* triads
      if (triad_has_edge_jk && col_jk %in% c("green","purple")) {
        # v-j green, v-k green, j-k green => highlight
        e_vj <- E(g_merged)[ v %--% j ]
        e_vk <- E(g_merged)[ v %--% k ]
        e_jk <- E(g_merged)[ j %--% k ]
        
        edges_cat1[e_vj] <- TRUE
        edges_cat1[e_vk] <- TRUE
        edges_cat1[e_jk] <- TRUE
      }
    }
    
    if (!is.na(category[v]) && category[v] == 2) {
      # 2) If category[v] == 2 => highlight "open triad" from v
      if (!triad_has_edge_jk) {
        e_vj <- E(g_merged)[ v %--% j ]
        e_vk <- E(g_merged)[ v %--% k ]
        edges_cat2[e_vj] <- TRUE
        edges_cat2[e_vk] <- TRUE
      }
    }
    
    if (!is.na(category[v]) && category[v] == 3) {
      # 3) If category[v] == 3 => highlight 2-green-1-red triad
      if (triad_has_edge_jk && col_jk == "red") {
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

# ------------------------------------------------------------------
# 5) FINALLY, MAKE THE THREE PLOTS
# ------------------------------------------------------------------

## Plot 1: All-green closed triads
edge_col_plot1 <- ifelse(edges_cat1, E(g_merged)$color, "gray")
plot(
  g_merged, layout = l_merged,
  main           = "Friendship Closed Triangles",
  vertex.size    = 5,
  edge.arrow.size= 0.4,
  vertex.label   = NA,
  vertex.color   = vertex_colors,
  edge.color     = edge_col_plot1
)
mtext("(B1)", side = 3, adj = 0, line = 0.2, cex = 1.2)

## Plot 2: Open triads from nodes that do not have 2-green-1-red
##         AND do not have green closed triads
edge_col_plot2 <- ifelse(edges_cat2, E(g_merged)$color, "gray")
plot(
  g_merged, layout = l_merged,
  main           = "Friendship Open Triads",
  vertex.size    = 5,
  edge.arrow.size= 0.4,
  vertex.label   = NA,
  vertex.color   = vertex_colors,
  edge.color     = edge_col_plot2
)
mtext("(B2)", side = 3, adj = 0, line = 0.2, cex = 1.2)


## Plot 3: 2-green-1-red triads
edge_col_plot3 <- ifelse(edges_cat3, E(g_merged)$color, "gray")
plot(
  g_merged, layout = l_merged,
  main           = "Closed Triads with friends enemies",
  vertex.size    = 5,
  edge.arrow.size= 0.4,
  vertex.label   = NA,
  vertex.color   = vertex_colors,
  edge.color     = edge_col_plot3
)


############################################
# 4. PANELS 5-6: TWO BARPLOTS (LARGE NUMBERS)
############################################
par(mar = c(3, 2, 2, 1))
Tri_vs_Enemies <- matrix(
  c(9188, 7181, 3614,
    3249, 1282, 652,
    6866, 4393, 1947,
    2255, 510, 268),
  nrow = 4, ncol = 3, byrow = TRUE)
rownames(Tri_vs_Enemies) <- c("W_Closed", "W_Enemies", "M_Closed", "M_Enemies")
colnames(Tri_vs_Enemies) <- c("Wave1", "Wave3", "Wave4")
Tri_Closed_Open <- matrix(
  c(9188, 7181, 3614,
    1699, 2195, 890,
    6866, 4393, 1947,
    1224, 1266, 569),
  nrow = 4, ncol = 3, byrow = TRUE)
rownames(Tri_Closed_Open) <- c("W_Closed", "W_Open", "M_Closed", "M_Open")
colnames(Tri_Closed_Open) <- c("Wave1", "Wave3", "Wave4")

# ----- PANEL 5: Left barplot (Triangles vs Enemies)
bp1 <- barplot(
  Tri_vs_Enemies,
  beside = TRUE,
  col = c("darkgreen", "red", "darkgreen", "red"),
  ylim = c(0, 10500),
  main = "Number of Motifs",
  axes = FALSE,  xaxt = "n"
)
axis(2)
box()
text(x = bp1, y = Tri_vs_Enemies, labels = Tri_vs_Enemies, pos = 3, cex = 0.8)
x_women1 <- colMeans(bp1[1:2, ])
x_men1   <- colMeans(bp1[3:4, ])
text(x = x_women1, y = -1500, labels = "Women", xpd = TRUE, srt = 20, adj = 0.5)
text(x = x_men1,   y = -1500, labels = "Men",   xpd = TRUE, srt = 20, adj = 0.5)
legend("topright", legend = c("Closed", "Adversaries"), fill = c("darkgreen", "red"), bty = "n")
mtext("(C1)", side = 3, adj = 0, line = 0.4, cex = 1.2)

# ----- PANEL 6: Right barplot (Closed vs Open)
bp2 <- barplot(
  Tri_Closed_Open,
  beside = TRUE,
  col = c("darkgreen", "lightgreen", "darkgreen", "lightgreen"),
  ylim = c(0, 10000),
  main = "Number of Motifs",
  axes = FALSE,  xaxt = "n"
)
axis(2)
box()
text(x = bp2, y = Tri_Closed_Open, labels = Tri_Closed_Open, pos = 3, cex = 0.8)
x_women2 <- colMeans(bp2[1:2, ])
x_men2   <- colMeans(bp2[3:4, ])
text(x = x_women2, y = -1500, labels = "Women", xpd = TRUE, srt = 20, adj = 0.5)
text(x = x_men2,   y = -1500, labels = "Men",   xpd = TRUE, srt = 20, adj = 0.5)
legend("topright", legend = c("Closed","Open"), fill = c("darkgreen", "lightgreen"), bty = "n")
mtext("(C2)", side = 3, adj = 0, line = 0.4, cex = 1.2)

############################################
# 5. PANELS 7-8: TWO BARPLOTS (FRACTIONAL VALUES)
############################################
par(mar = c(4, 2, 1, 1))
Tri_vs_Enemies_frac <- matrix(
  c(0.402, 0.422, 0.412,
    0.450, 0.523, 0.446,
    0.272, 0.298, 0.273,
    0.301, 0.369, 0.302),
  nrow = 4, ncol = 3, byrow = TRUE)
rownames(Tri_vs_Enemies_frac) <- c("W_Closed", "W_Enemies", "M_Closed", "M_Enemies")
colnames(Tri_vs_Enemies_frac) <- c("Wave1", "Wave3", "Wave4")
Tri_Closed_Open_frac <- matrix(
  c(0.402, 0.422, 0.412,
    0.359, 0.410, 0.380,
    0.272, 0.298, 0.273,
    0.260, 0.273, 0.253),
  nrow = 4, ncol = 3, byrow = TRUE)
rownames(Tri_Closed_Open_frac) <- c("W_Closed", "W_Open", "M_Closed", "M_Open")
colnames(Tri_Closed_Open_frac) <- c("Wave1", "Wave3", "Wave4")

# ----- PANEL 7: Left barplot (Triangles vs Enemies, fractional)
bp3 <- barplot(
  Tri_vs_Enemies_frac,
  beside = TRUE,
  col = c("darkgreen", "red", "darkgreen", "red"),
  ylim = c(0, 1),
  main = "Average Depression",
  axes = FALSE,  xaxt = "n"
)
axis(2)
box()
text(x = bp3, y = Tri_vs_Enemies_frac, labels = Tri_vs_Enemies_frac, pos = 3, cex = 0.8)
x_women3 <- colMeans(bp3[1:2, ])
x_men3   <- colMeans(bp3[3:4, ])
text(x = x_women3, y = -0.13, labels = "Women", xpd = TRUE, srt = 20, adj = 0.5)
text(x = x_men3,   y = -0.13, labels = "Men",   xpd = TRUE, srt = 20, adj = 0.5)
wavePos3 <- colMeans(bp3)
axis(side = 1, at = wavePos3, labels = colnames(Tri_vs_Enemies_frac), line = 1.5, tick = FALSE)
legend("topright", legend = c("Closed", "Adversaries"), fill = c("darkgreen", "red"), bty = "n")
mtext("(D1)", side = 3, adj = 0, line = 0.4, cex = 1.2)

# ----- PANEL 8: Right barplot (Closed vs Open, fractional)
bp4 <- barplot(
  Tri_Closed_Open_frac,
  beside = TRUE,
  col = c("darkgreen", "lightgreen", "darkgreen", "lightgreen"),
  ylim = c(0, 1),
  main = "Average Depression",
  axes = FALSE,  xaxt = "n"
)
axis(2)
box()
text(x = bp4, y = Tri_Closed_Open_frac, labels = Tri_Closed_Open_frac, pos = 3, cex = 0.8)
x_women4 <- colMeans(bp4[1:2, ])
x_men4   <- colMeans(bp4[3:4, ])
text(x = x_women4, y = -0.13, labels = "Women", xpd = TRUE, srt = 20, adj = 0.5)
text(x = x_men4,   y = -0.13, labels = "Men",   xpd = TRUE, srt = 20, adj = 0.5)
wavePos4 <- colMeans(bp4)
axis(side = 1, at = wavePos4, labels = colnames(Tri_Closed_Open_frac), line = 1.5, tick = FALSE)
legend("topright", legend = c("Closed","Open"), fill = c("darkgreen", "lightgreen"), bty = "n")
mtext("(D2)", side = 3, adj = 0, line = 0.4, cex = 1.2)



dev.off()

#file.exists("/WORKAREA/work/HONDURAS_GATES/M_PAPAMICHALIS/Depression/Code/Results/test.png")

