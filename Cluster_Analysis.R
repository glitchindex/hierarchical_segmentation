# ===================================================================
#
#       COMPREHENSIVE HIERARCHICAL & K-PROTOTYPES CLUSTER ANALYSIS
#
# ===================================================================
#
# Description:
# This script performs a comprehensive cluster analysis on a mixed-datatype
# dataset. It includes data preparation, methods for determining the
# optimal number of clusters, two different clustering algorithms
# (Hierarchical and K-Prototypes), and detailed cluster profiling.
#
# ===================================================================


# 1. SETUP
# -------------------------------------------------------------------
#
# This section installs and loads all the necessary packages for the
# analysis. The 'pacman' package helps manage packages efficiently.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # For data manipulation and visualization (includes ggplot2, dplyr)
  cluster,      # For calculating Gower's distance (daisy function)
  factoextra,   # For clustering visualization (fviz_dend, fviz_nbclust)
  dendextend,   # For enhancing dendrograms
  clustMixType, # For K-Prototypes clustering
  RColorBrewer, # For color palettes
  ggplot2,      # For beautiful visualizations
  FactoMineR    # For FAMD changes
)

library(cluster)
library(dplyr)
library(tidyverse)
library(pacman)
library(factoextra)
library(dendextend)
library(clustMixType)
library(RColorBrewer)
library(FactoMineR)
library(ggplot2)

# Load your dataset
# Replace the path with the correct file location 
df <- read_csv("C:/Users/dummy/Desktop/dummy_dataset.csv") 


# 2. DATA PREPARATION
# -------------------------------------------------------------------
#
# Here, we clean the data to make it suitable for clustering. This
# involves removing identifiers, handling missing values, and ensuring
# variables have the correct data type.

# Take a look at the raw data structure
print("Original Data Structure:")
str(df)

# Create a clean version of the dataframe
df_clean <- df %>%
  select(-id) %>%  # Remove the 'id' column, as it's not a feature for clustering
  mutate_if(is.character, as.factor) # Convert character columns to factors

# Handle missing values. This version removes any rows that contain NA
# (missing values) in any of the columns.
df_clean <- df_clean %>% drop_na()

# Review the structure of the cleaned data
print("Cleaned Data Structure:")
str(df_clean)


# 3. DETERMINE THE OPTIMAL NUMBER OF CLUSTERS (k)
# -------------------------------------------------------------------
#
# Before clustering, it's crucial to estimate the best number of
# clusters (k). We'll use three popular methods: the Elbow method,
# the Silhouette method, and the Gap Statistic.

# Calculate Gower's distance matrix. This is necessary because our
# data has both numeric and categorical variables.
gower_dist <- daisy(df_clean, metric = "gower")
gower_mat <- as.matrix(gower_dist) # Convert to matrix for compatibility

# Method A: Elbow Method (using Within-Cluster Sum of Squares)
# INTERPRETATION: We manually (subjectively) determine where our graph begins to even out
# by looking for the "elbow"
print("Running Elbow Method...")
fviz_nbclust(gower_mat, hcut, method = "wss") +
  labs(subtitle = "Elbow Method (Visual Inspection)")

# Method B: Silhouette Method
# This measures how well-separated the clusters are. Look for the highest peak.
# INTERPRETATION: The RED vertical dashed line in
# --- Method B: Silhouette Method with a Dynamic Red Line ---
# This method has a clear optimum (the highest score), which we can find programmatically.
print("Running Silhouette Method...")
sil_plot <- fviz_nbclust(gower_mat, hcut, method = "silhouette")

# Extract the data from the plot object
sil_data <- sil_plot$data

# Find the number of clusters with the highest average silhouette width
# The 'which.max()' function finds the index of the highest 'y' value (the silhouette score)
optimal_k_sil <- sil_data$clusters[which.max(sil_data$y)]
# The cluster count can be a factor, so we convert it to a number
optimal_k_sil <- as.numeric(as.character(optimal_k_sil))

print(paste("Optimal number of clusters according to Silhouette Method:", optimal_k_sil))

# Re-draw the plot with a red line at the dynamically found optimal k
sil_plot +
  geom_vline(xintercept = optimal_k_sil, linetype = 2, color = "red") +
  labs(subtitle = paste("Silhouette Method (Optimal k =", optimal_k_sil, ")"))

# Method C: Gap Statistic
# Compares the observed within-cluster variation to what we'd expect
# from a random uniform distribution. Look for the highest gap statistic.
# Note: This can be computationally intensive.
# INTERPRETATION: The vertical dashed line indicates "ideal" number for K
print("Running Gap Statistic... (this may take a moment)")
gap_stat <- clusGap(gower_mat, FUNcluster = hcut, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(subtitle = "Gap Statistic Method (for Hierarchical Clustering)")

# NEXT STEPS: Review the plots from these three methods. The "optimal" k
# is often the number suggested by at least two of the methods.
# Replace the number below with the number of clusters provided in the analyses above
OPTIMAL_K <- 6


# 4. CLUSTERING - METHOD 1: HIERARCHICAL CLUSTERING
# -------------------------------------------------------------------
#
# This is a bottom-up approach where each data point starts in its own
# cluster and pairs are merged as one moves up the hierarchy.

print("Performing Hierarchical Clustering...")
# We use the Gower distance matrix calculated earlier.
# "ward.D2" is a good general-purpose linkage method that creates compact clusters.
hc <- hclust(gower_dist, method = "ward.D2")

# Visualize the dendrogram
# This helps to see the hierarchical structure and validate the chosen k.
# Dynamically create a color palette with exactly OPTIMAL_K colors.
# "Set3" is a palette designed for visually distinct categorical colors.
# Note: brewer.pal() has a limit, so for K > 12 this simple approach will warn you.

color_palette <- brewer.pal(n = OPTIMAL_K, name = "Set3")


# Now, use this dynamic palette in your plot function
fviz_dend(hc, k = OPTIMAL_K,
          cex = 0.6,
          k_colors = color_palette, # Use the generated palette here
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Hierarchical Clustering Dendrogram",
          xlab = "Audience", 
          sub = "Height indicates the distance between clusters"
)

# Cut the tree to get the cluster assignments
hc_clusters <- cutree(hc, k = OPTIMAL_K)


# 5. CLUSTERING - METHOD 2: K-PROTOTYPES
# -------------------------------------------------------------------
#
# This is a partitioning method that is well-suited for mixed data types
# and is often more efficient for larger datasets than hierarchical clustering.

print("Performing K-Prototypes Clustering...")
kproto_result <- kproto(df_clean, k = OPTIMAL_K, nstart = 25, iter.max = 100)

# Get the cluster assignments
kproto_clusters <- kproto_result$cluster




# Perform FAMD for dimensionality reduction.
# This function creates the 2D map from your mixed data.
# We use 'df_clean' which is your fully cleaned data.
famd_result <- FAMD(df_clean, graph = FALSE)

# We'll take the coordinates from the FAMD result and add the
# cluster assignments from your k-prototypes model.
plot_data <- data.frame(
  Dim.1   = famd_result$ind$coord[, 1],
  Dim.2   = famd_result$ind$coord[, 2],
  cluster = as.factor(kproto_result$cluster)
)


# Create the plot using ggplot2 library.
ggplot(plot_data, aes(x = Dim.1, y = Dim.2, color = cluster)) +
  geom_point(alpha = 0.8) +
  stat_ellipse(aes(group = cluster), type = 't', level = 0.95) +
  labs(
    title = "K-Prototypes Clusters Visualized on FAMD Map",
    x = "Dimension 1 (Summarizes most variation)",
    y = "Dimension 2 (Summarizes second-most variation)",
    color = "Cluster"
  ) +
  theme_bw()

# 6. CLUSTER PROFILING AND ANALYSIS
# -------------------------------------------------------------------
#
# Now we'll add the cluster assignments from both methods back to the
# original data. Analyzing them side-by-side helps validate our findings.

# Add assignments from both Hierarchical (hc) and K-Prototypes (kp) clustering
# to the SAME dataframe that was used for clustering ('df_final').
df_clustered <- df_clean %>%
  mutate(
    h_cluster = hc_clusters,
    k_cluster = kproto_clusters
  )

# You can now proceed to analyze 'df_clustered', which contains both sets of results.
# For example, viewing the first few rows:
head(df_clustered)


# --- Profile 1: Hierarchical Clustering Results ---

print("High-Level Summary: HIERARCHICAL CLUSTERING")

cluster_summary_hc <- df_clustered %>%
  group_by(h_cluster) %>%
  summarise(
    count = n(),
    avg_ltv = mean(ltv, na.rm = TRUE),
    avg_cac = mean(cac, na.rm = TRUE),
    most_common_adoption = names(sort(table(adoption), decreasing = TRUE))[1],
    most_common_trust = names(sort(table(trust_reference), decreasing = TRUE))[1]
  ) %>%
  arrange(h_cluster)

print(cluster_summary_hc)


# --- Profile 2: K-Prototypes Clustering Results ---

print("High-Level Summary: K-PROTOTYPES CLUSTERING")

cluster_summary_kp <- df_clustered %>%
  group_by(k_cluster) %>%
  summarise(
    count = n(),
    avg_ltv = mean(ltv, na.rm = TRUE),
    avg_cac = mean(cac, na.rm = TRUE),
    most_common_adoption = names(sort(table(adoption), decreasing = TRUE))[1],
    most_common_trust = names(sort(table(trust_reference), decreasing = TRUE))[1]
  ) %>%
  arrange(k_cluster)

print(cluster_summary_kp)

# --- Deeper Visual Profiling ---

# Visualize numeric variables across clusters (e.g., income)
print("Generating visual profiles...")
ggplot(df_clustered, aes(x = factor(h_cluster), y = income, fill = factor(h_cluster))) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Income Distribution by Cluster", x = "Cluster", y = "Income") +
  theme_minimal() +
  guides(fill = "none") # Remove legend for fill

# Visualize categorical variables across clusters (e.g., adoption)
ggplot(df_clustered, aes(x = factor(h_cluster), fill = adoption)) +
  geom_bar(position = "fill") +
  labs(title = "Adoption Profile by Cluster", x = "Cluster", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


# 7. SAVE THE RESULTS
# -------------------------------------------------------------------
#
# Finally, save your dataframe with the new cluster assignments
# to a CSV file for further analysis or use in other tools.
# For example, drop this .csv file along with the accompanying prompt in the .README
# into your LLM of choice. 
# (for best results, Gemini Pro 2.5 is recommended // 2025.08.12)

write.csv(df_clustered, "C:/Users/dummy/Desktop/hierarchical_cluster.csv", row.names = FALSE)

# ===================================================================
#                                END OF SCRIPT
# ===================================================================
