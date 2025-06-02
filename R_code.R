# ===========================================================
#    R Code for Cluster Analysis in 
#    Zoghbour et al. (2025)
# ===========================================================

# Last modified: June 2, 2025


# ===========================================================
#    LOAD REQUIRED PACKAGES for Dubai Apartment and Villas
# ===========================================================

# Install these only ONCE (you can comment these lines after installing)
# install.packages(c("readxl", "dplyr", "cluster", "factoextra", "dbscan", "writexl", "ggplot2", "plotly"))
# Load libraries
# install.packages("tidyverse")
# install.packages("ggrepel")
install.packages("clusterCrit")
library(tidyverse)
library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(dbscan)
library(writexl)
library(ggplot2)
library(plotly)
library(ggrepel)
library(clusterCrit)

install.packages("clusterSim")   # run once if you don't have it
library(clusterSim)

# ======================================================
#             APARTMENTS ANALYSIS
# ======================================================

# ---------------------------
# 1. Read and View Data
# ---------------------------

data_apartment <- read_excel("~/Downloads/data paper.xlsx", sheet = "Apartment")
View(data_apartment)


# ---------------------------
# 2. Filter Relevant Columns
# ---------------------------

data_filtered <- data_apartment %>% select("Rent", "Beds", "Baths", "Area_in_sqft")
data_filtered2 <- data_apartment %>% select("Rent", "Beds", "Baths", "Area_in_sqft", "Location")

# ---------------------------
# 3. Normalize Data (Z-Score)
# ---------------------------

data_normalized <- as.data.frame(scale(data_filtered))
View(data_normalized)

# ======================================================
#             K-MEANS CLUSTERING
# ======================================================


fviz_nbclust(data_normalized, kmeans, method = "wss")
fviz_nbclust(data_normalized, kmeans, method = "silhouette")
# Set seed for reproducibility
set.seed(123)

# Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clus-tering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.
kmeans_result <- kmeans(data_normalized, centers = 8, nstart = 25)
data_apartment$KMeans_Cluster <- kmeans_result$cluster


# Caliński, T., Harabasz, J. (1974). A dendrite method for cluster analysis. Communications in Statistics - Theory and Methods, 3(1), 1-27.
# Davies, D. L., Bouldin, D. W. (2009). A cluster separation measure. IEEE Transactions on Pattern Analysis and Machine Intelligence, (2), 224-227.
intCriteria(as.matrix(data_normalized), kmeans_result$cluster,
            c("Calinski_Harabasz", "Davies_Bouldin"))


cluster_summary <- data_apartment %>%
  group_by(KMeans_Cluster) %>%
  summarise(across(c(Rent, Beds, Baths, Area_in_sqft,Location), mean))
print(cluster_summary)


# Perform K-Means with 8 clusters
kmeans_result <- kmeans(data_normalized, centers = 8, nstart = 25)
print(kmeans_result)

# Add KMeans Cluster Labels
data_apartment$KMeans_Cluster <- kmeans_result$cluster

# View updated data
View(data_apartment)

# Silhouette Coefficient
silhouette_values <- silhouette(kmeans_result$cluster, dist(data_normalized))
cat("Average Silhouette Score (KMeans):", mean(silhouette_values[, 3]), "\n")

# Cluster Size Table
table(data_apartment$KMeans_Cluster)

# NEW SCATTER PLOTS 
top_points <- data_apartment %>%
  group_by(KMeans_Cluster) %>%
  slice_max(Rent, n = 1)

ggplot(data_apartment, aes(x = Area_in_sqft, y = Rent, color = factor(KMeans_Cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(data = top_points, aes(label = Location), size = 3, show.legend = FALSE) +
  labs(title = "Apartment K-Means Clustering Rent vs Area(sq ft)",
       color = "Cluster", x = "Area (sq ft)", y = "Rent") +
  theme_minimal()

ggplot(data_apartment, aes(x = Baths, y = Rent, color = factor(KMeans_Cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(data = top_points, aes(label = Location), size = 3, show.legend = FALSE) +
  labs(title = "Apartment K-Means Clustering Rent vs Baths",
       color = "Cluster", x = "Baths", y = "Rent") +
  theme_minimal()

ggplot(data_apartment, aes(x = Beds, y = Rent, color = factor(KMeans_Cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(data = top_points, aes(label = Location), size = 3, show.legend = FALSE) +
  labs(title = "Apartment K-Means Clustering Rent vs Beds",
       color = "Cluster", x = "Beds", y = "Rent") +
  theme_minimal()

# Scatter Plot (Interactive)
p_kmeans <- ggplot(data_apartment, aes(x = Rent, y = Area_in_sqft, color = factor(KMeans_Cluster))) +
  geom_point(size = 2) +
  labs(title = "Interactive K-Means Clustering", color = "Cluster") +
  theme_minimal()

ggplotly(p_kmeans)

# Group By Cluster and summarize
data_apartment$KMeans_Cluster <- as.factor(data_apartment$KMeans_Cluster)

data_apartment <- data_apartment %>%
  mutate(Direction = location_direction[Location])


kmeans_apartment_cluster_summary <- data_apartment %>%
  group_by(KMeans_Cluster) %>%
  summarise(
    mean_rent  = mean(Rent, na.rm = TRUE),
    mean_beds  = mean(Beds, na.rm = TRUE),
    mean_baths = mean(Baths, na.rm = TRUE),
    mean_area  = mean(Area_in_sqft, na.rm = TRUE),
    top_locations = paste(names(sort(table(Location), decreasing = TRUE)[1:3]), collapse = ", "),
    top_directions = paste(names(sort(table(Direction), decreasing = TRUE)[1:2]), collapse = ", ")
  )
View(kmeans_apartment_cluster_summary)
print(cluster_summary)

# ======================================================
#             DBSCAN CLUSTERING
# ======================================================

kNNdistplot(data_normalized, k = 4)  # 4 = minPts
abline(h = 0.25, col = "red")

eps_values <- c(0.2, 0.25, 0.3)
minPts_values <- c(3, 4, 5)

for (eps in eps_values) {
  for (minPts in minPts_values) {
    result <- dbscan(data_normalized, eps = eps, minPts = minPts)
    cat("eps =", eps, "minPts =", minPts,
        "clusters =", length(unique(result$cluster)) - 1,
        "noise points =", sum(result$cluster == 0), "\n")
  }
}



# Perform DBSCAN
dbscan_result <- dbscan(data_normalized, eps = 0.25, minPts = 4)
print(dbscan_result)

# Add DBSCAN Cluster Labels
data_apartment$DBSCAN_Cluster <- dbscan_result$cluster

# Scatter Plot for DBSCAN (Static)
ggplot(data_apartment, aes(x = Rent, y = Area_in_sqft, color = factor(DBSCAN_Cluster))) +
  geom_point(size = 2) +
  labs(title = "DBSCAN Clustering Scatter Plot", color = "DBSCAN Cluster") +
  theme_minimal()

# Scatter Plot for DBSCAN (Interactive)
p_dbscan <- ggplot(data_apartment, aes(x = Rent, y = Area_in_sqft, color = factor(DBSCAN_Cluster))) +
  geom_point(size = 2) +
  labs(title = "Interactive DBSCAN Clustering", color = "DBSCAN Cluster") +
  theme_minimal()

ggplotly(p_dbscan)

top_outliers <- data_apartment %>%
  filter(DBSCAN_Cluster == 0) %>%
  slice_max(Rent, n = 12)     # top outliers

ggplot(data_apartment, aes(x = Area_in_sqft, y = Rent,
                       color = factor(DBSCAN_Cluster == 0),
                       shape = factor(DBSCAN_Cluster == 0))) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_text_repel(data = top_outliers,
                  aes(label = Location),
                  size = 3, color = "black", show.legend = FALSE,
                  max.overlaps = Inf) +
  scale_colour_manual(values = c("FALSE" = "#6c757d", "TRUE" = "firebrick"),
                      labels  = c("Clustered points", "Noise / outliers")) +
  scale_shape_manual(values  = c("FALSE" = 16, "TRUE" = 4),
                     labels  = c("Clustered points", "Noise / outliers")) +
  labs(title = "DBSCAN – Apartments Rent vs Area (sq ft)",
       x = "Area (sq ft)", y = "Rent (AED)",
       colour = "", shape = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggplot(data_apartment, aes(x = Baths, y = Rent,
                       color = factor(DBSCAN_Cluster == 0),
                       shape = factor(DBSCAN_Cluster == 0))) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_text_repel(data = top_outliers,
                  aes(label = Location),
                  size = 3, color = "black", show.legend = FALSE,
                  max.overlaps = Inf) +
  scale_colour_manual(values = c("FALSE" = "#6c757d", "TRUE" = "firebrick"),
                      labels  = c("Clustered points", "Noise / outliers")) +
  scale_shape_manual(values  = c("FALSE" = 16, "TRUE" = 4),
                     labels  = c("Clustered points", "Noise / outliers")) +
  labs(title = "DBSCAN – Apartments Rent vs Baths",
       x = "Baths", y = "Rent (AED)",
       colour = "", shape = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggplot(data_apartment, aes(x = Beds, y = Rent,
                       color = factor(DBSCAN_Cluster == 0),
                       shape = factor(DBSCAN_Cluster == 0))) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_text_repel(data = top_outliers,
                  aes(label = Location),
                  size = 3, color = "black", show.legend = FALSE,
                  max.overlaps = Inf) +
  scale_colour_manual(values = c("FALSE" = "#6c757d", "TRUE" = "firebrick"),
                      labels  = c("Clustered points", "Noise / outliers")) +
  scale_shape_manual(values  = c("FALSE" = 16, "TRUE" = 4),
                     labels  = c("Clustered points", "Noise / outliers")) +
  labs(title = "DBSCAN – Apartments Rent vs Beds",
       x = "Beds", y = "Rent (AED)",
       colour = "", shape = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")


dbscan_apartment_cluster_summary <- data_apartment %>%
  group_by(DBSCAN_Cluster) %>%
  summarise(
    mean_rent  = mean(Rent, na.rm = TRUE),
    mean_beds  = mean(Beds, na.rm = TRUE),
    mean_baths = mean(Baths, na.rm = TRUE),
    mean_area  = mean(Area_in_sqft, na.rm = TRUE),
    top_locations = paste(names(sort(table(Location), decreasing = TRUE)[1:3]), collapse = ", "),
    top_directions = paste(names(sort(table(Direction), decreasing = TRUE)[1:2]), collapse = ", ")

  )
View(dbscan_apartment_cluster_summary)

# ======================================================


# ======================================================
#             VILLAS ANALYSIS
# ======================================================

# ---------------------------
# 1. Read and View Data
# ---------------------------

data_villa <- read_excel("~/Downloads/data paper.xlsx", sheet = "Villa")
View(data_villa)

# ---------------------------
# 2. Filter Relevant Columns
# ---------------------------

data_villa_filtered <- data_villa %>% select("Rent", "Beds", "Baths", "Area_in_sqft")

# ---------------------------
# 3. Normalize Data (Z-Score)
# ---------------------------

data_villa_normalized <- as.data.frame(scale(data_villa_filtered))
View(data_villa_normalized)

# ======================================================
#             VILLAS - K-MEANS CLUSTERING
# ======================================================


fviz_nbclust(data_villa_normalized, kmeans, method = "silhouette")

set.seed(123)
kmeans_villa <- kmeans(data_villa_normalized, centers = 3, nstart = 25)
print(kmeans_villa)

data_villa$KMeans_Cluster <- kmeans_villa$cluster
View(data_villa)

# Caliński, T., Harabasz, J. (1974). A dendrite method for cluster analysis. Communications in Statistics - Theory and Methods, 3(1), 1-27.
# Davies, D. L., Bouldin, D. W. (2009). A cluster separation measure. IEEE Transactions on Pattern Analysis and Machine Intelligence, (2), 224-227.
criteria_villa <- intCriteria(as.matrix(data_villa_normalized), kmeans_villa$cluster,
                              c("Calinski_Harabasz", "Davies_Bouldin"))
criteria_villa


silhouette_values_villa <- silhouette(kmeans_villa$cluster, dist(data_villa_normalized))
cat("Average Silhouette Score (Villas - KMeans):", mean(silhouette_values_villa[, 3]), "\n")

table(data_villa$KMeans_Cluster)

centroids_3 <- data_villa %>% 
  group_by(KMeans_Cluster) %>% 
  summarise(n        = n(),
            rent     = round(mean(Rent)),
            area     = round(mean(Area_in_sqft)),
            .groups = "drop")
View(centroids_3)  


ggplot(data_villa, aes(x = Rent, y = Area_in_sqft, color = factor(KMeans_Cluster))) +
  geom_point(size = 2) +
  labs(title = "Villas - K-Means Clustering", color = "Cluster") +
  theme_minimal()

p_kmeans_villa <- ggplot(data_villa, aes(x = Rent, y = Area_in_sqft, color = factor(KMeans_Cluster))) +
  geom_point(size = 2) +
  labs(title = "Interactive Villas - KMeans Clustering", color = "Cluster") +
  theme_minimal()

ggplotly(p_kmeans_villa)

# NEW SCATTER PLOTS 
# Get the top-rent villa from each cluster
top_villas <- data_villa %>%
  group_by(KMeans_Cluster) %>%
  slice_max(Rent, n = 1)

# Create the scatter plot
ggplot(data_villa, aes(x = Area_in_sqft, y = Rent, color = factor(KMeans_Cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(data = top_villas, aes(label = Location), size = 3, show.legend = FALSE) +
  labs(
    title = "Villa K-Means Clustering Rent vs Area(sq ft)",
    color = "Cluster",
    x = "Area (sq ft)",
    y = "Rent (AED)"
  ) +
  theme_minimal()

ggplot(data_villa, aes(x = Baths, y = Rent, color = factor(KMeans_Cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(data = top_villas, aes(label = Location), size = 3, show.legend = FALSE) +
  labs(
    title = "Villa K-Means Clustering Rent vs Baths",
    color = "Cluster",
    x = "Baths",
    y = "Rent (AED)"
  ) +
  theme_minimal()

ggplot(data_villa, aes(x = Beds, y = Rent, color = factor(KMeans_Cluster))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(data = top_villas, aes(label = Location), size = 3, show.legend = FALSE) +
  labs(
    title = "Villa K-Means Clustering Rent vs Beds",
    color = "Cluster",
    x = "Beds",
    y = "Rent (AED)"
  ) +
  theme_minimal()


#-----------
data_villa$KMeans_Cluster <- as.factor(data_villa$KMeans_Cluster)

kmeans_villa_cluster_summary <- data_villa %>%
  group_by(KMeans_Cluster) %>%
  summarise(
    mean_rent  = mean(Rent, na.rm = TRUE),
    mean_beds  = mean(Beds, na.rm = TRUE),
    mean_baths = mean(Baths, na.rm = TRUE),
    mean_area  = mean(Area_in_sqft, na.rm = TRUE),
    top_locations = paste(names(sort(table(Location), decreasing = TRUE)[1:3]), collapse = ", "),
    top_directions = paste(names(sort(table(Direction), decreasing = TRUE)[1:2]), collapse = ", ")
    
  )

View(kmeans_villa_cluster_summary)

# ======================================================
#             VILLAS - DBSCAN CLUSTERING
# ======================================================

kNNdistplot(data_villa_normalized, k = 4)
abline(h = 0.3, col = "red")

dbscan_villa <- dbscan(data_villa_normalized, eps = 0.3, minPts = 4)
print(dbscan_villa)

data_villa$DBSCAN_Cluster <- dbscan_villa$cluster
view(data_villa)

eps_values <- c(0.2, 0.25, 0.3, 0.35, 0.4)
minPts_values <- c(4)


for (eps in eps_values) {
  for (minPts in minPts_values) {
    result <- dbscan(data_villa_normalized, eps = eps, minPts = minPts)
    cat("eps =", eps, "minPts =", minPts,
        "clusters =", length(unique(result$cluster)) - 1,
        "noise points =", sum(result$cluster == 0), "\n")
  }
}



ggplot(data_villa, aes(x = Rent, y = Area_in_sqft, color = factor(DBSCAN_Cluster))) +
  geom_point(size = 2) +
  labs(title = "Villas - DBSCAN Clustering", color = "DBSCAN Cluster") +
  theme_minimal()

p_dbscan_villa <- ggplot(data_villa, aes(x = Rent, y = Area_in_sqft, color = factor(DBSCAN_Cluster))) +
  geom_point(size = 2) +
  labs(title = "Interactive Villas - DBSCAN Clustering", color = "DBSCAN Cluster") +
  theme_minimal()

ggplotly(p_dbscan_villa)


villa_plot <- data_villa %>%
  mutate(Noise = dbscan_villa$cluster == 0)

top_villa_outliers <- villa_plot %>%
  filter(DBSCAN_Cluster == 0) %>%
  slice_max(Rent, n = 9)

ggplot(villa_plot, aes(x = Area_in_sqft, y = Rent,
                       color = factor(DBSCAN_Cluster == 0),
                       shape = factor(DBSCAN_Cluster == 0))) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_text_repel(data = top_villa_outliers,
                  aes(label = Location),
                  size = 3, color = "black", show.legend = FALSE,
                  max.overlaps = Inf) +
  scale_colour_manual(values = c("FALSE" = "#6c757d", "TRUE" = "firebrick"),
                      labels  = c("Clustered points", "Noise / outliers")) +
  scale_shape_manual(values  = c("FALSE" = 16, "TRUE" = 4),
                     labels  = c("Clustered points", "Noise / outliers")) +
  labs(title = "DBSCAN – Villas Rent vs Area (sq ft)",
       x = "Area (sq ft)", y = "Rent (AED)",
       colour = "", shape = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggplot(villa_plot, aes(x = Baths, y = Rent,
                       color = factor(DBSCAN_Cluster == 0),
                       shape = factor(DBSCAN_Cluster == 0))) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_text_repel(data = top_villa_outliers,
                  aes(label = Location),
                  size = 3, color = "black", show.legend = FALSE,
                  max.overlaps = Inf) +
  scale_colour_manual(values = c("FALSE" = "#6c757d", "TRUE" = "firebrick"),
                      labels  = c("Clustered points", "Noise / outliers")) +
  scale_shape_manual(values  = c("FALSE" = 16, "TRUE" = 4),
                     labels  = c("Clustered points", "Noise / outliers")) +
  labs(title = "DBSCAN – Villas Rent vs Baths",
       x = "Baths", y = "Rent (AED)",
       colour = "", shape = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggplot(villa_plot, aes(x = Beds, y = Rent,
                       color = factor(DBSCAN_Cluster == 0),
                       shape = factor(DBSCAN_Cluster == 0))) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_text_repel(data = top_villa_outliers,
                  aes(label = Location),
                  size = 3, color = "black", show.legend = FALSE,
                  max.overlaps = Inf) +
  scale_colour_manual(values = c("FALSE" = "#6c757d", "TRUE" = "firebrick"),
                      labels  = c("Clustered points", "Noise / outliers")) +
  scale_shape_manual(values  = c("FALSE" = 16, "TRUE" = 4),
                     labels  = c("Clustered points", "Noise / outliers")) +
  labs(title = "DBSCAN – Villas Rent vs Beds",
       x = "Beds", y = "Rent (AED)",
       colour = "", shape = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

dbscan_villa_cluster_summary <- data_villa %>%
  group_by(DBSCAN_Cluster) %>%
  summarise(
    mean_rent  = mean(Rent, na.rm = TRUE),
    mean_beds  = mean(Beds, na.rm = TRUE),
    mean_baths = mean(Baths, na.rm = TRUE),
    mean_area  = mean(Area_in_sqft, na.rm = TRUE),
    top_locations = paste(names(sort(table(Location), decreasing = TRUE)[1:3]), collapse = ", "),
    top_directions = paste(names(sort(table(Direction), decreasing = TRUE)[1:2]), collapse = ", ")

  )
View(dbscan_villa_cluster_summary)

# ======================================================
#             SAVE VILLAS RESULT (Optional)
# ======================================================

write_xlsx(data_villa, "villas_clustered.xlsx")


# ----------  Davies–Bouldin helper (drops DBSCAN noise)  ----------
dbi_clean <- function(xmat, labels){
  keep <- labels != 0                     # remove noise (label 0)
  index.DB(xmat[keep, ], labels[keep])$DB # clusterCrit::index.DB
}
# ----------  Davies–Bouldin comparison – Apartments ----------
db_kmeans_apt  <- intCriteria(as.matrix(data_normalized),
                              kmeans_result$cluster,
                              "Davies_Bouldin")$davies_bouldin

db_dbscan_apt  <- dbi_clean(as.matrix(data_normalized),
                            dbscan_result$cluster)

cat("\nDavies-Bouldin Index (Apartments)\n",
    "• K-means  :", round(db_kmeans_apt, 3), "\n",
    "• DBSCAN   :", round(db_dbscan_apt, 3), "\n")


# ----------  Davies–Bouldin comparison – Villas ----------
db_kmeans_vil <- intCriteria(as.matrix(data_villa_normalized),
                             kmeans_villa$cluster,
                             "Davies_Bouldin")$davies_bouldin

db_dbscan_vil <- dbi_clean(as.matrix(data_villa_normalized),
                           dbscan_villa$cluster)

cat("\nDavies-Bouldin Index (Villas)\n",
    "• K-means  :", round(db_kmeans_vil, 3), "\n",
    "• DBSCAN   :", round(db_dbscan_vil, 3), "\n")

performance_tbl <- tibble(
  Dataset = c("Apartments", "Villas"),
  `K-means (DBI)` = round(c(db_kmeans_apt, db_kmeans_vil), 3),
  `DBSCAN (DBI)`  = round(c(db_dbscan_apt, db_dbscan_vil), 3)
)
print(performance_tbl)

