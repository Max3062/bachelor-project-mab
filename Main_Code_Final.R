################################################################################
#################### Bachelor Project Final Code ###############################
################################################################################
library(dplyr)
library(ggplot2)
library(scales)

#NOTE: The main data file is the original data file with one extra column, this
#column is computed in line 165 of this code and contains the segment of the user
#If you do not wish to use the provided main data file, please load the initial dataset
#and uncomment and run line 171 - 201

load("main_data.Rdata")
#load("bscProjectData.Rdata")
#data <- bscProjectData$mainData

set.seed(123) #before running a code we set a random seed

################################################################################
############################## Clustering ######################################
################################################################################

# Function to calculate within-cluster sum of squares (WCSS)
calculate_wcss <- function(data, k) {
  kmeans_obj <- kmeans(data, centers = k)
  return(kmeans_obj$tot.withinss)
}

# Function to plot elbow method
plot_elbow_method <- function(data, max_k) {
  wcss_values <- sapply(1:max_k, function(k) calculate_wcss(data, k))
  elbow_data <- data.frame(K = 1:max_k, WCSS = wcss_values)
  
  # Plot the elbow curve
  ggplot(elbow_data, aes(x = K, y = WCSS)) +
    geom_line() +
    geom_point() +
    labs(x = "Number of clusters (K)", y = "Within-Cluster Sum of Squares (WCSS)", 
         title = "Elbow Method for Optimal K") +
    theme_minimal()
}

# Specify the maximum number of clusters to test
max_k <- 20


# Plot the elbow method
plot_elbow_method(data, max_k)


#number of clusters
k <- 5

# Apply K-Means Clustering
kmeans_result <- kmeans(data, centers = k, nstart = 25)
# nstart parameter runs the algorithm multiple times to avoid local optima

# Print cluster centers
print(kmeans_result$centers)

# Check the size of each cluster
table(kmeans_result$cluster)

###K-Means

# Select only the columns for user features
user_features <- data %>%
  select("User Feature 1", "User Feature 2", "User Feature 3", "User Feature 4", "User Feature 5")

# Apply K-Means Clustering with k = 5
kmeans_result <- kmeans(user_features, centers = 5, nstart = 25)

#Vizualizing clusters
install.packages("ggpubr")
install.packages("factoextra")
library(factoextra)
library(ggpubr)
fviz_cluster(kmeans_result, data = user_features,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#FF0000", "#A020F0"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


# Output the results
print(kmeans_result$centers)  # Print cluster centers
table(kmeans_result$cluster)  # Print the size of each cluster

# Add the cluster assignments back to the original data
data2 <- data
data2$cluster <- kmeans_result$cluster

#Create 5 cluster subsets

# Split the data frame based on the 'cluster' column
list_clusters <- split(data2, data2$cluster)

# Access each cluster's data frame
data_cluster_1 <- list_clusters[[1]]
data_cluster_2 <- list_clusters[[2]]
data_cluster_3 <- list_clusters[[3]]
data_cluster_4 <- list_clusters[[4]]
data_cluster_5 <- list_clusters[[5]]

# Build CTR

# Assuming the cluster data frames are named data_cluster_1, data_cluster_2, ..., data_cluster_5
clusters_data <- list(data_cluster_1 = data_cluster_1, 
                      data_cluster_2 = data_cluster_2,
                      data_cluster_3 = data_cluster_3, 
                      data_cluster_4 = data_cluster_4,
                      data_cluster_5 = data_cluster_5)

article_columns <- c("109536", "109531", "109543", "109534", "109525", "109524", "109552", "109547", "109510", "109548", "109542", "109417", "109550", "109544", "109533", "109529", "109546","109530", "109545", "109553", "109551", "109554", "109555")  # list of article columns
# Initialize an empty data frame to store the results

ctr_results <- data.frame(cluster = integer(), article = character(), click_through_rate = numeric())

# Function to calculate click-through rate
calculate_ctr <- function(data, article) {
  shown <- data[data[[article]] == 2, ]
  if (nrow(shown) > 0) {
    num_clicks <- sum(shown$clicks == 1, na.rm = TRUE)
    total_shown <- nrow(shown)
    ctr <- num_clicks / total_shown
  } else {
    ctr <- NA  # Return NA if no data to avoid division by zero
  }
  return(ctr)
}

# Loop over each cluster and each article to calculate and store CTR
for (cluster_name in names(clusters_data)) {
  for (article in article_columns) {
    ctr <- calculate_ctr(clusters_data[[cluster_name]], article)
    ctr_results <- rbind(ctr_results, 
                         data.frame(cluster = cluster_name, 
                                    article = article, 
                                    click_through_rate = ctr))
  }
}

# Output the final data frame with CTR for each article in each cluster
print(ctr_results)

#Bringing it in wide format.
# Load necessary library
library(reshape2)

# Assuming ctr_results contains columns 'cluster', 'article', and 'click_through_rate'
# Reshape ctr_results to wide format
ctr_wide <- dcast(ctr_results, cluster ~ article, value.var = "click_through_rate")

# Print the reshaped data frame
print(ctr_wide)

#add cluster centroids
cluster_centroids <- as.data.frame(kmeans_result$centers)

#merge
cluster_CTR_final <- cbind(ctr_wide, cluster_centroids)

cluster_CTR_final <- cluster_CTR_final[, c(1, 25,26,27,28,29, 2:24)]

Cluster_CTR <- cluster_CTR_final
Cluster_CTR



###Function: Finding closest Cluster --> this function was used to add a column to
#            the initial dataset that gives info about the segment of each customer
#            So that it is easier to identify when running the simulation

# Function to link it to cluster
#get_closest_cluster <- function(random_instance, Cluster_CTR) {
# Subset the cluster centroids (user features) from the cluster_CTR_final dataframe
#  cluster_features <- Cluster_CTR[, 2:6]

# Calculate the Euclidean distance between the user features of the random instance and each cluster centroid
#  distances <- apply(cluster_features, 1, function(cluster_features_row) {
#    sqrt(sum((random_instance[2:6] - cluster_features_row)^2))
#  })

# Find the index of the cluster centroid with the minimum distance
#  closest_cluster_index <- which.min(distances)

# Return the index of the closest cluster centroid
#  return(closest_cluster_index)
#}

#Used to accelerate process by adding segment info to dataset
#data["Segment"] <- NA
#
#for (p in 1:nrow(data)){
#  data[p,"Segment"] <- get_closest_cluster(data[p,],Cluster_CTR)
#}

#save(data, file="main_data.Rdata")

# Definition of Max CTR per segment (needed for optimal bandit)
transposed_df <- as.data.frame(t(Cluster_CTR))

transposed_df <- transposed_df[-c(1:6),]

df_numeric <- data.frame(lapply(transposed_df, function(x) as.numeric(as.character(x))))
summary(df_numeric)

maximum_values <- apply(df_numeric,2,max)

maximum_per_segment <- data.frame(Segment = 1:5,
                                  Maximum_CTR = maximum_values)

################################################################################
############################# Clustering End ###################################
################################################################################
set.seed(123)
# Initialize parameters (not recommended to run 500k --> will take hours)
nArms <- ncol(Cluster_CTR)-6
nRecommendations <- 500000
nSimulation_Runs <- 10
nSegments <- nrow(Cluster_CTR)
c_value <- 0.001
nt_value <- 3000


#Initialize results tables
##POOLED
metrics_pooled <- data.frame(
  Simulation = integer(0), 
  ACTR = numeric(0), 
  STD = numeric(0), 
  CI_Lower = numeric(0), 
  CI_Upper = numeric(0), 
  RM = numeric(0)
)

##UNPOOLED
metrics_unpooled <- data.frame(
  Simulation = integer(0), 
  ACTR = numeric(0), 
  STD = numeric(0), 
  CI_Lower = numeric(0), 
  CI_Upper = numeric(0), 
  RM = numeric(0)
)

##PARTIALLY
metrics_partially <- data.frame(
  Simulation = integer(0), 
  ACTR = numeric(0), 
  STD = numeric(0), 
  CI_Lower = numeric(0), 
  CI_Upper = numeric(0), 
  RM = numeric(0)
)

##OPTIMAL
metrics_optimal <- data.frame(
  Simulation = integer(0), 
  ACTR = numeric(0), 
  STD = numeric(0), 
  CI_Lower = numeric(0), 
  CI_Upper = numeric(0), 
  RM = numeric(0)
)

##RANDOM
metrics_random <- data.frame(
  Simulation = integer(0), 
  ACTR = numeric(0), 
  STD = numeric(0), 
  CI_Lower = numeric(0), 
  CI_Upper = numeric(0), 
  RM = numeric(0)
)

#Initialize Data Frames for Regret
cumulative_regret_pooled <- data.frame(Instances = 1:nRecommendations)
cumulative_regret_unpooled <- data.frame(Instances = 1:nRecommendations)
cumulative_regret_partially <- data.frame(Instances = 1:nRecommendations)
cumulative_regret_random <- data.frame(Instances = 1:nRecommendations)

###############################################################################
################### Function Definition Bandit ################################
###############################################################################

# Basic Thompson Sampling Function based on Beta Distribution
selectArmTS <- function(parameters, nArms) {
  values <- rep(0, nArms)
  for (p in 1:nArms) {
    values[p] <- rbeta(1, parameters[p, 1], parameters[p, 2])
  }
  return(which.max(values))
}


#Partially Pooling: Logistic Weight Function
logistic_weight <- function(t,nt_value, c_value){
  1/(1+exp(c_value*(t-nt_value)))
}



################################################################################
####################### Start of Simulation ####################################
################################################################################

################################################################################
######################## Random & Optimal Bandit ###############################
################################################################################

#Initialize tables to store all values of the random and optimal bandit, as these
#are required by the otehr bandits to compute their evaluation metrics
aggregated_results_random <- matrix(0,nrow=nRecommendations, ncol=nSimulation_Runs)
aggregated_results_optimal <- matrix(0,nrow=nRecommendations, ncol=nSimulation_Runs)
aggregated_ctr_optimal <- matrix(0,nrow=nRecommendations, ncol=nSimulation_Runs)


for (i in 1:nSimulation_Runs) {
  
  set.seed(i)  # Set Seed for reproducibility
  
  # Sample a dataset of 500,000 observations for the simulation to run on
  unique_indices <- sample(nrow(data), nRecommendations, replace = FALSE)
  
  # Initialize results tables for both optimal and random bandits
  results_optimal <- matrix(0, nrow = nRecommendations, ncol = 4)  # columns: 1=index, 2=NA, 3=result, 4=CTR
  results_random <- matrix(0, nrow = nRecommendations, ncol = 4)  # columns: 1=index, 2=arm, 3=result, 4=CTR
  
  
  for (t in 1:nRecommendations) {
    
    # Optimal Bandit
    ctr_optimal <- maximum_per_segment[data[unique_indices[t], "Segment"], 2]
    outcome_optimal <- rbinom(1, 1, ctr_optimal)
    results_optimal[t,] <- c(t, NA, outcome_optimal, ctr_optimal)
    
    # Random Bandit
    segment <- data[unique_indices[t], "Segment"]
    arm_random <- sample(1:nArms, 1)
    outcome_random <- rbinom(1, 1, Cluster_CTR[segment, arm_random+6])
    results_random[t,] <- c(t, arm_random, outcome_random, Cluster_CTR[segment, arm_random+6])
  }
  
  aggregated_results_random[,i] <- results_random[,3]
  aggregated_ctr_optimal[,i]<- results_optimal[,4]
  aggregated_results_optimal[,i] <- results_optimal[,3]

  
  # Calculating metrics for optimal bandit
  ACTR_optimal <- mean(results_optimal[, 3])
  STD_optimal <- sd(results_optimal[, 3])
  CI_optimal <- c(ACTR_optimal - 1.96 * STD_optimal / sqrt(nrow(results_optimal)),
                  ACTR_optimal + 1.96 * STD_optimal / sqrt(nrow(results_optimal)))
  
  # Calculating metrics for random bandit
  ACTR_random <- mean(results_random[, 3])
  STD_random <- sd(results_random[, 3])
  CI_random <- c(ACTR_random - 1.96 * STD_random / sqrt(nrow(results_random)),
                 ACTR_random + 1.96 * STD_random / sqrt(nrow(results_random)))
  
  # Calculate relative mean (RM) and regret
  RM_optimal <- ((ACTR_optimal - ACTR_random) / (ACTR_optimal-ACTR_random)) * 100
  metrics_optimal <- rbind(metrics_optimal, data.frame(ACTR = ACTR_optimal, STD = STD_optimal, CI_Lower = CI_optimal[1], CI_Upper = CI_optimal[2], RM = RM_optimal))
  metrics_random <- rbind(metrics_random, data.frame(ACTR = ACTR_random, STD = STD_random, CI_Lower = CI_random[1], CI_Upper = CI_random[2], RM = 0))
  
  regret_random <- results_optimal[,4] - results_random[,4]
  cumulative_regret_random[, paste0("Run_", i)] <- cumsum(regret_random)
}


################################################################################
############################ Pooled policy #####################################
################################################################################

for (i in 1:nSimulation_Runs){
  
  #Set Seed based on simulation run to ensure reproducability
  set.seed(i)
  
  #Sample a dataset of 500,000 observations for the simulation to run on
  unique_indices <- sample(nrow(data),nRecommendations, replace = FALSE)

  ##Initialize results tables & Beta Distribution tables
  
  #POOLED
  # Initialize Results Table
  results_pooled<-matrix(0,nrow=nRecommendations,ncol=4)# columns: 1=index, 2= arm, 3= result, 4= CTR
  
  #Matrix to store alpha and beta for Pooled
  mAB_pooled<-matrix(1,nrow=nArms,ncol=2) #columns: 1) = alpha, 2= beta
  

  for (t in 1:nRecommendations){
    
    # Identify the segment the user belongs to to later find CTR etc.
    segment <- data[unique_indices[t],"Segment"]
    
    # Select an arm based on Beta distribution
    arm_pooled<-selectArmTS(mAB_pooled,nArms)
    
    # Determine the outcome
    outcome_p <- rbinom(1, 1, Cluster_CTR[segment, arm_pooled+6])
    
    # Update the Beta distribution
    mAB_pooled[arm_pooled,1]<-mAB_pooled[arm_pooled,1]+outcome_p
    
    mAB_pooled[arm_pooled,2]<-mAB_pooled[arm_pooled,2]+(1-outcome_p)
    
    # Store the results
    results_pooled[t,]<-c(t,arm_pooled,outcome_p,Cluster_CTR[segment, arm_pooled+6])
  }
  ACTR_pooled <- mean(results_pooled[,3]) # Mean of the third column (outcome)
  STD_pooled <- sd(results_pooled[,3]) # Standard deviation of the third column (outcome)
  CI_pooled <- c(mean(results_pooled[,3]) - 1.96 * STD_pooled / sqrt(nrow(results_pooled)),
                 mean(results_pooled[,3]) + 1.96 * STD_pooled / sqrt(nrow(results_pooled)))
  RM_pooled <- ((ACTR_pooled - mean(aggregated_results_random[,i])) / (mean(aggregated_results_optimal[,i]) - mean(aggregated_results_random[,i]))) * 100
  
  metrics_pooled <- rbind(metrics_pooled, data.frame(ACTR = ACTR_pooled, STD = STD_pooled, CI_Lower = CI_pooled[1], CI_Upper = CI_pooled[2], RM = RM_pooled))
  regret_pooled <- aggregated_ctr_optimal[,i]-results_pooled[,4]
  cumulative_regret_pooled[, paste0("Run_", i)] <- cumsum(regret_pooled)
  }

################################################################################
########################### Unpooled Policy ####################################
################################################################################

for (i in 1:nSimulation_Runs){
  
  #Set Seed based on simulation run to ensure reproducability
  set.seed(i)
  
  #Sample a dataset of 500,000 observations for the simulation to run on
  unique_indices <- sample(nrow(data),nRecommendations, replace = FALSE)
  
  ##Initialize results tables & Beta Distribution tables
  
  #UNPOOLED
  # Initialize results storage
  results_unpooled <- matrix(0, nrow = nRecommendations, ncol = 5) # Added a column for segment & one for CTR
  
  # Matrix to store alpha and beta for each arm for each segment
  mAB_unpooled <- array(1, dim = c(nArms, 2, nSegments)) # Dimensions: arms, parameters (alpha, beta), segments
  
  
  for (t in 1:nRecommendations){
    
    # Identify the segment the user belongs to to later find CTR etc.
    segment <- data[unique_indices[t],"Segment"]
    
    # Select an arm based on the current segment's beta distribution
    arm_unpooled <- selectArmTS(mAB_unpooled[, , segment], nArms)
    
    # Determine the outcome based on the arm's CTR
    outcome_un <- rbinom(1, 1, Cluster_CTR[segment, arm_unpooled+6])
    
    # Update the beta parameters for the selected arm in the specific segment
    mAB_unpooled[arm_unpooled, 1, segment] <- mAB_unpooled[arm_unpooled, 1, segment] + outcome_un
    mAB_unpooled[arm_unpooled, 2, segment] <- mAB_unpooled[arm_unpooled, 2, segment] + (1 - outcome_un)
    
    # Store the results
    results_unpooled[t, ] <- c(t, arm_unpooled, outcome_un, segment,Cluster_CTR[segment, arm_unpooled+6])
    
  }
  ACTR_unpooled <- mean(results_unpooled[, 3])
  STD_unpooled <- sd(results_unpooled[, 3])
  CI_unpooled <- c(mean(results_unpooled[,3]) - 1.96 * STD_unpooled / sqrt(nrow(results_unpooled)),
                   mean(results_unpooled[,3]) + 1.96 * STD_unpooled / sqrt(nrow(results_unpooled)))
  RM_unpooled <- ((ACTR_unpooled - mean(aggregated_results_random[,i])) / (mean(aggregated_results_optimal[,i]) - mean(aggregated_results_random[,i]))) * 100
  
  metrics_unpooled <- rbind(metrics_unpooled, data.frame(ACTR = ACTR_unpooled, STD = STD_unpooled, CI_Lower = CI_unpooled[1], CI_Upper = CI_unpooled[2], RM = RM_unpooled))
  regret_unpooled <- aggregated_ctr_optimal[,i]-results_unpooled[,5]
  cumulative_regret_unpooled[, paste0("Run_", i)] <- cumsum(regret_unpooled)
} 

################################################################################
############################ Partially Pooled ##################################
################################################################################

for (i in 1:nSimulation_Runs){
  
  #Set Seed based on simulation run to ensure reproducability
  set.seed(i)
  
  #Sample a dataset of 500,000 observations for the simulation to run on
  unique_indices <- sample(nrow(data),nRecommendations, replace = FALSE)
  
  
  ##Initialize results tables & Beta Distribution tables
  
  #PARTIALLY
  # Initialize results storage
  results_partially <- matrix(0, nrow = nRecommendations, ncol = 5)#1 iteration, 2 arm, 3 outcome, 4 segment 5 CTR
  
  # Matrix to store alpha and beta for each arm for each segment
  mAB_partially <- array(1, dim = c(nArms, 2, nSegments)) # Dimensions: arms, parameters (alpha, beta), segments
  
  # Matrix to store it for the populations as a whole 
  mAB_global <-matrix(1,nrow=nArms,ncol=2)
  
  # Segment Counter as gamma depends on it (n_runs_segment is nk)
  segment_n_runs <- data.frame(
    Segment = 1:5,
    Count_Occurences = rep(0, 5)
  )
  

  
  for (t in 1:nRecommendations){
    
    # As we need the number of times a certain segment was drawn so far, we
    # already store the segment information in the table
    
    segment <- data[unique_indices[t],"Segment"]
    segment_n_runs[segment,2] <- segment_n_runs[segment,2] +1
    
    # We compute the number of times that particular segment has been drawn
    n_runs_segment <- segment_n_runs[segment,2]
    
    #Compute Gamma
    gamma <- logistic_weight(n_runs_segment,nt_value,c_value)
    
    mAB_aggregated <- cbind(mAB_global, mAB_partially[, , segment])
    
    # Compute sums and scaling factors using matrix operations
    sum_pooled <- rowSums(mAB_aggregated[, 1:2])
    sum_partially <- rowSums(mAB_aggregated[, 3:4])
    scaling_factor <- sum_pooled / sum_partially
    
    # Scale segment-specific distributions
    scaled_alpha <- mAB_aggregated[, 3] * scaling_factor
    scaled_beta <- mAB_aggregated[, 4] * scaling_factor
    
    # Compute the aggregated alpha and beta distributions
    aggregated_alpha <- scaled_alpha * (1 - gamma) + gamma * mAB_aggregated[, 1]
    aggregated_beta <- scaled_beta * (1 - gamma) + gamma * mAB_aggregated[, 2]
    
    # Form the aggregated matrix
    mAB_aggregated <- cbind(aggregated_alpha, aggregated_beta)
    
    # Select an arm based on the aggregated beta distribution
    arm_partially <- selectArmTS(mAB_aggregated, nArms)
    
    # Determine the outcome based on the arm's CTR
    outcome_pa <- rbinom(1, 1, Cluster_CTR[segment, arm_partially+6])

    
    # Update the results table
    results_partially[t, ] <- c(t, arm_partially, outcome_pa, segment, Cluster_CTR[segment, arm_partially + 6])

    
    #UPDATE PROCESS:
    #Update segment-specific distribution
    
    mAB_partially[arm_partially, 1, segment] <- mAB_partially[arm_partially, 1, segment] + outcome_pa
    mAB_partially[arm_partially, 2, segment] <- mAB_partially[arm_partially, 2, segment] + (1 - outcome_pa)
    
    #Update global model distribution
    mAB_global[arm_partially,1]<-mAB_global[arm_partially,1]+outcome_pa
    
    mAB_global[arm_partially,2]<-mAB_global[arm_partially,2]+(1-outcome_pa)
    
  }   


  ACTR_partially <- mean(results_partially[, 3])
  STD_partially <- sd(results_partially[, 3])
  CI_partially <- c(mean(results_partially[,3]) - 1.96 * STD_partially / sqrt(nrow(results_partially)),
                   mean(results_partially[,3]) + 1.96 * STD_partially / sqrt(nrow(results_partially)))
  
  RM_partially <- ((ACTR_partially - mean(aggregated_results_random[,i])) / (mean(aggregated_results_optimal[,i]) - mean(aggregated_results_random[,i])))*100
  
  metrics_partially <- rbind(metrics_partially, data.frame(ACTR = ACTR_partially, STD = STD_partially, CI_Lower = CI_partially[1], CI_Upper = CI_partially[2], RM = RM_partially))
  regret_partially <- aggregated_ctr_optimal[,i]-results_partially[,5]
  cumulative_regret_partially[, paste0("Run_", i)] <- cumsum(regret_partially)
}



### REGRET PLOT

regret_plot <- ggplot() +
  geom_line(data = cumulative_regret_pooled, aes(x = Instances, y = rowMeans(select(cumulative_regret_pooled, -Instances)), color = "Pooled"), size = 0.5) +
  geom_line(data = cumulative_regret_unpooled, aes(x = Instances, y = rowMeans(select(cumulative_regret_unpooled, -Instances)), color = "Unpooled"), size = 0.5) +
  geom_line(data = cumulative_regret_partially, aes(x = Instances, y = rowMeans(select(cumulative_regret_partially, -Instances)), color = "Partially"), size = 0.5) +
  labs(title = "Average Cumulative Regret over 10 Simulation Runs",
       x = "Instances (in thousands)", y = "Average Cumulative Regret") +
  scale_x_continuous(labels = comma_format(scale = 1e-3)) +
  scale_color_manual(values = c("Pooled" = "#4daf4a", "Unpooled" = "#e41a1c", "Partially" = "#377eb8")) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.title.x = element_text(margin = margin(t = 10)),  # top margin for x axis title
        axis.title.y = element_text(margin = margin(r = 10)))  # right margin for y axis title

regret_plot

#Zoomed in Regret Plot
regret_plot_zoom <- ggplot() +
  geom_line(data = cumulative_regret_pooled, aes(x = Instances, y = rowMeans(select(cumulative_regret_pooled, -Instances)), color = "Pooled"), size = 0.5) +
  geom_line(data = cumulative_regret_unpooled, aes(x = Instances, y = rowMeans(select(cumulative_regret_unpooled, -Instances)), color = "Unpooled"), size = 0.5) +
  geom_line(data = cumulative_regret_partially, aes(x = Instances, y = rowMeans(select(cumulative_regret_partially, -Instances)), color = "Partially"), size = 0.5) +
  labs(title = "Average Cumulative Regret over 10 Simulation Runs (Zoomed)",
       x = "Instances (in thousands)", y = "Average Cumulative Regret") +
  scale_x_continuous(labels = comma_format(scale = 1e-3)) +
  scale_color_manual(values = c("Pooled" = "#4daf4a", "Unpooled" = "#e41a1c", "Partially" = "#377eb8")) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box.margin = margin(6, 6, 6, 6),
        axis.title.x = element_text(margin = margin(t = 10)),  # top margin for x axis title
        axis.title.y = element_text(margin = margin(r = 10)))+  # right margin for y axis title
coord_cartesian(xlim = c(0, 100000), ylim = c(0, 1000))  # Setting limits for zoom

# Print the plot
regret_plot_zoom

################################################################################
################# Create a summary table across all runs #######################
################################################################################

#Summary

metrics_summary <- data.frame(
  Policy = c("Random", "Pooled", "Unpooled", "Partially", "Optimal"),
  ACTR = numeric(5),
  SE = numeric(5),
  STD = numeric(5),
  CI_Lower = numeric(5),
  CI_Upper = numeric(5),
  RM=numeric(5)
)

#mean ACTR
metrics_summary[metrics_summary$Policy == "Optimal", "ACTR"] <- mean(metrics_optimal$ACTR)
metrics_summary[metrics_summary$Policy == "Random", "ACTR"] <- mean(metrics_random$ACTR)
metrics_summary[metrics_summary$Policy == "Pooled", "ACTR"] <- mean(metrics_pooled$ACTR)
metrics_summary[metrics_summary$Policy == "Unpooled", "ACTR"] <- mean(metrics_unpooled$ACTR)
metrics_summary[metrics_summary$Policy == "Partially", "ACTR"] <- mean(metrics_partially$ACTR)

#sample STD
metrics_summary[metrics_summary$Policy == "Optimal", "STD"] <- sqrt(mean(metrics_optimal$STD^2))
metrics_summary[metrics_summary$Policy == "Random", "STD"] <- sqrt(mean(metrics_random$STD^2))
metrics_summary[metrics_summary$Policy == "Pooled", "STD"] <- sqrt(mean(metrics_pooled$STD^2))
metrics_summary[metrics_summary$Policy == "Unpooled", "STD"] <- sqrt(mean(metrics_unpooled$STD^2))
metrics_summary[metrics_summary$Policy == "Partially", "STD"] <- sqrt(mean(metrics_partially$STD^2))

#sample SE
metrics_summary[metrics_summary$Policy == "Optimal", "SE"] <- sd(metrics_optimal$ACTR) / sqrt(nrow(metrics_optimal))
metrics_summary[metrics_summary$Policy == "Random", "SE"] <-   sd(metrics_random$ACTR) / sqrt(nrow(metrics_random))
metrics_summary[metrics_summary$Policy == "Pooled", "SE"] <- sd(metrics_pooled$ACTR) / sqrt(nrow(metrics_pooled))
metrics_summary[metrics_summary$Policy == "Unpooled", "SE"] <- sd(metrics_unpooled$ACTR) / sqrt(nrow(metrics_unpooled))
metrics_summary[metrics_summary$Policy == "Partially", "SE"] <- sd(metrics_partially$ACTR) / sqrt(nrow(metrics_partially))

#mean CI_Lower for each policy
metrics_summary[metrics_summary$Policy == "Optimal", "CI_Lower"] <- mean(metrics_optimal$CI_Lower)
metrics_summary[metrics_summary$Policy == "Random", "CI_Lower"] <- mean(metrics_random$CI_Lower)
metrics_summary[metrics_summary$Policy == "Pooled", "CI_Lower"] <- mean(metrics_pooled$CI_Lower)
metrics_summary[metrics_summary$Policy == "Unpooled", "CI_Lower"] <- mean(metrics_unpooled$CI_Lower)
metrics_summary[metrics_summary$Policy == "Partially", "CI_Lower"] <- mean(metrics_partially$CI_Lower)

#mean CI_Upper for each policy
metrics_summary[metrics_summary$Policy == "Optimal", "CI_Upper"] <- mean(metrics_optimal$CI_Upper)
metrics_summary[metrics_summary$Policy == "Random", "CI_Upper"] <- mean(metrics_random$CI_Upper)
metrics_summary[metrics_summary$Policy == "Pooled", "CI_Upper"] <- mean(metrics_pooled$CI_Upper)
metrics_summary[metrics_summary$Policy == "Unpooled", "CI_Upper"] <- mean(metrics_unpooled$CI_Upper)
metrics_summary[metrics_summary$Policy == "Partially", "CI_Upper"] <- mean(metrics_partially$CI_Upper)

#mean RM
metrics_summary[metrics_summary$Policy == "Optimal", "RM"] <- mean(metrics_optimal$RM)
metrics_summary[metrics_summary$Policy == "Random", "RM"] <- mean(metrics_random$RM)
metrics_summary[metrics_summary$Policy == "Pooled", "RM"] <- mean(metrics_pooled$RM)
metrics_summary[metrics_summary$Policy == "Unpooled", "RM"] <- mean(metrics_unpooled$RM)
metrics_summary[metrics_summary$Policy == "Partially", "RM"] <- mean(metrics_partially$RM)

print(metrics_summary)

