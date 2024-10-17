################################################################################
#################### Bachelor Project Final Code ###############################
################################################################################
library(dplyr)
library(ggplot2)
library(reshape2)

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


###############################################################################
################### Function Definition Bandit ################################
###############################################################################

# Basic Thompson Sampling Function based on Beta Distribution
selectArmTS <- function(parameters, nArms) {
  values <- rep(0, nArms)
  for (i in 1:nArms) {
    values[i] <- rbeta(1, parameters[i, 1], parameters[i, 2])
  }
  return(which.max(values))
}


################################################################################
########## Find initial nt estimate through beta variance development ##########
################################################################################
# Function to calculate variance of the beta distribution
beta_variance <- function(alpha, beta) {
  return((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
}

# Function to simulate recommendations and measure variance at the end
simulate_recommendations <- function(Test_CTR, nt_range, n_trials) {
  results <- data.frame(nt = integer(), avg_variance = numeric())
  
  for (nt in nt_range) {
    variances <- numeric(n_trials)
    
    for (trial in 1:n_trials) {
      # Initialize Beta distributions
      parameters <- matrix(1, nrow = length(Test_CTR), ncol = 2)  # columns: 1 = alpha, 2 = beta
      
      for (t in 1:nt) {
        # Select an arm based on Thompson Sampling
        predicted_arm <- selectArmTS(parameters, length(Test_CTR))
        
        # Simulate an outcome for the selected arm
        outcome <- rbinom(1, 1, Test_CTR[predicted_arm])
        parameters[predicted_arm, 1] <- parameters[predicted_arm, 1] + outcome
        parameters[predicted_arm, 2] <- parameters[predicted_arm, 2] + (1 - outcome)
      }
      
      # Calculate average variance at the end of k0 iterations
      avg_variance <- mean(apply(parameters, 1, function(p) beta_variance(p[1], p[2])))
      variances[trial] <- avg_variance
    }
    
    # Calculate average variance for this k0 across all trials
    average_variance <- mean(variances)
    results <- rbind(results, data.frame(nt = nt, avg_variance = average_variance))
  }
  
  return(results)
}

# Example usage
set.seed(123)
Test_CTR <- as.numeric(Cluster_CTR[1, 7:29]) # Actual CTRs for a specific segment
nt_range <- seq(1000, 10000, by = 1000) # Range of k0 values to test
n_trials <- 10 # Number of trials for each nk value

results <- simulate_recommendations(Test_CTR, nt_range, n_trials)

# Generate the plot
variance_plot <- ggplot(results, aes(x = nt, y = avg_variance)) +
  geom_line(color = "#377eb8", size = 0.5) +  # Blue line
  geom_point(color = "#377eb8", size = 1.5) +  # Blue points
  scale_x_continuous(name = "nt values", labels = scales::comma) +
  scale_y_continuous(name = "Average Variance of Beta Distribution", labels = scales::scientific) +
  labs(title = "Average Variance of Beta Distribution (across 10 runs) by nt") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"  # Remove legend if not needed
  )

# Display the plot
print(variance_plot)

#CONCLUSION: We choose nt = 3000 as the beta variance starts to stabilize at this point, 
#indicating that we are sufficiently certain in our preferences (distributions)

################################################################################
################## Iterate over different values for c #########################
################################################################################

logistic_weight <- function(t,nt_value, c_value){
  1/(1+exp(c_value*(t-nt_value)))
}

set.seed(123)
c_values <- c(0.1,0.05,0.01,0.005,0.001,0.0005,0.0001,0.00005,0.00001,0.000005,0.000001) # Range of k values to test
nt_value <- 3000 # Based on previous analysis
nRecommendations <- 500000
nSimulation_Runs <- 1
nSegments <- nrow(Cluster_CTR)
nArms <- ncol(Cluster_CTR) - 6

# Data frame to store results for different k values
results_all_c <- matrix(ncol = 2, nrow = 0) #1 for the k value, 2 for the aggregated CTR

# Loop through each k value
for (c in c_values) {
  results_specific_c <- matrix(ncol = 2, nrow = 0) #1 for the iteration number, 3 for the ACTR of that run
  for (i in 1:nSimulation_Runs) {
    
    # Set Seed based on simulation run to ensure reproducibility
    set.seed(i)
    
    # Sample a dataset of 500,000 observations for the simulation to run on
    unique_indices <- sample(nrow(data), nRecommendations, replace = FALSE)
    
    ## Initialize results tables & Beta Distribution tables
    
    # PARTIALLY
    # Initialize results storage
    results_partially <- matrix(0, nrow = nRecommendations, ncol = 5)#3 is outcome
    
    # Matrix to store alpha and beta for each arm for each segment
    mAB_partially <- array(1, dim = c(nArms, 2, nSegments)) # Dimensions: arms, parameters (alpha, beta), segments
    
    # Matrix to store it for the populations as a whole 
    mAB_global <- matrix(1, nrow = nArms, ncol = 2)
    
    # Segment Counter
    segment_n_runs <- data.frame(
      Segment = 1:nSegments,
      Count_Occurences = rep(0, nSegments)
    )
    
    for (t in 1:nRecommendations) {
      
      # As we need the number of times a certain segment was drawn so far, we
      # already store the segment information in the table
      segment <- data[unique_indices[t], "Segment"]
      segment_n_runs[segment, 2] <- segment_n_runs[segment, 2] + 1
      
      # We compute the number of times that particular segment has been drawn
      n_runs_segment <- segment_n_runs[segment, 2]
      
      # Compute Gamma
      gamma <- logistic_weight(n_runs_segment, nt_value, c)
      
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
      outcome <- rbinom(1, 1, Cluster_CTR[segment, arm_partially + 6])
      
      # Update the results table
      results_partially[t, ] <- c(t, arm_partially, outcome, segment, Cluster_CTR[segment, arm_partially + 6])
      
      # UPDATE PROCESS:
      # Update segment-specific distribution
      mAB_partially[arm_partially, 1, segment] <- mAB_partially[arm_partially, 1, segment] + outcome
      mAB_partially[arm_partially, 2, segment] <- mAB_partially[arm_partially, 2, segment] + (1 - outcome)
      
      # Update global model distribution
      mAB_global[arm_partially, 1] <- mAB_global[arm_partially, 1] + outcome
      mAB_global[arm_partially, 2] <- mAB_global[arm_partially, 2] + (1 - outcome)
    }
    
    
    
    # Aggregate results for this k
    results_specific_c <- rbind(results_specific_c, c(i, mean(results_partially[,3])))
  }
  results_all_c <- rbind(results_all_c,c(c,mean(results_specific_c[,2])))
}

# Set column names
colnames(results_all_c) <- c("Value", "Result")

# Convert matrix to data frame
results_all_c_df <- as.data.frame(results_all_c)

actr_unpooled <- 0.0472202

# Plot the data
# Plot the data
ggplot(results_all_c_df, aes(x = Value, y = Result)) +
  geom_line(color = "#377eb8", size = 0.5) +  # Blue line for model results
  geom_point(color = "#377eb8", shape = 21, fill = "#377eb8", size = 1.5, stroke = 1.5) +  # Blue points for specific observations
  geom_hline(yintercept = actr_unpooled, color = "red", linetype = "dashed", size = 0.7) +  # Explicitly set dashed line to red
  scale_x_log10() +  # Applying log scale to x-axis
  labs(
    title = "Plot of different c over nt=5000",
    subtitle = "Comparison of Model Results and Unpooled ACTR",
    x = "c Values (log scale)",
    y = "ACTR Result"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",  # Removing legend entirely
    axis.title.x = element_text(margin = margin(t = 10)),  # Top margin for x axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Right margin for y axis title
    text = element_text(family = "Times New Roman")  # Ensuring consistent font usage
  )

# Analyze the results
print(results_all_k)
  
#Pick K = 0.001

################################################################################
################# Iterate over different values for nt #########################
################################################################################

logistic_weight <- function(t,nt_value, c_value){
  1/(1+exp(c_value*(t-nt_value)))
}

selectArmTS <- function(parameters, nArms) {
  values <- rep(0, nArms)
  for (i in 1:nArms) {
    values[i] <- rbeta(1, parameters[i, 1], parameters[i, 2])
  }
  return(which.max(values))
}

c_value <- 0.001  # Based on previous analysis 
nt_values <- c(seq(1000, 10000, by = 1000),15000,20000,30000,40000,50000)  # Range of t0 values to test
nRecommendations <- 500000
nSimulation_Runs <- 1
nSegments <- nrow(Cluster_CTR)
nArms <- ncol(Cluster_CTR) - 6

# Data frame to store results for different t0 values
results_all_nt <- matrix(ncol = 2, nrow = 0) #1 for the t0 value, 2 for the aggregated CTR

# Loop through each t0 value
for (nt_value in nt_values) {
  results_specific_nt <- matrix(ncol = 2, nrow = 0) #1 for the run, 2 for the CTRs of that run
  for (i in 1:nSimulation_Runs) {
    
    # Set Seed based on simulation run to ensure reproducibility
    set.seed(i)
    
    # Sample a dataset of 500,000 observations for the simulation to run on
    unique_indices <- sample(nrow(data), nRecommendations, replace = FALSE)
    
    ## Initialize results tables & Beta Distribution tables
    
    # PARTIALLY
    # Initialize results storage
    results_partially <- matrix(0, nrow = nRecommendations, ncol = 5)
    
    # Matrix to store alpha and beta for each arm for each segment
    mAB_partially <- array(1, dim = c(nArms, 2, nSegments))  # Dimensions: arms, parameters (alpha, beta), segments
    
    # Matrix to store it for the populations as a whole 
    mAB_global <- matrix(1, nrow = nArms, ncol = 2)
    
    # Segment Counter
    segment_n_runs <- data.frame(
      Segment = 1:nSegments,
      Count_Occurences = rep(0, nSegments)
    )
    
    for (t in 1:nRecommendations) {
      
      # As we need the number of times a certain segment was drawn so far, we
      # already store the segment information in the table
      segment <- data[unique_indices[t], "Segment"]
      segment_n_runs[segment, 2] <- segment_n_runs[segment, 2] + 1
      
      # We compute the number of times that particular segment has been drawn
      n_runs_segment <- segment_n_runs[segment, 2]
      
      # Compute Gamma
      gamma <- logistic_weight(n_runs_segment, nt_value, c_value)
      
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
      outcome <- rbinom(1, 1, Cluster_CTR[segment, arm_partially + 6])
      
      # Update the results table
      results_partially[t, ] <- c(t, arm_partially, outcome, segment, Cluster_CTR[segment, arm_partially + 6])
      
      # UPDATE PROCESS:
      # Update segment-specific distribution
      mAB_partially[arm_partially, 1, segment] <- mAB_partially[arm_partially, 1, segment] + outcome
      mAB_partially[arm_partially, 2, segment] <- mAB_partially[arm_partially, 2, segment] + (1 - outcome)
      
      # Update global model distribution
      mAB_global[arm_partially, 1] <- mAB_global[arm_partially, 1] + outcome
      mAB_global[arm_partially, 2] <- mAB_global[arm_partially, 2] + (1 - outcome)
    }
    
    # Aggregate results for this t0
    results_specific_nt <- rbind(results_specific_nt, c(i, mean(results_partially[,3])))
  }
  results_all_nt <- rbind(results_all_nt,c(nt_value,mean(results_specific_nt[,2])))
}

# Analyze the results
print(results_all_nt)

results_nt_df <- as.data.frame(results_all_nt)

# Define the ACTR of the UNpooled model
actr_unpooled <- 0.0472202



# Plot the data
ggplot(results_nt_df, aes(x = V1, y = V2)) +
  geom_line(color = "#377eb8", size = 0.5) +  # Blue line with increased size for visibility
  geom_point(color = "#377eb8", size = 1.5, shape = 21, fill = "#377eb8") +  # Blue points for specific observations
  geom_hline(yintercept = actr_unpooled, color = "red", linetype = "dashed", size = 0.7) +  # Dashed line explicitly set to red
  labs(x = "nt values",
    y = "ACTR Results"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",  # Removing legend entirely
    axis.title.x = element_text(margin = margin(t = 10)),  # Top margin for x axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Right margin for y axis title
    text = element_text(family = "Times New Roman")  # Ensuring consistent font usage
  )




