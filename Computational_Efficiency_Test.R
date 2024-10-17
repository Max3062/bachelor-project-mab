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
c_value <- 0.01
nt_value <- 3000


#Initialize time storage
time_results_pooled <- matrix(0, nrow = nSimulation_Runs, ncol = 3)
time_results_unpooled <- matrix(0, nrow = nSimulation_Runs, ncol = 3)
time_results_partially <- matrix(0, nrow = nSimulation_Runs, ncol = 3)

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


#Partially Pooling: Logistic Weight Function (Explanation at line 416)
logistic_weight <- function(t,nt_value, c_value){
  1/(1+exp(c_value*(t-nt_value)))
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
  
time_pooled <- system.time({
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
  }})
#Store time metrics for that period
  time_results_pooled[i, ] <- c(time_pooled["user.self"], time_pooled["sys.self"], time_pooled["elapsed"])
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
  
time_unpooled <- system.time({
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
})
#Store time metrics for that period
time_results_unpooled[i, ] <- c(time_unpooled["user.self"], time_unpooled["sys.self"], time_unpooled["elapsed"])
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
  results_partially <- matrix(0, nrow = nRecommendations, ncol = 5)
  
  # Matrix to store alpha and beta for each arm for each segment
  mAB_partially <- array(1, dim = c(nArms, 2, nSegments)) # Dimensions: arms, parameters (alpha, beta), segments
  
  # Matrix to store it for the populations as a whole 
  mAB_global <-matrix(1,nrow=nArms,ncol=2)
  
  # Segment Counter
  segment_n_runs <- data.frame(
    Segment = 1:5,
    Count_Occurences = rep(0, 5)
  )
  

time_partially <- system.time({  
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
})
#Store time metrics for that period
time_results_partially[i, ] <- c(time_partially["user.self"], time_partially["sys.self"], time_partially["elapsed"])
}

average_time_pooled <- colMeans(time_results_pooled)
average_time_unpooled <- colMeans(time_results_unpooled)
average_time_partially <- colMeans(time_results_partially)

# Initialize a dataframe to store the efficiency results
efficiency_results <- data.frame(
  Model = c("Pooled", "Unpooled", "Partially Pooled"),
  User_CPU_Time = numeric(3),
  System_CPU_Time = numeric(3),
  Elapsed_Time = numeric(3)
)

# Assign the system time results to the dataframe
efficiency_results <- data.frame(
  Model = c("Pooled", "Unpooled", "Partially Pooled"),
  User_CPU_Time = c(average_time_pooled[1], average_time_unpooled[1], average_time_partially[1]),
  System_CPU_Time = c(average_time_pooled[2], average_time_unpooled[2], average_time_partially[2]),
  Elapsed_Time = c(average_time_pooled[3], average_time_unpooled[3], average_time_partially[3])
)

# Print the dataframe to view the results
print(efficiency_results)