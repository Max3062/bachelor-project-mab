# bachelor-project-mab
This repositiory contains the data and code files used in the Bachelor Project which revolved around analyzing how Reinforcement Learning (Multi-Armed Bandits) can be used to optimize News Recommendations on the Yahoo! website. In a nutshell, the goal was to investigate how pooling of learnings between user clusters using a Thompson Sampling algorithm can impact the performance of news recommendations. We used an unpooled model to treat each cluster individually and update learnings only based on their observations. We compared that to a fully pooled model, which treated all the observations as homogeneous observations. Finally, I engineered a partially pooled approach, which uses a logistic function to transition from prioritizing shared learnings between clusters in the beginning (when first observing articles) while gradually moving to a more segment-specific approach. The goal was to mitigate the cold-start problem, which is amplified through segmentation, while still taking advantage of catering to segment-specific needs once they become more certain.

The final report is attached and can be used as a guide to the code files and the overall project. The code files contain extensive comments.

BP_Report --> Final PDF of the report

Main_Code_Final --> Code file containing the clustering of the users and the development of the three algorithms. Also contains the evaluation of the different models.

Hyper_parameter_Selection --> Shows how the hyperparameters of the selected function were chosen.

Exploration_Tracking_Test --> stores code to test and visualize the exploration of the different algorithms.

Computational_Efficiency_Test --> Check to evaluate the computational efficiency of approaches
