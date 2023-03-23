# Introduction ----
#Created by:
#Stephanie Wong 
#U3209816
#For the University of Canberra Capstone Project Semester 1, 2023.
#This code contains the data analysis for the results of model metrics 
#run for the project. It includes metric results for KNN, SVM, and RF.


# Set up ----
#Load libraries
library(tidyverse)
library(forcats)

#Set working directory
setwd(getwd())

#Load datasets
results <- read.csv("Results.csv")
featSel <- read.csv("FeatureSelection.csv")
CVA <- read.csv("KFoldCVAccuracies.csv")

# Feature Selection ----
#view features
str(featSel)

#plot feature selection results
ggplot(
  data = featSel, 
  mapping = aes(
    x = reorder(Feature.Name, Score),
    y = Score,
    fill = Score
  )) +
  geom_bar(stat = "identity", fill = "#9999CC") +
  coord_flip() +
  labs(
    title = "Feature Selection Results",
    x = "Feature", 
    y = "Score"
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 20)
  )

#Comparison between cross validation fluctuations ----
#view CVA dataset
str(CVA)
summary(CVA)

#plot cross validation fluctuations
ggplot(data = CVA,
         aes(
           x = Split,
           y = Accuracy,
           color = Algorithm,
           linewidth = 2
         )
    ) +
    geom_line(
    ) +
    geom_point(
       size = 5
    ) +
    scale_color_manual(values = c(
      "SVM" = "#CC6666",
      "RF" = "#000000",
      "KNN" = "#9999CC"
    )) +
    ylim(0.895,0.97) +
    labs(
      title = "Cross Validation Accuracy Fluctuations",
      x = "Splits", 
      y = "Accuracy"
    ) +
    theme(
      text = element_text(size = 20)
    )

# Cross Validation Variance ----
#view results dataset
str(results)
summary(results)

#plot cross validation variances
ggplot(
  data = subset(
    results, 
    Metric %in% "Cross validation variance"
    ),
  aes(
    x = Model,
    y = Score,
    fill = Model,
    )
  ) +
  geom_bar(stat = "identity"
  ) +
  scale_fill_manual(values = c(
    "SVM" = "#CC6666",
    "RF" = "#000000",
    "KNN" = "#9999CC"
  )) +
  labs(
    title = "Cross validation variances",
    x = "Model", 
    y = "Variance"
  ) +
  theme(
    text = element_text(size = 20)
  )
  
  
  
# Malicious Recall ----
#plot malicious recall
ggplot(
  data = subset(
    results, 
    Metric %in% "Malicious recall"
    ),
  aes(
    x = Model,
    y = Score,
    fill = Model,
  )
) +
  geom_bar(stat = "identity"
  ) +
  scale_fill_manual(values = c(
    "SVM" = "#CC6666",
    "RF" = "#000000",
    "KNN" = "#9999CC"
  )) +
  labs(
    title = "Malicious Recall Values",
    x = "Model", 
    y = "Malicious Recall"
  ) +
  theme(
    text = element_text(size = 20)
  )

# AUC ----
#plot AUC
ggplot(
  data = subset(
    results, 
    Metric %in% "AUC"
    ),
  aes(
    x = Model,
    y = Score,
    fill = Model,
  )
) +
  geom_bar(stat = "identity"
  ) +
  scale_fill_manual(values = c(
    "SVM" = "#CC6666",
    "RF" = "#000000",
    "KNN" = "#9999CC"
  )) +
  labs(
    title = "AUC Values",
    x = "Model", 
    y = "AUC Value"
  ) +
  theme(
    text = element_text(size = 20)
  )

# Overall Score ----
#plot overall score
ggplot(
  data = subset(
    results, 
    Metric %in% "Overall score"
    ),
  aes(
    x = Model,
    y = Score,
    fill = Model,
  )
) +
  geom_bar(stat = "identity"
  ) +
  scale_fill_manual(values = c(
    "SVM" = "#CC6666",
    "RF" = "#000000",
    "KNN" = "#9999CC"
  )) +
  labs(
    title = "Overall Scores",
    x = "Model", 
    y = "Overall Score"
  ) +
  theme(
    text = element_text(size = 20)
  )
