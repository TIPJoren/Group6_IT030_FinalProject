# Load libraries
library(tidyverse)      # for data wrangling
library(caret)          # for modeling and evaluation
library(randomForest)   # for random forest model

# Load the cleaned dataset (update path if needed)
cyber_data <- read.csv("C:/Users/Joren Hanz/Documents/cleaned_DA_FinalProj.csv")

# View structure and columns
str(cyber_data)
colnames(cyber_data)

# Create a binary label: 1 if attack is phishing, 0 otherwise
cyber_data$Phishing_Flag <- ifelse(cyber_data$Attack.Type == "Phishing", 1, 0)

# Select relevant columns and convert to factors
model_data <- cyber_data %>%
  select(Phishing_Flag, Target.Industry, Country, Attack.Source) %>%
  na.omit() %>%
  mutate(across(everything(), as.factor))  # make sure all columns are categorical

# Set random seed for reproducibility
set.seed(42)

# Split data: 80% train, 20% test
train_index <- createDataPartition(model_data$Phishing_Flag, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# Train logistic regression model
logit_model <- train(Phishing_Flag ~ ., data = train_data, method = "glm", family = "binomial")

# Predict on test data
logit_pred <- predict(logit_model, test_data)

# Evaluate with confusion matrix
logit_results <- confusionMatrix(logit_pred, test_data$Phishing_Flag)
print(logit_results)

# Train random forest model with 100 trees
rf_model <- randomForest(Phishing_Flag ~ ., data = train_data, ntree = 100)

# Predict on test data
rf_pred <- predict(rf_model, test_data)

# Evaluate performance
rf_results <- confusionMatrix(rf_pred, test_data$Phishing_Flag)
print(rf_results)

# Plot feature importance
varImpPlot(rf_model)

# Save the confusion matrix results as a text file
sink("model_results.txt")
print("Logistic Regression Results:")
print(logit_results)
print("Random Forest Results:")
print(rf_results)
sink()

# Save plot as image
png("feature_importance_rf.png", width = 800, height = 600)
varImpPlot(rf_model)
dev.off()