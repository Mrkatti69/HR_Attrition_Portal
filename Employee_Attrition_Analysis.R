# Load necessary libraries
library(readr)
library(dplyr)
library(caret)

# Load the dataset
data <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Inspect the data
head(data)

sum(is.na(data))

# 14. Create the boxplot
boxplot(data$MonthlyIncome, main = "Boxplot of Monthly Income")

# 17. Calculate quantiles
q1 <- quantile(data$MonthlyIncome, 0.25)
q3 <- quantile(data$MonthlyIncome, 0.75)
IQR <- q3 - q1

# 20. Filter outliers
data <- data %>% filter(MonthlyIncome >= (q1 - 1.5 * IQR) & 
                          MonthlyIncome <= (q3 + 1.5 * IQR))

# 23. Plot distribution
ggplot(data, aes(x = Attrition)) +
  geom_bar(fill = "blue") +
  labs(title = "Attrition Distribution", x = "Attrition", y = "Count")

# Plot the relationship between job satisfaction and attrition
ggplot(data, aes(x = JobSatisfaction, fill = Attrition)) + 
  geom_bar(position = "dodge") +
  labs(title = "Job Satisfaction vs Attrition", x = "Job Satisfaction", y = "Count")                        

# Plot the relationship between monthly income and attrition
ggplot(data, aes(x = Attrition, y = MonthlyIncome)) + 
  geom_boxplot() +
  labs(title = "Monthly Income vs Attrition", x = "Attrition", y = "Monthly Income")

# Plot the relationship between age and attrition
ggplot(data, aes(x = Age, fill = Attrition)) + 
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Age vs Attrition", x = "Age", y = "Count")

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Attrition, p = .7, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[ trainIndex,]
testData  <- data[-trainIndex,]

# 1. Create the new features mentioned in the tutorial
data$totalSatisfaction <- data$JobSatisfaction + 
  data$EnvironmentSatisfaction + 
  data$RelationshipSatisfaction

data$yearsAtCompanyByAge <- data$YearsAtCompany / data$Age

# 2. Re-split the datasets so they include these new features
trainData <- data[trainIndex, ]
testData  <- data[-trainIndex, ]

# 3. Convert the Attrition column to a factor so the model knows it is a classification task
trainData$Attrition <- as.factor(trainData$Attrition)
testData$Attrition  <- as.factor(testData$Attrition)

# 4. Load the library and train the Random Forest model
library(randomForest)
rf_model <- randomForest(Attrition ~ ., data = trainData, importance = TRUE)
print(rf_model)

# 5. Predict on testing data and print the accuracy results
rf_pred <- predict(rf_model, testData)
confusionMatrix(rf_pred, testData$Attrition)

# 1. Combine test data with the predictions
final_results <- testData
final_results$Predicted_Attrition <- rf_pred

# 2. Export the table to a spreadsheet file
write.csv(final_results, "HR_Attrition_Predictions.csv", row.names = FALSE)

# 1. Open a new window for the plot layout
# (This keeps the old histogram safely saved underneath)
dev.new()

# 2. Draw the Variable Importance Plot
varImpPlot(rf_model, 
           main = "Feature Importance for Employee Attrition",
           n.var = 10,
           col = "darkblue",
           pch = 19)

