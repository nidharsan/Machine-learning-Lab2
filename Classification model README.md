##Classification using random forest
library(randomForest)

# Read the dataset and handle missing values
data <- read.csv("C:/Users/tejat/OneDrive/Desktop/pit and balcony project/oulad-students.csv", na.strings = c("", "NA"))

# Check for missing values
if (anyNA(data)) {
  # Remove rows with missing values
  data <- na.omit(data)
}

# Check the structure of the dataset
str(data)

# Convert 'final_result' to factor if it's not already
data$final_result <- factor(data$final_result)

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Assuming 'code_module', 'code_presentation', 'id_student' are not useful for classification
# and removing them from the dataset
data <- data[, !(names(data) %in% c("code_module", "code_presentation", "id_student"))]

# Train a Random Forest model
rf_model <- randomForest(final_result ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(rf_model, test_data)

# Evaluate the model
confusion_matrix <- table(predictions, test_data$final_result)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

print(confusion_df)

# Check unique values of 'value' in freq_data
unique(freq_data$value)

# Print the structure of freq_data
str(freq_data)

# Plot heatmap
ggplot(freq_data, aes(x = Predicted, y = variable, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), vjust = 1) +
  scale_fill_manual(values = c("0" = "lightblue", "1960" = "darkblue")) +
  labs(x = "Actual", y = "Predicted") +
  theme_minimal()

