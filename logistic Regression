## Load necessary libraries
library(ggplot2)

# Make sure the file path is correct and the file is in CSV format
your_data <- read.csv("C:/Users/tejat/OneDrive/Desktop/pit and balcony project/oulad-students.csv")

# Explore the structure of your dataset
str(your_data)
# Check for NAs in the variables
sum(is.na(your_data$module_presentation_length))

sum(is.na(your_data$studied_credits))

sum(is.na(your_data$final_result))

your_data$final_result_binary <- ifelse(your_data$final_result == "Pass", 1, 0)

# Fit logistic regression model
logit_model <- glm(final_result_binary ~ module_presentation_length + studied_credits, data = your_data, family = "binomial")

# Predict on test data
predictions_logit <- predict(logit_model, type = "response")

# Plot the predictions vs. actuals
ggplot(data = your_data, aes(x = final_result_binary, y = predictions_logit)) +
  geom_point(color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
  labs(title = "Logistic Regression: Predictions vs. Actuals",
       x = "Actual Values",
       y = "Predicted Probabilities")
summary(logit_model)




##output
#summary(logit_model) provides information about the logistic regression model fitted to your data
> summary(logit_model)

Call:
glm(formula = final_result_binary ~ module_presentation_length + 
    studied_credits, family = "binomial", data = your_data)

Coefficients:
                             Estimate Std. Error z value Pr(>|z|)    
(Intercept)                -1.8624262  0.2266206  -8.218  < 2e-16 ***
module_presentation_length  0.0070903  0.0008773   8.082 6.38e-16 ***
studied_credits            -0.0056967  0.0003045 -18.706  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 43264  on 32592  degrees of freedom
Residual deviance: 42818  on 32590  degrees of freedom
AIC: 42824

Number of Fisher Scoring iterations: 4

#Explanation From the provided output
the AIC value is 42824, which provides a measure of model fit relative to other models.
Significance Codes: The significance codes (0 ‘’ 0.001 ‘’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1) indicate the level of significance for each coefficient estimate.
'*'**: Highly significant (p < 0.001)
''**: Significant (p < 0.01)
'*': Marginally significant (p < 0.05)
'.': Not significant (p > 0.05)

##Graph plot
