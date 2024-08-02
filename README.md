# Load the dataset
data(mtcars)

# Convert 'am' (transmission) to a factor variable
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))

# Fit logistic regression model
model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)

# Summary of the model
summary(model)

# Predict probabilities
predicted_probs <- predict(model, type = "response")

# Predicted class
predicted_class <- ifelse(predicted_probs > 0.5, "Manual", "Automatic")

# Confusion matrix
table(Predicted = predicted_class, Actual = mtcars$am)

# Calculate accuracy
mean(predicted_class == mtcars$am)

# Load necessary library for plotting
library(ggplot2)

# Plot predicted probabilities against weight
ggplot(mtcars, aes(x = wt, y = predicted_probs)) +
  geom_point(aes(color = am), size = 3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Predicted Probability of Manual Transmission vs. Weight (wt)",
       x = "Weight (1000 lbs)",
       y = "Predicted Probability of Manual Transmission")
