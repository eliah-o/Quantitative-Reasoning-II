# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load your dataset (replace path with your actual dataset path)
dataset <- read.csv("~/Downloads/simulated_salary_dataset.csv")

# Rebuild your models for visualization purposes (adjust with your actual variable names)
Original_Model <- lm(Salary ~ Education_Years + Work_Experience, data = dataset)
Adjusted_Model <- lm(Salary ~ Education_Years + Work_Experience + City_Population + Age, data = dataset)

# Create a dataframe for visualization
plot_data <- dataset %>%
  mutate(
    Predicted_Original = predict(Original_Model),
    Predicted_Adjusted = predict(Adjusted_Model),
    Residuals_Original = resid(Original_Model),
    Residuals_Adjusted = resid(Adjusted_Model)
  )

# Visualization 1: Predicted vs Actual values (Original Model)
ggplot(plot_data, aes(x = Predicted_Original, y = Salary)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Values (Original Model)",
       x = "Predicted Values",
       y = "Actual Values")

# Visualization 2: Residuals Distribution
ggplot(plot_data, aes(x = Residuals_Original)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Residuals Distribution (Original Model)",
       x = "Residuals",
       y = "Frequency") 

# Visualization 3: Residuals vs Predicted values
ggplot(plot_data, aes(x = Predicted_Original, y = Residuals_Original)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Predicted (Original Model) Values",
       x = "Predicted Values",
       y = "Residuals") 
