# Load necessary library
library(lubridate)
library(dplyr)
library(ggplot2)

# Load the data from the URL
url <- "https://raw.githubusercontent.com/friansakoko/Forecasting/refs/heads/main/daily_csv.csv"
model_data <- read.csv(url)
View(model_data)

# Convert Date column to Date type
model_data$Date <- as.Date(model_data$Date)

# Extract year and create a new column
model_data$Year <- year(model_data$Date)
model_data$Month <- month(model_data$Date)
model_data$Day <- day(model_data$Date)

# Add lag variable for Price and replace NA with 0
model_data <- model_data %>%
  mutate(Price_Lag1 = lag(Price, default = 0)) # Default replaces NA with 0

# Remove the Lag_Price column (if Necessary)
#model_data <- model_data %>%
  #select(-Lag_Price) #select(-"The column name")

# View the updated data
View(model_data)

# Build dataframe----

# Remove missing values (if any)
model_data <- na.omit(model_data)

# Define cutoff for chronological splitting (e.g., 80% of the dataset)
cutoff_date <- model_data$Date[floor(0.8 * nrow(model_data))]

# Split data based on the cutoff date
train_data <- model_data %>% filter(Date <= cutoff_date)
test_data <- model_data %>% filter(Date > cutoff_date)

# Check the ranges
print(range(train_data$Date))
print(range(test_data$Date))

# Load necessary library for Decision Tree
library(rpart)
library(rpart.plot)  # For visualizing the tree

# Build a Decision Tree model----
# Formula(Target ~ Predictor1 + Predictor2 + ...)
dt_model <- rpart(Price ~ Year + Month + Day + Price_Lag1, 
                  data = train_data, 
                  method = "anova")  # Use "anova" for regression, "class" for classification

# Visualize the Decision Tree
rpart.plot(dt_model, type = 2, extra = 101, fallen.leaves = TRUE, main = "Decision Tree")

# Display the Decision Tree model summary
summary(dt_model)

# Testing----
# Make predictions on the testing dataset
test_data$Predicted_DT <- predict(dt_model, newdata = test_data)

# Model Evaluation----
# Compare actual vs predicted values for Decision Tree
results_dt <- data.frame(
  Actual = test_data$Price,
  Predicted = test_data$Predicted_DT
)

# Calculate Mean Squared Error (MSE) for Decision Tree
mse_dt <- mean((results_dt$Actual - results_dt$Predicted)^2)
print(paste("MSE of Decision Tree:", round(mse_dt, 4)))

# Calculate R-squared for Decision Tree
ss_res_dt <- sum((results_dt$Actual - results_dt$Predicted)^2, na.rm = TRUE)
r_squared_dt <- 1 - (ss_res_dt / ss_tot)  # Reuse SS_tot from Linear Regression
print(paste("R-squared of Decision Tree:", round(r_squared_dt, 4)))

# Visualization----
# Create a ggplot to visualize testing predictions vs. actuals over time for Decision Tree
ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = Price, color = "Actual"), linewidth = 1) +  # Actual values
  geom_line(aes(y = Predicted_DT, color = "Predicted"), linewidth = 1) +  # Predicted values
  labs(title = "Decision Tree: Testing Predictions vs Actuals Over Time",
       x = "Date", y = "Price ($)") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "green")) +
  theme_minimal()


