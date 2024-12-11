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

# Build a linear regression model----
# Model(Target ~ Predictor 1 + Predictor 2 + ...)
lm_model <- lm(Price ~ Year + Month + Day + Price_Lag1, data = train_data) # poly(Predictor1, 2) + poly(Predictor2, 2) + Predictor3

# Display the model summary
summary(lm_model)

# Testing----
# Make predictions on the testing dataset
test_data$Predicted <- predict(lm_model, newdata = test_data)

# Check the first few rows of the test data with predictions
head(test_data)

# Model Evaluation----
# Compare actual vs. predicted values
results <- data.frame(
  Actual = test_data$Price,
  Predicted = test_data$Predicted
)
head(results)

# Calculate Mean Squared Error (MSE)
mse <- mean((results$Actual - results$Predicted)^2)
print(paste("MSE of Linear Regression:", round(mse, 4)))

# Calculate RSquare
# Calculate Total Sum of Squares (SS_tot)
ss_tot <- sum((results$Actual - mean(results$Actual))^2, na.rm = TRUE)

# Calculate Residual Sum of Squares (SS_res)
ss_res <- sum((results$Actual - results$Predicted)^2, na.rm = TRUE)

# Calculate R-squared
r_squared <- 1 - (ss_res / ss_tot)

# Print the R-squared value
print(paste("R-squared of Linear Regression:", round(r_squared, 4)))


# Visualization----
# Plot Actual vs Predicted
plot(results$Actual, results$Predicted,
     main = "Actual vs Predicted Price",
     xlab = "Actual Price ($)",
     ylab = "Predicted Price ($)",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Add a diagonal reference line
grid()

# Create a ggplot to visualize testing predictions vs. actuals over time
ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = Price, color = "Actual"), linewidth = 1) +  # Actual values
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1) +  # Predicted values
  labs(title = "Testing Predictions vs Actuals Over Time",
       x = "Date", y = "Price ($)") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

# Model Equation----
coefficients <- coef(lm_model)
intercept <- coefficients[1]
slopes <- coefficients[-1]

equation <- paste0("Adjusted = ", round(intercept, 4), 
                   " + ", paste0(round(slopes, 4), " * ", names(slopes), collapse = " + "))
cat("Regression Equation of Linear Regression:\n")
cat(equation, "\n")

