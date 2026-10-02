
# ==================================================
# PROJECT: HOUSE MARKET TRENDS ANALYSIS USING R
# ==================================================

# 1. Load Packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# 2. Create Output Folder
output_folder <- "House_Market_Output"

if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# 3. Generate Sample Dataset (100 Houses)
set.seed(123)

n <- 100

locations <- c(
  "Coimbatore",
  "Chennai",
  "Madurai",
  "Salem",
  "Trichy"
)

house_data <- data.frame(
  House_ID = 1:n,
  
  Location = sample(
    locations, n, replace = TRUE
  ),
  
  Area_sqft = sample(
    seq(600, 3000, by = 100),
    n, replace = TRUE
  ),
  
  Bedrooms = sample(
    1:5, n, replace = TRUE
  ),
  
  Bathrooms = sample(
    1:4, n, replace = TRUE
  ),
  
  House_Age = sample(
    0:30, n, replace = TRUE
  ),
  
  Sale_Date = sample(
    seq.Date(
      as.Date("2022-01-01"),
      as.Date("2026-09-01"),
      by = "month"
    ),
    n, replace = TRUE
  )
)

# 4. Generate Sample House Prices
location_rate <- c(
  "Coimbatore" = 4500,
  "Chennai" = 6500,
  "Madurai" = 3800,
  "Salem" = 3200,
  "Trichy" = 4000
)

house_data <- house_data %>%
  mutate(
    Base_Rate = location_rate[Location],
    
    House_Price =
      Area_sqft * Base_Rate +
      Bedrooms * 150000 +
      Bathrooms * 100000 -
      House_Age * 25000 +
      as.numeric(Sale_Date - min(Sale_Date)) * 8 +
      rnorm(n, mean = 0, sd = 400000),
    
    House_Price = round(
      pmax(House_Price, 500000)
    ),
    
    Price_Lakhs = round(
      House_Price / 100000, 2
    ),
    
    Price_Per_Sqft = round(
      House_Price / Area_sqft, 2
    )
  ) %>%
  select(-Base_Rate)

# 5. Display Dataset
cat("HOUSE MARKET DATASET\n")
print(head(house_data, 10))

cat("\nDATA SUMMARY\n")
print(summary(house_data))

# 6. Check Missing Values
cat("\nMISSING VALUES\n")
print(colSums(is.na(house_data)))

# 7. Data Cleaning
house_data <- house_data %>%
  drop_na() %>%
  filter(
    Area_sqft > 0,
    House_Price > 0,
    Bedrooms > 0
  )

# 8. Overall Market Statistics
market_summary <- house_data %>%
  summarise(
    Total_Houses = n(),
    Average_Price = round(mean(House_Price), 2),
    Maximum_Price = max(House_Price),
    Minimum_Price = min(House_Price),
    Average_Area = round(mean(Area_sqft), 2),
    Average_Price_Per_Sqft =
      round(mean(Price_Per_Sqft), 2)
  )

cat("\nOVERALL MARKET SUMMARY\n")
print(market_summary)

# 9. Location-wise Analysis
location_summary <- house_data %>%
  group_by(Location) %>%
  summarise(
    Total_Houses = n(),
    Average_Price_Lakhs =
      round(mean(Price_Lakhs), 2),
    Maximum_Price_Lakhs =
      round(max(Price_Lakhs), 2),
    Average_Area =
      round(mean(Area_sqft), 2),
    Average_Price_Per_Sqft =
      round(mean(Price_Per_Sqft), 2),
    .groups = "drop"
  )

cat("\nLOCATION-WISE HOUSE ANALYSIS\n")
print(location_summary)

# 10. Year-wise Market Trends
year_summary <- house_data %>%
  mutate(Year = year(Sale_Date)) %>%
  group_by(Year) %>%
  summarise(
    Total_Sales = n(),
    Average_Price_Lakhs =
      round(mean(Price_Lakhs), 2),
    .groups = "drop"
  )

cat("\nYEAR-WISE MARKET TRENDS\n")
print(year_summary)

# 11. Bedroom-wise Analysis
bedroom_summary <- house_data %>%
  group_by(Bedrooms) %>%
  summarise(
    Average_Price_Lakhs =
      round(mean(Price_Lakhs), 2),
    Number_of_Houses = n(),
    .groups = "drop"
  )

print(bedroom_summary)

# ==================================================
# 12. VISUALIZATION
# ==================================================

# Graph 1: House Price Distribution
p1 <- ggplot(
  house_data,
  aes(Price_Lakhs)
) +
  geom_histogram(
    bins = 20,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "House Price Distribution",
    x = "House Price (Lakhs)",
    y = "Number of Houses"
  ) +
  theme_minimal()

print(p1)

ggsave(
  file.path(output_folder, "House_Price_Distribution.png"),
  p1, width = 8, height = 5
)

# Graph 2: Location-wise Average House Price
p2 <- ggplot(
  location_summary,
  aes(Location, Average_Price_Lakhs, fill = Location)
) +
  geom_col() +
  labs(
    title = "Average House Price by Location",
    x = "Location",
    y = "Average Price (Lakhs)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

ggsave(
  file.path(output_folder, "Location_Price_Comparison.png"),
  p2, width = 8, height = 5
)

# Graph 3: House Area vs Price
p3 <- ggplot(
  house_data,
  aes(Area_sqft, Price_Lakhs)
) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(
    method = "lm",
    color = "red",
    se = TRUE
  ) +
  labs(
    title = "House Area vs House Price",
    x = "Area (Square Feet)",
    y = "House Price (Lakhs)"
  ) +
  theme_minimal()

print(p3)

ggsave(
  file.path(output_folder, "Area_vs_Price.png"),
  p3, width = 8, height = 5
)

# Graph 4: Bedroom-wise Price
p4 <- ggplot(
  house_data,
  aes(
    factor(Bedrooms),
    Price_Lakhs,
    fill = factor(Bedrooms)
  )
) +
  geom_boxplot() +
  labs(
    title = "House Price Based on Bedrooms",
    x = "Number of Bedrooms",
    y = "House Price (Lakhs)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p4)

ggsave(
  file.path(output_folder, "Bedroom_Price_Analysis.png"),
  p4, width = 8, height = 5
)

# Graph 5: Year-wise Price Trend
p5 <- ggplot(
  year_summary,
  aes(Year, Average_Price_Lakhs)
) +
  geom_line(
    color = "blue",
    linewidth = 1
  ) +
  geom_point(size = 3, color = "red") +
  labs(
    title = "House Market Price Trend",
    x = "Year",
    y = "Average Price (Lakhs)"
  ) +
  theme_minimal()

print(p5)

ggsave(
  file.path(output_folder, "Yearly_Price_Trend.png"),
  p5, width = 8, height = 5
)

# Graph 6: House Age vs Price
p6 <- ggplot(
  house_data,
  aes(House_Age, Price_Lakhs)
) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(
    method = "lm",
    color = "red"
  ) +
  labs(
    title = "House Age vs House Price",
    x = "House Age (Years)",
    y = "House Price (Lakhs)"
  ) +
  theme_minimal()

print(p6)

ggsave(
  file.path(output_folder, "Age_vs_Price.png"),
  p6, width = 8, height = 5
)

# Graph 7: Price per Square Foot by Location
p7 <- ggplot(
  house_data,
  aes(Location, Price_Per_Sqft, fill = Location)
) +
  geom_boxplot() +
  labs(
    title = "Price per Square Foot by Location",
    x = "Location",
    y = "Price per Square Foot"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p7)

ggsave(
  file.path(output_folder, "Price_Per_Sqft.png"),
  p7, width = 8, height = 5
)

# ==================================================
# 13. CORRELATION ANALYSIS
# ==================================================

correlation_data <- house_data %>%
  select(
    Area_sqft,
    Bedrooms,
    Bathrooms,
    House_Age,
    House_Price,
    Price_Per_Sqft
  )

correlation_matrix <- cor(correlation_data)

cat("\nCORRELATION MATRIX\n")
print(round(correlation_matrix, 2))

# ==================================================
# 14. LINEAR REGRESSION MODEL
# ==================================================

# Split data into training and testing sets
set.seed(123)

train_index <- sample(
  seq_len(nrow(house_data)),
  size = floor(0.8 * nrow(house_data))
)

train_data <- house_data[train_index, ]
test_data <- house_data[-train_index, ]

# Build regression model
model <- lm(
  House_Price ~ Area_sqft +
    Bedrooms +
    Bathrooms +
    House_Age +
    Location +
    Sale_Date,
  data = train_data
)

cat("\nREGRESSION MODEL SUMMARY\n")
print(summary(model))

# Predict test data prices
predicted_price <- predict(
  model,
  newdata = test_data
)

# Calculate model performance
actual_price <- test_data$House_Price

MAE <- mean(
  abs(actual_price - predicted_price)
)

RMSE <- sqrt(
  mean((actual_price - predicted_price)^2)
)

R2 <- 1 -
  sum((actual_price - predicted_price)^2) /
  sum((actual_price - mean(actual_price))^2)

cat("\nMODEL PERFORMANCE\n")
cat("MAE:", round(MAE, 2), "\n")
cat("RMSE:", round(RMSE, 2), "\n")
cat("R-squared:", round(R2, 3), "\n")

# ==================================================
# 15. NEW HOUSE PRICE PREDICTION
# ==================================================

new_house <- data.frame(
  Area_sqft = 1500,
  Bedrooms = 3,
  Bathrooms = 2,
  House_Age = 5,
  Location = "Coimbatore",
  Sale_Date = as.Date("2026-10-01")
)

predicted_house_price <- predict(
  model,
  newdata = new_house
)

cat("\nNEW HOUSE PRICE PREDICTION\n")
cat(
  "Predicted House Price: Rs.",
  round(predicted_house_price, 2),
  "\n"
)

cat(
  "Predicted Price in Lakhs:",
  round(predicted_house_price / 100000, 2),
  "\n"
)

# ==================================================
# 16. EXPORT DATA AND REPORTS
# ==================================================

write.csv(
  house_data,
  file.path(output_folder, "House_Market_Dataset.csv"),
  row.names = FALSE
)

write.csv(
  market_summary,
  file.path(output_folder, "Market_Summary.csv"),
  row.names = FALSE
)

write.csv(
  location_summary,
  file.path(output_folder, "Location_Analysis.csv"),
  row.names = FALSE
)

write.csv(
  year_summary,
  file.path(output_folder, "Yearly_Market_Trends.csv"),
  row.names = FALSE
)

write.csv(
  bedroom_summary,
  file.path(output_folder, "Bedroom_Analysis.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(correlation_matrix),
  file.path(output_folder, "Correlation_Matrix.csv")
)

prediction_report <- data.frame(
  Actual_Price = actual_price,
  Predicted_Price = round(predicted_price, 2)
)

write.csv(
  prediction_report,
  file.path(output_folder, "Prediction_Report.csv"),
  row.names = FALSE
)

capture.output(
  summary(model),
  file = file.path(output_folder, "Regression_Report.txt")
)

# Save model performance
model_metrics <- data.frame(
  Metric = c("MAE", "RMSE", "R_Squared"),
  Value = c(MAE, RMSE, R2)
)

write.csv(
  model_metrics,
  file.path(output_folder, "Model_Performance.csv"),
  row.names = FALSE
)

# ==================================================
# 17. FINAL RESULT
# ==================================================

cat("\n====================================\n")
cat("HOUSE MARKET TRENDS ANALYSIS COMPLETED\n")
cat("====================================\n")

cat("Total Houses:", nrow(house_data), "\n")

cat(
  "Average House Price (Lakhs):",
  round(mean(house_data$Price_Lakhs), 2),
  "\n"
)

cat(
  "Maximum House Price (Lakhs):",
  max(house_data$Price_Lakhs),
  "\n"
)

cat(
  "Minimum House Price (Lakhs):",
  min(house_data$Price_Lakhs),
  "\n"
)

cat("Output Folder:", output_folder, "\n")
cat("Graphs, reports and prediction files saved.\n")
cat("====================================\n")
