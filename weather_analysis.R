
# ==========================================
# WEATHER ANALYSIS AND PREDICTION USING R
# ==========================================

# STEP 1: INSTALL REQUIRED PACKAGES

packages <- c("ggplot2", "dplyr", "lubridate",
              "tidyr", "scales")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

# STEP 2: LOAD LIBRARIES

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)

# STEP 3: CREATE OUTPUT FOLDER

if (!dir.exists("Weather_Output")) {
  dir.create("Weather_Output")
}

# STEP 4: CREATE SAMPLE WEATHER DATASET

set.seed(123)

Date <- seq(as.Date("2025-01-01"),
            as.Date("2025-12-31"),
            by = "day")

n <- length(Date)
day <- yday(Date)

weather <- data.frame(
  Date = Date,
  
  Temperature = round(
    29 + 5 * sin(2*pi*(day-80)/365) +
      rnorm(n, 0, 2), 1),
  
  Humidity = round(
    pmin(98, pmax(35,
                  70 + 12*sin(2*pi*(day+30)/365) +
                    rnorm(n, 0, 8))), 1),
  
  Rainfall = round(
    pmax(0, rnorm(n, 5, 6)), 1),
  
  WindSpeed = round(
    runif(n, 5, 25), 1)
)

# STEP 5: ADD MONTH AND SEASON

weather$Month <- factor(
  month(weather$Date, label = TRUE),
  levels = month.abb
)

weather$Season <- case_when(
  month(weather$Date) %in% c(3,4,5) ~ "Summer",
  month(weather$Date) %in% c(6,7,8,9) ~ "Monsoon",
  month(weather$Date) %in% c(10,11) ~ "Post-Monsoon",
  TRUE ~ "Winter"
)

# STEP 6: CLASSIFY WEATHER CONDITIONS

weather$Condition <- case_when(
  weather$Rainfall >= 10 ~ "Rainy",
  weather$Temperature >= 35 ~ "Hot",
  weather$Humidity >= 80 ~ "Humid",
  TRUE ~ "Normal"
)

# STEP 7: DISPLAY DATASET

cat("FIRST 10 WEATHER RECORDS\n")
print(head(weather, 10))

cat("DATASET STRUCTURE\n")
str(weather)

cat("DATASET SUMMARY\n")
print(summary(weather))

# STEP 8: STATISTICAL ANALYSIS

cat("\nWEATHER STATISTICS\n")

cat("Average Temperature:",
    mean(weather$Temperature), "C\n")

cat("Maximum Temperature:",
    max(weather$Temperature), "C\n")

cat("Minimum Temperature:",
    min(weather$Temperature), "C\n")

cat("Average Humidity:",
    mean(weather$Humidity), "%\n")

cat("Total Rainfall:",
    sum(weather$Rainfall), "mm\n")

cat("Average Wind Speed:",
    mean(weather$WindSpeed), "km/h\n")

# STEP 9: MONTHLY ANALYSIS

monthly <- weather %>%
  group_by(Month) %>%
  summarise(
    Average_Temperature = round(mean(Temperature), 2),
    Maximum_Temperature = max(Temperature),
    Minimum_Temperature = min(Temperature),
    Average_Humidity = round(mean(Humidity), 2),
    Total_Rainfall = round(sum(Rainfall), 2),
    Average_WindSpeed = round(mean(WindSpeed), 2),
    .groups = "drop"
  )

cat("\nMONTHLY WEATHER ANALYSIS\n")
print(monthly)

# STEP 10: DAILY TEMPERATURE GRAPH

p1 <- ggplot(weather, aes(Date, Temperature)) +
  geom_line(color = "red", linewidth = 0.6) +
  labs(
    title = "Daily Temperature Analysis",
    x = "Date",
    y = "Temperature (C)"
  ) +
  theme_minimal()

print(p1)

ggsave("Weather_Output/Temperature_Trend.png",
       p1, width = 9, height = 5)

# STEP 11: MONTHLY TEMPERATURE GRAPH

p2 <- ggplot(monthly,
             aes(Month, Average_Temperature)) +
  geom_col(fill = "orange") +
  labs(
    title = "Monthly Average Temperature",
    x = "Month",
    y = "Temperature (C)"
  ) +
  theme_minimal()

print(p2)

ggsave("Weather_Output/Monthly_Temperature.png",
       p2, width = 8, height = 5)

# STEP 12: RAINFALL ANALYSIS

p3 <- ggplot(monthly,
             aes(Month, Total_Rainfall)) +
  geom_col(fill = "blue") +
  labs(
    title = "Monthly Rainfall Analysis",
    x = "Month",
    y = "Rainfall (mm)"
  ) +
  theme_minimal()

print(p3)

ggsave("Weather_Output/Rainfall_Analysis.png",
       p3, width = 8, height = 5)

# STEP 13: HUMIDITY DISTRIBUTION

p4 <- ggplot(weather, aes(Humidity)) +
  geom_histogram(
    bins = 20,
    fill = "green",
    color = "white"
  ) +
  labs(
    title = "Humidity Distribution",
    x = "Humidity (%)",
    y = "Number of Days"
  ) +
  theme_minimal()

print(p4)

ggsave("Weather_Output/Humidity_Analysis.png",
       p4, width = 8, height = 5)

# STEP 14: TEMPERATURE VS HUMIDITY

p5 <- ggplot(weather,
             aes(Humidity, Temperature,
                 color = Condition)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Temperature vs Humidity",
    x = "Humidity (%)",
    y = "Temperature (C)"
  ) +
  theme_minimal()

print(p5)

ggsave("Weather_Output/Temperature_Humidity.png",
       p5, width = 8, height = 5)

# STEP 15: WEATHER CONDITION ANALYSIS

p6 <- ggplot(weather, aes(Condition,
                          fill = Condition)) +
  geom_bar() +
  labs(
    title = "Weather Condition Analysis",
    x = "Weather Condition",
    y = "Number of Days"
  ) +
  theme_minimal()

print(p6)

ggsave("Weather_Output/Weather_Conditions.png",
       p6, width = 8, height = 5)

# STEP 16: CORRELATION ANALYSIS

correlation <- cor(
  weather[, c("Temperature", "Humidity",
              "Rainfall", "WindSpeed")]
)

cat("\nCORRELATION MATRIX\n")
print(round(correlation, 2))

# STEP 17: LINEAR REGRESSION MODEL

weather$DayNumber <- 1:n

model <- lm(
  Temperature ~ DayNumber,
  data = weather
)

cat("\nLINEAR REGRESSION MODEL\n")
print(summary(model))

# STEP 18: TEMPERATURE TREND ESTIMATION

future <- data.frame(
  DayNumber = (n + 1):(n + 7)
)

future$Predicted_Temperature <-
  round(predict(model, newdata = future), 2)

future$Date <- seq(
  max(weather$Date) + 1,
  by = "day",
  length.out = 7
)

cat("\nNEXT 7 DAYS TREND-BASED ESTIMATES\n")

print(future[, c("Date",
                 "Predicted_Temperature")])

# STEP 19: REGRESSION GRAPH

p7 <- ggplot(weather,
             aes(DayNumber, Temperature)) +
  geom_point(color = "gray", alpha = 0.5) +
  geom_smooth(method = "lm",
              color = "blue",
              se = TRUE) +
  labs(
    title = "Temperature Prediction Using Linear Regression",
    x = "Day Number",
    y = "Temperature (C)"
  ) +
  theme_minimal()

print(p7)

ggsave("Weather_Output/Regression_Prediction.png",
       p7, width = 8, height = 5)

# STEP 20: SAVE DATASETS

write.csv(
  weather,
  "Weather_Output/Weather_Dataset.csv",
  row.names = FALSE
)

write.csv(
  monthly,
  "Weather_Output/Monthly_Analysis.csv",
  row.names = FALSE
)

write.csv(
  future,
  "Weather_Output/Temperature_Prediction.csv",
  row.names = FALSE
)

write.csv(
  correlation,
  "Weather_Output/Correlation_Matrix.csv"
)

# STEP 21: FINAL RESULT

cat("\n================================\n")
cat("WEATHER ANALYSIS COMPLETED!\n")
cat("Dataset created successfully.\n")
cat("Graphs generated successfully.\n")
cat("Statistical analysis completed.\n")
cat("Temperature trend estimation completed.\n")
cat("================================\n")

