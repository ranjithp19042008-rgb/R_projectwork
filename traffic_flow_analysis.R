
# ==========================================
# TRAFFIC FLOW ANALYSIS USING R
# 100 DATA RECORDS
# ==========================================

# STEP 1: LOAD PACKAGES

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)

# STEP 2: CREATE OUTPUT FOLDER

if (!dir.exists("Traffic_Output")) {
  dir.create("Traffic_Output")
}

# STEP 3: GENERATE 100 TRAFFIC RECORDS

set.seed(42)

n <- 100

traffic <- data.frame(
  Record_ID = 1:n,
  Date = as.Date("2026-01-01") + 0:(n-1),
  Hour = sample(6:22, n, replace = TRUE),
  Vehicle_Count = sample(10:100, n, replace = TRUE),
  Average_Speed = round(runif(n, 10, 65), 1),
  Road_Occupancy = round(runif(n, 10, 95), 1)
)

traffic$Vehicle_Type <- sample(
  c("Car", "Bike", "Bus", "Truck"),
  n,
  replace = TRUE
)

# STEP 4: TRAFFIC CLASSIFICATION

traffic <- traffic %>%
  mutate(
    Congestion_Score =
      round(0.6 * Vehicle_Count +
              0.4 * Road_Occupancy, 1),
    
    Traffic_Level = case_when(
      Congestion_Score < 40 ~ "Low",
      Congestion_Score < 70 ~ "Moderate",
      TRUE ~ "High"
    ),
    
    Time_Period = case_when(
      Hour < 10 ~ "Morning Peak",
      Hour < 16 ~ "Daytime",
      Hour <= 19 ~ "Evening Peak",
      TRUE ~ "Night"
    )
  )

# STEP 5: DISPLAY DATASET

cat("\nFIRST 10 TRAFFIC RECORDS\n")
print(head(traffic, 10))

cat("\nDATASET STRUCTURE\n")
str(traffic)

cat("\nDATASET SUMMARY\n")
print(summary(traffic))

# STEP 6: TRAFFIC STATISTICS

cat("\nTRAFFIC STATISTICS\n")

cat("Total Records:", nrow(traffic), "\n")

cat("Total Vehicles:",
    sum(traffic$Vehicle_Count), "\n")

cat("Average Vehicle Count:",
    round(mean(traffic$Vehicle_Count), 2), "\n")

cat("Average Speed:",
    round(mean(traffic$Average_Speed), 2),
    "km/h\n")

cat("Maximum Speed:",
    max(traffic$Average_Speed), "km/h\n")

cat("Minimum Speed:",
    min(traffic$Average_Speed), "km/h\n")

cat("Average Road Occupancy:",
    round(mean(traffic$Road_Occupancy), 2), "%\n")

# STEP 7: HOURLY TRAFFIC ANALYSIS

hourly <- traffic %>%
  group_by(Hour) %>%
  summarise(
    Average_Vehicles = round(mean(Vehicle_Count), 2),
    Average_Speed = round(mean(Average_Speed), 2),
    Average_Occupancy = round(mean(Road_Occupancy), 2),
    .groups = "drop"
  )

cat("\nHOURLY TRAFFIC ANALYSIS\n")
print(hourly)

# STEP 8: VEHICLE TYPE ANALYSIS

vehicle_summary <- traffic %>%
  group_by(Vehicle_Type) %>%
  summarise(
    Total = n(),
    Average_Speed = round(mean(Average_Speed), 2),
    .groups = "drop"
  )

cat("\nVEHICLE TYPE ANALYSIS\n")
print(vehicle_summary)

# STEP 9: GRAPH 1 - TRAFFIC VOLUME

p1 <- ggplot(traffic,
             aes(Record_ID, Vehicle_Count)) +
  geom_line(color = "blue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Traffic Volume Analysis",
    x = "Record ID",
    y = "Vehicle Count"
  ) +
  theme_minimal()

print(p1)

ggsave("Traffic_Output/01_Traffic_Volume.png",
       p1, width = 8, height = 5)

# STEP 10: GRAPH 2 - HOURLY TRAFFIC

p2 <- ggplot(hourly,
             aes(Hour, Average_Vehicles)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = 6:22) +
  labs(
    title = "Hourly Traffic Flow Analysis",
    x = "Hour",
    y = "Average Vehicle Count"
  ) +
  theme_minimal()

print(p2)

ggsave("Traffic_Output/02_Hourly_Traffic.png",
       p2, width = 8, height = 5)

# STEP 11: GRAPH 3 - SPEED DISTRIBUTION

p3 <- ggplot(traffic, aes(Average_Speed)) +
  geom_histogram(
    bins = 15,
    fill = "orange",
    color = "white"
  ) +
  labs(
    title = "Traffic Speed Distribution",
    x = "Average Speed (km/h)",
    y = "Frequency"
  ) +
  theme_minimal()

print(p3)

ggsave("Traffic_Output/03_Speed_Distribution.png",
       p3, width = 8, height = 5)

# STEP 12: GRAPH 4 - CONGESTION LEVEL

p4 <- ggplot(traffic,
             aes(Traffic_Level, fill = Traffic_Level)) +
  geom_bar() +
  labs(
    title = "Traffic Congestion Analysis",
    x = "Traffic Level",
    y = "Number of Records"
  ) +
  theme_minimal()

print(p4)

ggsave("Traffic_Output/04_Congestion.png",
       p4, width = 8, height = 5)

# STEP 13: GRAPH 5 - VEHICLE TYPE

p5 <- ggplot(traffic,
             aes(Vehicle_Type, fill = Vehicle_Type)) +
  geom_bar() +
  labs(
    title = "Vehicle Type Distribution",
    x = "Vehicle Type",
    y = "Number of Records"
  ) +
  theme_minimal()

print(p5)

ggsave("Traffic_Output/05_Vehicle_Types.png",
       p5, width = 8, height = 5)

# STEP 14: GRAPH 6 - SPEED VS VEHICLE COUNT

p6 <- ggplot(traffic,
             aes(Vehicle_Count, Average_Speed,
                 color = Traffic_Level)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Vehicle Count vs Average Speed",
    x = "Vehicle Count",
    y = "Average Speed (km/h)"
  ) +
  theme_minimal()

print(p6)

ggsave("Traffic_Output/06_Count_vs_Speed.png",
       p6, width = 8, height = 5)

# STEP 15: GRAPH 7 - ROAD OCCUPANCY

p7 <- ggplot(traffic,
             aes(Record_ID, Road_Occupancy)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Road Occupancy Analysis",
    x = "Record ID",
    y = "Road Occupancy (%)"
  ) +
  theme_minimal()

print(p7)

ggsave("Traffic_Output/07_Road_Occupancy.png",
       p7, width = 8, height = 5)

# STEP 16: CORRELATION ANALYSIS

correlation <- cor(
  traffic[, c("Vehicle_Count",
              "Average_Speed",
              "Road_Occupancy",
              "Congestion_Score")]
)

cat("\nCORRELATION MATRIX\n")
print(round(correlation, 2))

# STEP 17: LINEAR REGRESSION

model <- lm(
  Vehicle_Count ~ Hour + Average_Speed +
    Road_Occupancy,
  data = traffic
)

cat("\nREGRESSION MODEL RESULTS\n")
print(summary(model))

# STEP 18: TRAFFIC VOLUME PREDICTION

future <- data.frame(
  Hour = c(8, 9, 12, 15, 17, 18, 20),
  Average_Speed = c(25, 22, 40, 38, 20, 18, 35),
  Road_Occupancy = c(75, 82, 45, 50, 88, 92, 55)
)

future$Predicted_Vehicles <- round(
  predict(model, newdata = future), 1
)

cat("\nTRAFFIC VOLUME PREDICTION\n")
print(future)

# STEP 19: SAVE DATASETS

write.csv(
  traffic,
  "Traffic_Output/traffic_dataset_100.csv",
  row.names = FALSE
)

write.csv(
  hourly,
  "Traffic_Output/hourly_analysis.csv",
  row.names = FALSE
)

write.csv(
  vehicle_summary,
  "Traffic_Output/vehicle_summary.csv",
  row.names = FALSE
)

write.csv(
  future,
  "Traffic_Output/traffic_prediction.csv",
  row.names = FALSE
)

write.csv(
  correlation,
  "Traffic_Output/correlation_matrix.csv"
)

# STEP 20: FINAL RESULT

cat("\n==================================\n")
cat("TRAFFIC FLOW ANALYSIS COMPLETED!\n")
cat("Total Records: 100\n")
cat("Graphs Generated Successfully\n")
cat("Traffic Statistics Completed\n")
cat("Prediction Completed\n")
cat("==================================\n")

