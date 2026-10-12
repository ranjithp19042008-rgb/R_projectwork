# ============================================================
# ELECTRIC VEHICLE SALES ANALYSIS USING R
# Four Separate Graph Windows + Analysis + Prediction
# ============================================================

# Install required packages only if they are not installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# ---------------- 1. CREATE SAMPLE DATASET ------------------
# Sample figures are illustrative, not official market data.
ev_data <- data.frame(
  Year = rep(2020:2025, each = 4),
  Vehicle_Type = rep(c("Electric Car", "Electric Two-Wheeler",
                       "Electric Three-Wheeler", "Electric Bus"), 6),
  Units_Sold = c(
    12000, 25000, 8000, 500,
    18000, 32000, 11000, 700,
    28000, 40000, 15000, 950,
    42000, 52000, 21000, 1300,
    60000, 68000, 28000, 1800,
    82000, 85000, 36000, 2400
  ),
  Avg_Price_Lakh = c(
    12.5, 0.9, 3.2, 95,
    12.8, 0.95, 3.3, 96,
    13.2, 1.0, 3.4, 98,
    13.5, 1.05, 3.6, 100,
    14.0, 1.1, 3.8, 103,
    14.5, 1.15, 4.0, 106
  )
)

# ---------------- 2. CALCULATE ESTIMATED REVENUE -------------
ev_data$Estimated_Revenue_Crore <-
  ev_data$Units_Sold * ev_data$Avg_Price_Lakh / 100

# ---------------- 3. YEAR-WISE SALES ANALYSIS ----------------
yearly_sales <- aggregate(Units_Sold ~ Year, data = ev_data, sum)
names(yearly_sales)[2] <- "Total_Sales"

yearly_revenue <- aggregate(
  Estimated_Revenue_Crore ~ Year, data = ev_data, sum
)
yearly_sales <- merge(yearly_sales, yearly_revenue, by = "Year")
yearly_sales <- yearly_sales[order(yearly_sales$Year), ]

yearly_sales$Growth_Percent <- c(
  NA,
  round(
    diff(yearly_sales$Total_Sales) /
      head(yearly_sales$Total_Sales, -1) * 100,
    2
  )
)

# ---------------- 4. VEHICLE CATEGORY ANALYSIS ---------------
type_sales <- aggregate(Units_Sold ~ Vehicle_Type,
                        data = ev_data, sum)
type_sales <- type_sales[order(-type_sales$Units_Sold), ]

# ---------------- 5. PRINT REPORT IN CONSOLE -----------------
cat("\n============================================\n")
cat(" ELECTRIC VEHICLE SALES ANALYSIS REPORT\n")
cat("============================================\n")

cat("\n1. SAMPLE DATASET\n")
print(ev_data)

cat("\n2. YEAR-WISE SALES, REVENUE AND GROWTH\n")
print(yearly_sales)

cat("\n3. TOTAL SALES BY VEHICLE CATEGORY\n")
print(type_sales)

cat("\n4. KEY STATISTICS\n")
cat("Total vehicles sold:", sum(ev_data$Units_Sold), "\n")
cat("Average sales per year:",
    round(mean(yearly_sales$Total_Sales), 2), "\n")
cat("Total estimated revenue (crore):",
    round(sum(ev_data$Estimated_Revenue_Crore), 2), "\n")
cat("Highest-selling category:", type_sales$Vehicle_Type[1], "\n")
cat("Highest sales year:",
    yearly_sales$Year[which.max(yearly_sales$Total_Sales)], "\n")

# ---------------- 6. LINEAR REGRESSION MODEL -----------------
sales_model <- lm(Total_Sales ~ Year, data = yearly_sales)

cat("\n5. LINEAR REGRESSION MODEL SUMMARY\n")
print(summary(sales_model))

# ---------------- 7. PREDICT SALES FOR 2026-2030 -------------
future_years <- data.frame(Year = 2026:2030)
future_years$Predicted_Sales <- round(
  pmax(0, as.numeric(predict(sales_model, newdata = future_years)))
)

cat("\n6. FUTURE EV SALES PREDICTION (ILLUSTRATIVE)\n")
print(future_years)

# ---------------- 8. CREATE FOUR GRAPHS ----------------------
p1 <- ggplot(yearly_sales, aes(x = Year, y = Total_Sales)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2020:2025) +
  labs(
    title = "Total Electric Vehicle Sales by Year",
    subtitle = "Illustrative sample data",
    x = "Year", y = "Total vehicles sold"
  ) +
  theme_minimal(base_size = 13)

p2 <- ggplot(type_sales,
             aes(x = reorder(Vehicle_Type, Units_Sold),
                 y = Units_Sold)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sales by Vehicle Category",
    x = "Vehicle type", y = "Total vehicles sold"
  ) +
  theme_minimal(base_size = 13)

p3 <- ggplot(yearly_sales, aes(x = Year, y = Total_Sales)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_line(
    data = future_years,
    aes(x = Year, y = Predicted_Sales),
    linetype = "dashed", linewidth = 1
  ) +
  geom_point(
    data = future_years,
    aes(x = Year, y = Predicted_Sales),
    shape = 17, size = 3
  ) +
  scale_x_continuous(breaks = 2020:2030) +
  labs(
    title = "Actual and Predicted EV Sales",
    subtitle = "Dashed line and triangles show predictions",
    x = "Year", y = "Vehicles sold"
  ) +
  theme_minimal(base_size = 13)

p4 <- ggplot(
  ev_data,
  aes(x = Year, y = Units_Sold, group = Vehicle_Type,
      linetype = Vehicle_Type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2020:2025) +
  labs(
    title = "Sales Trend by Vehicle Type",
    x = "Year", y = "Vehicles sold",
    linetype = "Vehicle type"
  ) +
  theme_minimal(base_size = 12)

# ---------------- 9. OPEN FOUR SEPARATE WINDOWS --------------
# Windows operating system:
if (.Platform$OS.type == "windows") {
  windows(width = 9, height = 6)
  print(p1)

  windows(width = 9, height = 6)
  print(p2)

  windows(width = 9, height = 6)
  print(p3)

  windows(width = 9, height = 6)
  print(p4)
} else {
  # Alternative for macOS/Linux R graphical sessions
  dev.new(width = 9, height = 6)
  print(p1)

  dev.new(width = 9, height = 6)
  print(p2)

  dev.new(width = 9, height = 6)
  print(p3)

  dev.new(width = 9, height = 6)
  print(p4)
}

# ---------------- 10. SAVE RESULTS TO CSV --------------------
write.csv(ev_data, "EV_Detailed_Sales_Data.csv", row.names = FALSE)
write.csv(yearly_sales, "EV_Yearly_Analysis.csv", row.names = FALSE)
write.csv(type_sales, "EV_Category_Analysis.csv", row.names = FALSE)
write.csv(future_years, "EV_Sales_Prediction_2026_2030.csv",
          row.names = FALSE)

capture.output(
  summary(sales_model),
  file = "EV_Regression_Model_Summary.txt"
)

cat("\nOutput files saved in folder:\n")
cat(getwd(), "\n")
cat("\nPROJECT COMPLETED SUCCESSFULLY!\n")
cat("Note: Sample data is illustrative, not official EV sales data.\n")
