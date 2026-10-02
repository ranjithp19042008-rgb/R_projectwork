
# =====================================================
# PROJECT: HEART DISEASE RISK PREDICTION USING R
# SAMPLE SIZE: 300 PATIENTS
# =====================================================

# 1. Load Packages
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Create Output Folder
output_folder <- "Heart_Disease_Output"

if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# 3. Generate 300 Sample Patient Records
set.seed(123)

n <- 300

patient_data <- data.frame(
  Patient_ID = 1:n,
  
  Age = sample(25:80, n, replace = TRUE),
  
  Sex = sample(
    c("Male", "Female"),
    n, replace = TRUE,
    prob = c(0.55, 0.45)
  ),
  
  Chest_Pain = sample(
    c("Typical", "Atypical", "Non-Anginal", "Asymptomatic"),
    n, replace = TRUE
  ),
  
  Resting_BP = sample(
    90:180, n, replace = TRUE
  ),
  
  Cholesterol = sample(
    150:350, n, replace = TRUE
  ),
  
  Fasting_Blood_Sugar = sample(
    c("Normal", "High"),
    n, replace = TRUE,
    prob = c(0.75, 0.25)
  ),
  
  Max_Heart_Rate = sample(
    80:200, n, replace = TRUE
  ),
  
  Exercise_Angina = sample(
    c("Yes", "No"),
    n, replace = TRUE
  ),
  
  Smoking = sample(
    c("Yes", "No"),
    n, replace = TRUE
  ),
  
  Diabetes = sample(
    c("Yes", "No"),
    n, replace = TRUE
  )
)

# 4. Generate Demonstration Outcome
# This synthetic outcome is generated from
# an illustrative probability formula.
# It is NOT a clinical risk equation.

patient_data <- patient_data %>%
  mutate(
    Risk_Score =
      -4.5 +
      0.045 * (Age - 45) +
      0.018 * (Resting_BP - 120) +
      0.012 * (Cholesterol - 200) -
      0.015 * (Max_Heart_Rate - 140) +
      ifelse(Sex == "Male", 0.25, 0) +
      ifelse(Chest_Pain == "Asymptomatic", 0.8, 0) +
      ifelse(Chest_Pain == "Typical", 0.3, 0) +
      ifelse(Fasting_Blood_Sugar == "High", 0.45, 0) +
      ifelse(Exercise_Angina == "Yes", 0.8, 0) +
      ifelse(Smoking == "Yes", 0.45, 0) +
      ifelse(Diabetes == "Yes", 0.6, 0),
    
    Probability = 1 / (1 + exp(-Risk_Score)),
    
    Heart_Disease = rbinom(
      n, size = 1, prob = Probability
    ),
    
    Heart_Disease = factor(
      Heart_Disease,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  ) %>%
  select(-Risk_Score, -Probability)

# 5. Display Dataset
cat("HEART DISEASE PATIENT DATASET\n")
print(head(patient_data, 10))

cat("\nDATA SUMMARY\n")
print(summary(patient_data))

# 6. Check Missing Values
cat("\nMISSING VALUES\n")
print(colSums(is.na(patient_data)))

# 7. Data Cleaning
patient_data <- patient_data %>%
  drop_na()

# 8. Outcome Distribution
cat("\nHEART DISEASE CLASS DISTRIBUTION\n")
print(table(patient_data$Heart_Disease))

# 9. Statistical Analysis
patient_summary <- patient_data %>%
  summarise(
    Total_Patients = n(),
    Average_Age = round(mean(Age), 2),
    Average_BP = round(mean(Resting_BP), 2),
    Average_Cholesterol = round(mean(Cholesterol), 2),
    Average_Max_Heart_Rate =
      round(mean(Max_Heart_Rate), 2),
    Heart_Disease_Cases =
      sum(Heart_Disease == "Yes"),
    No_Disease_Cases =
      sum(Heart_Disease == "No")
  )

cat("\nPATIENT SUMMARY\n")
print(patient_summary)

# 10. Risk Group for Exploratory Display
# These are demonstration labels, not clinical categories.

patient_data <- patient_data %>%
  mutate(
    Demonstration_Group = ifelse(
      Heart_Disease == "Yes",
      "Positive Sample",
      "Negative Sample"
    )
  )

# =====================================================
# 11. DATA VISUALIZATION
# =====================================================

# Graph 1: Patient Age Distribution
p1 <- ggplot(
  patient_data,
  aes(Age)
) +
  geom_histogram(
    bins = 20,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Patient Age Distribution",
    x = "Age",
    y = "Number of Patients"
  ) +
  theme_minimal()

print(p1)

ggsave(
  file.path(output_folder, "Age_Distribution.png"),
  p1, width = 8, height = 5
)

# Graph 2: Heart Disease Class Distribution
p2 <- ggplot(
  patient_data,
  aes(Heart_Disease, fill = Heart_Disease)
) +
  geom_bar() +
  labs(
    title = "Heart Disease Class Distribution",
    x = "Synthetic Outcome",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

ggsave(
  file.path(output_folder, "Disease_Distribution.png"),
  p2, width = 8, height = 5
)

# Graph 3: Age vs Cholesterol
p3 <- ggplot(
  patient_data,
  aes(Age, Cholesterol, color = Heart_Disease)
) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Age vs Cholesterol",
    x = "Age",
    y = "Cholesterol",
    color = "Synthetic Outcome"
  ) +
  theme_minimal()

print(p3)

ggsave(
  file.path(output_folder, "Age_Cholesterol.png"),
  p3, width = 8, height = 5
)

# Graph 4: Blood Pressure Distribution
p4 <- ggplot(
  patient_data,
  aes(Resting_BP, fill = Heart_Disease)
) +
  geom_histogram(
    bins = 20,
    position = "identity",
    alpha = 0.6
  ) +
  labs(
    title = "Resting Blood Pressure Distribution",
    x = "Resting Blood Pressure",
    y = "Frequency"
  ) +
  theme_minimal()

print(p4)

ggsave(
  file.path(output_folder, "Blood_Pressure.png"),
  p4, width = 8, height = 5
)

# Graph 5: Cholesterol by Outcome
p5 <- ggplot(
  patient_data,
  aes(Heart_Disease, Cholesterol, fill = Heart_Disease)
) +
  geom_boxplot() +
  labs(
    title = "Cholesterol Comparison",
    x = "Synthetic Outcome",
    y = "Cholesterol"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)

ggsave(
  file.path(output_folder, "Cholesterol_Comparison.png"),
  p5, width = 8, height = 5
)

# Graph 6: Heart Disease by Smoking
p6 <- ggplot(
  patient_data,
  aes(Smoking, fill = Heart_Disease)
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Smoking vs Synthetic Outcome",
    x = "Smoking",
    y = "Number of Patients"
  ) +
  theme_minimal()

print(p6)

ggsave(
  file.path(output_folder, "Smoking_Analysis.png"),
  p6, width = 8, height = 5
)

# Graph 7: Maximum Heart Rate
p7 <- ggplot(
  patient_data,
  aes(Max_Heart_Rate, fill = Heart_Disease)
) +
  geom_histogram(
    bins = 20,
    position = "identity",
    alpha = 0.6
  ) +
  labs(
    title = "Maximum Heart Rate Distribution",
    x = "Maximum Heart Rate",
    y = "Frequency"
  ) +
  theme_minimal()

print(p7)

ggsave(
  file.path(output_folder, "Maximum_Heart_Rate.png"),
  p7, width = 8, height = 5
)

# =====================================================
# 12. TRAINING AND TESTING
# =====================================================

set.seed(123)

train_index <- sample(
  seq_len(nrow(patient_data)),
  size = floor(0.8 * nrow(patient_data))
)

train_data <- patient_data[train_index, ]
test_data <- patient_data[-train_index, ]

cat("\nTRAINING RECORDS:", nrow(train_data), "\n")
cat("TESTING RECORDS:", nrow(test_data), "\n")

# =====================================================
# 13. LOGISTIC REGRESSION MODEL
# =====================================================

model <- glm(
  Heart_Disease ~ Age + Sex + Chest_Pain +
    Resting_BP + Cholesterol +
    Fasting_Blood_Sugar +
    Max_Heart_Rate + Exercise_Angina +
    Smoking + Diabetes,
  data = train_data,
  family = binomial
)

cat("\nLOGISTIC REGRESSION MODEL\n")
print(summary(model))

# =====================================================
# 14. PREDICTION
# =====================================================

predicted_probability <- predict(
  model,
  newdata = test_data,
  type = "response"
)

predicted_class <- ifelse(
  predicted_probability >= 0.5,
  "Yes",
  "No"
)

predicted_class <- factor(
  predicted_class,
  levels = c("No", "Yes")
)

# Create Prediction Report
prediction_report <- data.frame(
  Patient_ID = test_data$Patient_ID,
  Actual = test_data$Heart_Disease,
  Predicted = predicted_class,
  Predicted_Probability =
    round(predicted_probability, 4)
)

cat("\nPREDICTION RESULTS\n")
print(head(prediction_report, 15))

# =====================================================
# 15. MODEL EVALUATION
# =====================================================

actual <- test_data$Heart_Disease

confusion_matrix <- table(
  Actual = actual,
  Predicted = predicted_class
)

cat("\nCONFUSION MATRIX\n")
print(confusion_matrix)

TP <- sum(actual == "Yes" & predicted_class == "Yes")
TN <- sum(actual == "No" & predicted_class == "No")
FP <- sum(actual == "No" & predicted_class == "Yes")
FN <- sum(actual == "Yes" & predicted_class == "No")

accuracy <- (TP + TN) / length(actual)

precision <- ifelse(
  TP + FP == 0,
  NA,
  TP / (TP + FP)
)

recall <- ifelse(
  TP + FN == 0,
  NA,
  TP / (TP + FN)
)

specificity <- ifelse(
  TN + FP == 0,
  NA,
  TN / (TN + FP)
)

f1_score <- ifelse(
  is.na(precision) || is.na(recall) ||
    precision + recall == 0,
  NA,
  2 * precision * recall / (precision + recall)
)

model_metrics <- data.frame(
  Metric = c(
    "Accuracy",
    "Precision",
    "Recall",
    "Specificity",
    "F1 Score"
  ),
  Value = round(
    c(accuracy, precision, recall, specificity, f1_score),
    3
  )
)

cat("\nMODEL PERFORMANCE\n")
print(model_metrics)

# =====================================================
# 16. NEW SAMPLE PATIENT PREDICTION
# =====================================================

new_patient <- data.frame(
  Age = 55,
  Sex = factor("Male", levels = levels(patient_data$Sex)),
  Chest_Pain = factor(
    "Atypical",
    levels = levels(patient_data$Chest_Pain)
  ),
  Resting_BP = 140,
  Cholesterol = 240,
  Fasting_Blood_Sugar = factor(
    "High",
    levels = levels(patient_data$Fasting_Blood_Sugar)
  ),
  Max_Heart_Rate = 130,
  Exercise_Angina = factor(
    "Yes",
    levels = levels(patient_data$Exercise_Angina)
  ),
  Smoking = factor(
    "Yes",
    levels = levels(patient_data$Smoking)
  ),
  Diabetes = factor(
    "No",
    levels = levels(patient_data$Diabetes)
  )
)

new_probability <- predict(
  model,
  newdata = new_patient,
  type = "response"
)

new_class <- ifelse(
  new_probability >= 0.5,
  "Positive Sample",
  "Negative Sample"
)

cat("\nDEMONSTRATION PATIENT MODEL OUTPUT\n")
cat(
  "Predicted Probability:",
  round(new_probability, 4),
  "\n"
)
cat("Model Classification:", new_class, "\n")

# This output is not a medical diagnosis or
# an individual's actual clinical risk.

# =====================================================
# 17. SAVE RESULTS
# =====================================================

write.csv(
  patient_data,
  file.path(output_folder, "Patient_Dataset_300.csv"),
  row.names = FALSE
)

write.csv(
  patient_summary,
  file.path(output_folder, "Patient_Summary.csv"),
  row.names = FALSE
)

write.csv(
  prediction_report,
  file.path(output_folder, "Prediction_Report.csv"),
  row.names = FALSE
)

write.csv(
  model_metrics,
  file.path(output_folder, "Model_Performance.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame.matrix(confusion_matrix),
  file.path(output_folder, "Confusion_Matrix.csv")
)

capture.output(
  summary(model),
  file = file.path(output_folder, "Logistic_Regression_Report.txt")
)

new_patient_report <- data.frame(
  Demonstration_Probability = new_probability,
  Model_Classification = new_class
)

write.csv(
  new_patient_report,
  file.path(output_folder, "Sample_Patient_Output.csv"),
  row.names = FALSE
)

# Save model object
saveRDS(
  model,
  file.path(output_folder, "Heart_Disease_Model.rds")
)

# =====================================================
# 18. FINAL RESULT
# =====================================================

cat("\n====================================\n")
cat("HEART DISEASE PREDICTION COMPLETED\n")
cat("====================================\n")

cat("Total Sample Patients:", nrow(patient_data), "\n")
cat("Training Patients:", nrow(train_data), "\n")
cat("Testing Patients:", nrow(test_data), "\n")
cat("Model Accuracy:", round(accuracy * 100, 2), "%\n")
cat("Output Folder:", output_folder, "\n")
cat("Dataset, graphs and reports saved.\n")
cat("====================================\n")
