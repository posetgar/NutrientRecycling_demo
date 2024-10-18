# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset (replace 'NutrientRecycling_Dataset.xlsx' with your file path)
data <- read.csv("NutrientRecycling_Dataset.csv")  # For CSV files
# For Excel: data <- readxl::read_excel("NutrientRecycling_Dataset.xlsx")

# Inspect the first few rows of the dataset
head(data)

# Calculate percentage reduction in heavy metals (Cd and Zn) after treatment
data <- data %>%
  mutate(
    Cd_Reduction = ((Cd_Before_mg_kg - Cd_After_mg_kg) / Cd_Before_mg_kg) * 100,
    Zn_Reduction = ((Zn_Before_mg_kg - Zn_After_mg_kg) / Zn_Before_mg_kg) * 100
  )

# Calculate nutrient retention after treatment
data <- data %>%
  mutate(
    P_Retention = (P_After_% / P_Before_%) * 100,
    N_Retention = (N_After_% / N_Before_%) * 100,
    K_Retention = (K_After_% / K_Before_%) * 100
  )

# Summary statistics for heavy metal reduction and nutrient retention by treatment
summary_data <- data %>%
  group_by(Treatment) %>%
  summarise(
    Avg_Cd_Reduction = mean(Cd_Reduction, na.rm = TRUE),
    Avg_Zn_Reduction = mean(Zn_Reduction, na.rm = TRUE),
    Avg_P_Retention = mean(P_Retention, na.rm = TRUE),
    Avg_N_Retention = mean(N_Retention, na.rm = TRUE),
    Avg_K_Retention = mean(K_Retention, na.rm = TRUE)
  )

# Print the summary table
print(summary_data)

# Plot heavy metal reduction by treatment
ggplot(data, aes(x = Treatment, y = Cd_Reduction, fill = Treatment)) +
  geom_boxplot() +
  ggtitle("Reduction in Cadmium (Cd) by Treatment") +
  ylab("Cd Reduction (%)") +
  theme_minimal()

ggplot(data, aes(x = Treatment, y = Zn_Reduction, fill = Treatment)) +
  geom_boxplot() +
  ggtitle("Reduction in Zinc (Zn) by Treatment") +
  ylab("Zn Reduction (%)") +
  theme_minimal()

# Plot nutrient retention by treatment
ggplot(data, aes(x = Treatment, y = P_Retention, fill = Treatment)) +
  geom_boxplot() +
  ggtitle("Phosphorus Retention by Treatment") +
  ylab("P Retention (%)") +
  theme_minimal()

ggplot(data, aes(x = Treatment, y = N_Retention, fill = Treatment)) +
  geom_boxplot() +
  ggtitle("Nitrogen Retention by Treatment") +
  ylab("N Retention (%)") +
  theme_minimal()

ggplot(data, aes(x = Treatment, y = K_Retention, fill = Treatment)) +
  geom_boxplot() +
  ggtitle("Potassium Retention by Treatment") +
  ylab("K Retention (%)") +
  theme_minimal()

# Find the best treatment for heavy metal reduction and nutrient retention
best_treatment <- summary_data %>%
  filter(Avg_Cd_Reduction > 50 & Avg_Zn_Reduction > 50 & Avg_P_Retention > 70)

print("Best treatment for heavy metal reduction and nutrient retention:")
print(best_treatment)
