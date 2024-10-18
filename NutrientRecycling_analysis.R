# Install necessary libraries (only runs if the package is not already installed)
if(!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
if(!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
if(!require(readxl)) install.packages("readxl", dependencies=TRUE)

# Load the libraries
library(dplyr)
library(ggplot2)
library(readxl)

# Load the dataset from the specified file path
data <- read_excel("C:\\Users\\posetgar\\Downloads\\NutrientRecycling_Dataset.xlsx")

# 1. Calculate percentage reduction in heavy metals (Cd and Zn)
# and percentage retention of nutrients (P, N, K)
# Note: Make sure the column names match the dataset exactly, including special characters and spaces
data <- data %>%
  mutate(
    Cd_Reduction = ((Cd_Before_mg_kg - Cd_After_mg_kg) / Cd_Before_mg_kg) * 100,
    Zn_Reduction = ((Zn_Before_mg_kg - Zn_After_mg_kg) / Zn_Before_mg_kg) * 100,
    P_Retention = (`P_After_%` / `P_Before_%`) * 100,
    N_Retention = (`N_After_%` / `N_Before_%`) * 100,
    K_Retention = (`K_After_%` / `K_Before_%`) * 100
  )

# 2. Summarize the data by treatment
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

# 3. Plotting: Boxplot of heavy metal reduction by treatment
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

# 4. Plotting: Boxplot of nutrient retention by treatment
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

# 5. Identify the best treatment based on criteria for Cd, Zn reduction, and nutrient retention
best_treatment <- summary_data %>%
  filter(Avg_Cd_Reduction > 50 & Avg_Zn_Reduction > 50 & Avg_P_Retention > 70)

print("Best treatment for heavy metal reduction and nutrient retention:")
print(best_treatment)
