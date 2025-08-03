library(tidyverse)
library(stringr)

# Load datasets
lookup_df <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/LEA_lookup.csv")  # <-- Add this file

population_df <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/Towns.csv")
postcode_df <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/postcode_lsoa_clean.csv")
crime_df <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/cleanCrimeData.csv")
att8_df <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/cleanSchoolPerformance.csv")
# Clean postcodes (remove spaces and uppercase)
population_df <- population_df %>%
  mutate(shortPostcode = str_replace_all(shortPostcode, "\\s+", "") %>% str_to_upper())

postcode_df <- postcode_df %>%
  mutate(shortPostcode = str_replace_all(shortPostcode, "\\s+", "") %>% str_to_upper(),
         ladnm = str_to_title(ladnm))  # Optional: tidy district names

# Join population with postcode-to-district mapping
district_population <- population_df %>%
  left_join(postcode_df, by = "shortPostcode") %>%
  drop_na(ladnm)

# Summarize population for 2023 by district (ladnm)
district_population_summary <- district_population %>%
  group_by(ladnm) %>%
  summarise(Total_Population = sum(Population2023, na.rm = TRUE)) %>%
  ungroup()

# Filter drug crimes for 2023 (correct column names)
drug_crimes_2023 <- crime_df %>%
  filter(str_detect(tolower(`Crime type`), "drug"),
         Year == 2022,
         !is.na(district))

# Count drug offenses by district
drug_summary <- drug_crimes_2023 %>%
  group_by(district) %>%
  summarise(Drug_Offense_Count = n()) %>%
  ungroup()

# Join drug data with population summary by matching district names
# Make district names consistent case for joining
drug_summary <- drug_summary %>%
  mutate(district = str_to_title(district))

district_population_summary <- district_population_summary %>%
  mutate(ladnm = str_to_title(ladnm))

drug_rate_df <- drug_summary %>%
  left_join(district_population_summary, by = c("district" = "ladnm")) %>%
  mutate(Drug_Offense_Rate = (Drug_Offense_Count / Total_Population) * 10000)

att8_df <- att8_df %>%
  rename(
    district = LEA,
    Attainment8_Score = ATT8SCR
  ) %>%
  mutate(
    district = str_to_title(district),
    Attainment8_Score = as.numeric(Attainment8_Score)  # 👈 convert to numeric
  ) %>%
  drop_na(Attainment8_Score) %>%
  select(district, Attainment8_Score)

drug_rate_df %>% distinct(district) %>% head(10)
att8_df %>% distinct(district) %>% head(10)

# Join with real Attainment 8 scores
final_df <- drug_rate_df %>%
  left_join(att8_df, by = "district") %>%
  drop_na()

# Calculate correlation
correlation <- cor(final_df$Drug_Offense_Rate, final_df$Attainment8_Score)
cat("📌 Pearson Correlation:", round(correlation, 3), "\n\n")

# Linear regression model
model <- lm(Attainment8_Score ~ Drug_Offense_Rate, data = final_df)
print(summary(model))

# Plot the relationship
ggplot(final_df, aes(x = Drug_Offense_Rate, y = Attainment8_Score)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Attainment 8 Score vs Drug Offense Rate per 10,000 People (2023)",
       subtitle = paste("Correlation:", round(correlation, 3)),
       x = "Drug Offense Rate per 10,000",
       y = "Attainment 8 Score") +
  theme_minimal()

names(att8_df)
grep("attain|att8|score", names(att8_df), ignore.case = TRUE, value = TRUE)
str(final_df$Attainment8_Score)

