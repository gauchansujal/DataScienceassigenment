library(tidyverse)
library(janitor)
library(ggplot2)

library(broom)  # for tidy model summaries



# Load and clean datasets
school_data <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/cleanSchoolPerformance.csv") %>%
  clean_names()

crime_data <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/cleanCrimeData.csv") %>%
  clean_names()



# Prepare school data for 2021
school_2021 <- school_data %>%
  filter(year == 2021) %>%
  mutate(
    att8_score = na_if(att8scr_nfsm6cla1a, "SUPP"),
    att8_score = na_if(att8_score, "NE"),
    att8_score = as.numeric(att8_score),
    county = case_when(
      lea == 370 ~ "South Yorkshire",
      lea == 383 ~ "West Yorkshire",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(att8_score), !is.na(county)) %>%
  group_by(county) %>%
  summarise(attainment_score = mean(att8_score, na.rm = TRUE))

# Prepare drug offense count for 2022 (since 2023 missing)
drug_crimes_2022 <- crime_data %>%
  filter(year == 2022, crime_type == "Drugs") %>%
  mutate(county = str_to_title(county)) %>%  # Fix case to match school_data counties
  group_by(county) %>%
  summarise(drug_rate = n())

# Check counties in both datasets before join
print(unique(school_2021$county))
print(unique(drug_crimes_2022$county))

# Fix county names to match exactly (both title case)
school_2021 <- school_2021 %>%
  mutate(county = str_to_title(county))

# Merge on county
combined <- inner_join(school_2021, drug_crimes_2022, by = "county")


# 1. Plot with points and linear regression line
ggplot(combined, aes(x = attainment_score, y = drug_rate, color = county)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
  labs(
    title = "Attainment 8 Score (2021) vs Drug Offense Count (2022)",
    x = "Average Attainment 8 Score",
    y = "Drug Offense Count"
  ) +
  theme_minimal()


# Calculate correlation
cor_val <- cor(combined$attainment_score, combined$drug_rate, use = "complete.obs")

# Fit linear model
model <- lm(drug_rate ~ attainment_score, data = combined)

# Get tidy model summary (coefficients, p-values, etc.)
model_summary <- tidy(model)

# Create a combined summary table
result_table <- model_summary %>%
  mutate(correlation = cor_val) %>% 
  select(term, estimate, std.error, statistic, p.value, correlation)

print(result_table)




