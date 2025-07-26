library(tidyverse)
library(ggplot2)
library(broom)
library(purrr)

house_data <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/cleanHousePrices.csv")

attainment_data <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/cleanSchoolPerformance.csv")  


# Step 2: Clean shortPostcode
house_data <- house_data %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

attainment_data <- attainment_data %>%
  mutate(shortPostcode = str_trim(toupper(PCODE)))

# 3. Join both datasets
merged_data <- inner_join(house_data, attainment_data, by = "shortPostcode")

merged_data <- merged_data %>%
  mutate(
    price = as.numeric(price),
    ATT8SCR = as.numeric(ATT8SCR)  # ← this line ensures ATT8SCR is numeric too
  ) %>%
  select(ATT8SCR, price, county) %>%
  drop_na()



# Plot with linear models for each county
ggplot(merged_data, aes(x = ATT8SCR, y = price, color = county)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Attainment 8 Score vs House Price by County",
    x = "Attainment 8 Score",
    y = "House Price"
  ) +
  theme_minimal()

# Linear regression model with county as a factor
model <- lm(price ~ ATT8SCR + county, data = merged_data)
summary(model)
model_coef <- tidy(model)

# Step 3: Calculate correlation per county
county_cor <- merged_data %>%
  group_by(county) %>%
  summarise(correlation = cor(ATT8SCR, price, use = "complete.obs"))



# Step 4: Extract county coefficients (excluding intercept and ATT8SCR)
county_effects <- model_coef %>%
  filter(str_detect(term, "^county")) %>%
  mutate(
    county = str_remove(term, "^county"),
    county = str_trim(county)
  ) %>%
  select(county, estimate) %>%
  rename(county_effect = estimate)

# Step 5: Add base (reference) county with effect 0
all_counties <- unique(merged_data$county)
missing_counties <- setdiff(all_counties, county_effects$county)
base_effect <- tibble(county = missing_counties, county_effect = 0)

county_effects <- bind_rows(county_effects, base_effect)

# Step 6: Combine county-wise correlation with regression county effect
combined_table <- county_cor %>%
  left_join(county_effects, by = "county")


print(combined_table)