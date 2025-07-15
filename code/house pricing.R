
library(tidyverse)
library(ggplot2)
HousePrices2020 = read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\pp-2020.csv", col_names = FALSE)
HousePrices2021 = read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\pp-2021.csv", col_names = FALSE)
HousePrices2022 = read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\pp-2022.csv", col_names = FALSE)
HousePrices2023 = read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\pp-2023.csv", col_names = FALSE)
HousePrices2024 = read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\pp-2024.csv", col_names = FALSE)
colnames(HousePrices2020) <- c(
  "transaction_id", "price", "date_of_transfer", "postcode", 
  "property_type", "old_new", "duration", "paon", "saon", "street",
  "locality", "town_city", "district", "county", "ppd_category", "record_status"
)

# Assign the same column names to the rest
colnames(HousePrices2021) <- colnames(HousePrices2020)
colnames(HousePrices2022) <- colnames(HousePrices2020)
colnames(HousePrices2023) <- colnames(HousePrices2020)
colnames(HousePrices2024) <- colnames(HousePrices2020)
HousePrices <- bind_rows(
  HousePrices2020,
  HousePrices2021,
  HousePrices2022,
  HousePrices2023,
  HousePrices2024
)
colnames(HousePrices2020)
cleanHousePrices <- HousePrices %>%
  filter(county == "SOUTH YORKSHIRE" | county == "WEST YORKSHIRE") %>%
  mutate(
    shortPostcode = str_trim(substr(postcode, 1, 4)),
    Year = substr(date_of_transfer, 1, 4)
  ) %>%
  arrange(county) %>%
  select(postcode, shortPostcode, price, Year, property_type,district, county)
dir.create("Cleaned Data", showWarnings = FALSE)
write.csv(cleanHousePrices, "Cleaned Data/cleanHousePrices.csv", row.names = FALSE)
View(cleanHousePrices)
colnames(HousePrices)


cleanHousePrices %>%
  filter(Year %in% c("2021", "2022", "2023", "2024")) %>%
  group_by(Year, district, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = Year, y = avg_price, color = county, group = county)) +
  geom_line(size = 1) +
  facet_wrap(~district, scales = "free_y") +
  labs(title = "Average House Prices (2021–2024) by District and County",
       x = "Year", y = "Average Price (£)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cleanHousePrices %>%
  filter(Year == "2023") %>%
  group_by(district, county) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = reorder(district, avg_price), y = avg_price, fill = county)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Average House Prices by District in 2023",
       x = "District", y = "Average Price (£)") +
  theme_minimal()


#Boxplots – Average Prices by District (Separate for Each County)

#South Yorkshire
cleanHousePrices %>%
  filter(county == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = district, y = price)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Price Distribution by District – South Yorkshire",
       x = "District", y = "Price (£)") +
  theme_minimal() +
  coord_flip()

# West Yorkshire
cleanHousePrices %>%
  filter(county == "WEST YORKSHIRE") %>%
  ggplot(aes(x = district, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Price Distribution by District – West Yorkshire",
       x = "District", y = "Price (£)") +
  theme_minimal() +
  coord_flip() 
#test relationships between price and variables like year, property type, and district.
#Correlation: Price vs Year (numeric)
cleanHousePrices %>%
  mutate(Year = as.numeric(Year)) %>%
  summarise(correlation = cor(price, Year, use = "complete.obs"))
#ANOVA: Is Price significantly different across property_type?
anova_result <- aov(price ~ property_type, data = cleanHousePrices)
summary(anova_result)

#Multiple Linear Regression
lm_model <- lm(price ~ as.factor(Year) + district + property_type, data = cleanHousePrices)
summary(lm_model)

#Recommend Affordable Districts
recommend_districts("2023", "T", 200000)
{
  cleanHousePrices %>%
    filter(Year == Year, property_type == type) %>%
    group_by(district) %>%
    summarise(avg_price = mean(price, na.rm = TRUE)) %>%
    filter(avg_price <= budget) %>%
    arrange(avg_price)
}




