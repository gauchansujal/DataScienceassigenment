library(tidyverse)

# Load only the 2024 dataset
postcode_lsoa <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/Postcode to LSOA 1.csv")
postcode_lsoa_clean <- postcode_lsoa %>%
  filter(ladnm %in% c("Barnsley", "Doncaster", "Rotherham", "Sheffield",
                      "Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield")) %>%
  mutate(
    shortPostcode = str_trim(str_sub(pcds, 1, 4)),  # Assuming 'pcds' is postcode column
    Year = NA_character_  # No Date column available in this dataset
  ) %>%
  arrange(ladnm) %>%
  select(pcds, shortPostcode, ladnm)

# Create output folder if it doesn't exist
if(!dir.exists("Cleaned Data")) {
  dir.create("Cleaned Data")
}

# Save cleaned data as CSV
write_csv(postcode_lsoa_clean, "Cleaned Data/postcode_lsoa_clean.csv")

# Optional: View the cleaned data
View(postcode_lsoa_clean)