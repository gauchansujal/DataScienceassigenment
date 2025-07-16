library(tidyverse)

# Step 1: Define correct column names for house price data
col_names <- c("ID", "Price", "Date", "Postcode", "PropertyType", "Old/New",
               "Duration", "PAON", "SAON", "Street", "Locality", "Town",
               "District", "County", "PPDCategoryType", "RecordStatus")

# Step 2: Load all house price files (with correct column names and skipping header row)
HousePrices2020 <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/pp-2020.csv", 
                            col_names = col_names, skip = 1, show_col_types = FALSE)
HousePrices2021 <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/pp-2021.csv", 
                            col_names = col_names, skip = 1, show_col_types = FALSE)
HousePrices2022 <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/pp-2022.csv", 
                            col_names = col_names, skip = 1, show_col_types = FALSE)
HousePrices2023 <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/pp-2023.csv", 
                            col_names = col_names, skip = 1, show_col_types = FALSE)
HousePrices2024 <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/pp-2024.csv", 
                            col_names = col_names, skip = 1, show_col_types = FALSE)

# Step 3: Combine all years
HousePrices <- bind_rows(HousePrices2020, HousePrices2021, HousePrices2022, HousePrices2023, HousePrices2024)

# Step 4: Load and process population data
PopulationData <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/Population2011_1656567141570 (4).csv",
                           show_col_types = FALSE)

PopulationData <- PopulationData %>%
  mutate(shortPostcode = str_trim(str_sub(Postcode, 1, 4))) %>%
  group_by(shortPostcode) %>%
  summarise(Population2011 = sum(Population)) %>%
  mutate(Population2012 = 1.00695353132322269 * Population2011,
         Population2013 = 1.00669740535540783 * Population2012,
         Population2014 = 1.00736463978721671 * Population2013,
         Population2015 = 1.00792367505802859 * Population2014,
         Population2016 = 1.00757874492811929 * Population2015,
         Population2017 = 1.00679374473924223 * Population2016,
         Population2018 = 1.00605929132212552 * Population2017,
         Population2019 = 1.00561255390388033 * Population2018,
         Population2020 = 1.00561255390388033 * Population2019,
         Population2021 = 1.005425 * Population2020,
         Population2022 = 1.004920 * Population2021,
         Population2023 = 1.004510 * Population2022,
         Population2024 = 1.004220 * Population2023) %>%
  select(shortPostcode, Population2020, Population2021, Population2022, Population2023, Population2024)

# Step 5: Clean and merge with house price data
Towns <- HousePrices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(shortPostcode = str_trim(str_sub(Postcode, 1, 4))) %>%
  left_join(PopulationData, by = "shortPostcode") %>%
  select(shortPostcode, Town, District, County,
         Population2020, Population2021, Population2022, Population2023, Population2024) %>%
  group_by(shortPostcode) %>%
  filter(row_number() == 1) %>%
  arrange(County)

# Step 6: Save to CSV
if (!dir.exists("Cleaned Data")) {
  dir.create("Cleaned Data")
}
write_csv(Towns, "Cleaned Data/Towns.csv")
view(Towns)
