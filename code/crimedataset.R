
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(fmsb)      # for radar chart
library(scales)    # for formatting pie chart percentages
install.packages("fmsb")  # Only run once if not already installed

# Read all four files
south_street <- read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\2022-05-south-yorkshire-street.csv")
south_outcomes <- read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\2022-05-south-yorkshire-outcomes.csv")
west_street <- read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\2022-05-west-yorkshire-street.csv")
west_outcomes <- read_csv("C:\\Users\\Sujal\\OneDrive\\Documents\\R lab\\data science project\\downlode the datasets\\2022-05-west-yorkshire-outcomes.csv")

# Add County column
south_street   <- south_street   %>% mutate(County = "SOUTH YORKSHIRE")
south_outcomes <- south_outcomes %>% mutate(County = "SOUTH YORKSHIRE")
west_street    <- west_street    %>% mutate(County = "WEST YORKSHIRE")
west_outcomes  <- west_outcomes  %>% mutate(County = "WEST YORKSHIRE")

# Merge street and outcome data
south_combined <- left_join(south_street, south_outcomes, by = "Crime ID")
west_combined  <- left_join(west_street,  west_outcomes,  by = "Crime ID")

# Combine both counties
crime_data <- bind_rows(south_combined, west_combined)

# Clean and transform the dataset
clean_crime_data <- crime_data %>%
  select(
    `Crime ID`,
    Month = Month.x,
    district = Location.x,        # 👈 Rename Location.x to 'district', but keep original values
    `Crime type`, 
    Longitude = Longitude.x,
    Latitude = Latitude.x,
    `Outcome type`,
    County = County.x
  ) %>%
  filter(!is.na(`Crime ID`)) %>%
  mutate(
    shortPostcode = str_trim(str_extract(district, "[A-Z]{1,2}[0-9R][0-9A-Z]?")),
    Year = substr(Month, 1, 4)
  )


# Save to CSV
dir.create("Cleaned Data", showWarnings = FALSE)
write_csv(clean_crime_data, "Cleaned Data/cleanCrimeData.csv")

# View the final cleaned dataset
View(clean_crime_data)
# Calculate total crimes and drug crimes per district and county
crime_summary <- clean_crime_data %>%
  group_by(County, district) %>%
  summarise(
    total_crimes = n(),
    drug_crimes = sum(`Crime type` == "Drugs"),
    drug_offense_rate = drug_crimes / total_crimes
  ) %>%
  ungroup()
#south
crime_summary %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = district, y = drug_offense_rate)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Drug Offense Rate by District – South Yorkshire",
    x = "District",
    y = "Drug Offense Rate"
  ) +
  theme_minimal() +
  coord_flip()
#west 
crime_summary %>%
  filter(County == "WEST YORKSHIRE") %>%
  ggplot(aes(x = district, y = drug_offense_rate)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Drug Offense Rate by District – West Yorkshire",
    x = "District",
    y = "Drug Offense Rate"
  ) +
  theme_minimal() +
  coord_flip()



# Step 1: Filter and summarize
radar_data <- clean_crime_data %>%
  filter(County == "WEST YORKSHIRE", 
         Year == "2022", 
         Month == "2022-05", 
         `Crime type` == "Vehicle crime") %>%
  count(district)

# Step 2: Reshape to correct radar format (columns = districts)
radar_ready <- data.frame(t(radar_data$n))
colnames(radar_ready) <- radar_data$district

# Step 3: Add max and min rows for radar scaling
radar_ready <- rbind(
  max = rep(max(radar_data$n), ncol(radar_ready)),
  min = rep(0, ncol(radar_ready)),
  radar_ready
)


radar_data <- clean_crime_data %>%
  filter(
    County == "WEST YORKSHIRE",
    Year == "2022",
    Month == "2022-05",
    `Crime type` == "Vehicle crime"
  ) %>%
  count(district, name = "VehicleCrime")

# Check: Did it return anything?
if(nrow(radar_data) == 0) {
  stop("No matching records found for selected county/month/crime type.")
}

# Step 2: Optional — limit to top 6 districts for readability
radar_data <- radar_data %>%
  arrange(desc(VehicleCrime)) %>%
  slice(1:6)

# Step 3: Build radar data frame
values <- radar_data$VehicleCrime
names(values) <- radar_data$district

radar_ready <- rbind(
  max = rep(max(values) + 10, length(values)),  # Max scale line
  min = rep(0, length(values)),                 # Min scale line
  values                                         # Actual values
)

# Ensure it's a data frame
radar_ready <- as.data.frame(radar_ready)

# Step 4: Plot
radarchart(radar_ready,
           axistype = 1,
           pcol = "darkblue", pfcol = rgb(0.2, 0.5, 0.9, 0.3), plwd = 2,
           cglcol = "grey", cglty = 1, cglwd = 0.8,
           axislabcol = "black", vlcex = 0.8,
           title = "Vehicle Crime Rate by District – West Yorkshire (May 2022)"
)


# Step 1: Filter data for South Yorkshire, May 2022, Robbery
robbery_data <- clean_crime_data %>%
  filter(
    County == "SOUTH YORKSHIRE",
    Year == "2022",
    Month == "2022-05",
    `Crime type` == "Robbery"
  ) %>%
  count(district, name = "RobberyCount")

# Step 2: Calculate percentages
robbery_data <- robbery_data %>%
  mutate(
    percent = RobberyCount / sum(RobberyCount),
    label = paste0(district, " (", percent(percent), ")")
  )

# Step 3: Plot pie chart
ggplot(robbery_data, aes(x = "", y = RobberyCount, fill = district)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Robbery Rate by District – South Yorkshire (May 2022)", fill = "District") +
  geom_text(aes(label = percent(percent)), 
            position = position_stack(vjust = 0.5), size = 4)

