# Load necessary library
library(tidyverse)

# Step 1: Unzip the broadband dataset
unzip("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/201809_fixed_pc_r03.zip", 
      exdir = "C:/Users/Sujal/OneDrive/Documents/R lab/data science project/broadband_speed")

# Step 2: Read the extracted CSV file
# Check actual filename inside the ZIP; here we assume it's named exactly like the ZIP
broadband_speed = read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/201809_fixed_pc_r03.zip")

# Step 3: Clean the data
clean_broadband_data = broadband_speed %>%
  mutate(
    shortPostcode = str_trim(str_sub(postcode, 1, 4))
  ) %>%
  select(
    postcode,
    shortPostcode,
    `Average download speed (Mbit/s)`,
    `Average upload speed (Mbit/s)`,
    `Average data usage (GB)`
  )


# Step 4: Save the cleaned data
write_csv(clean_broadband_data, "C:/Users/Sujal/OneDrive/Documents/R lab/data science project/broadband_speed/clean_broadband.csv")

view(clean_broadband_data)

colnames(broadband_speed)

# Read cleaned dataset that includes 'district' or 'County'
broadband = read_csv(

# Create boxplot for South Yorkshire
broadband %>%
  filter(district == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = shortPostcode, y = `Average download speed (Mbit/s)`)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Average Download Speed in South Yorkshire",
       x = "District (short postcode)",
       y = "Speed (Mbps)") +
  theme_minimal()


