library(tidyverse)
library(ggplot2)
library(broom)  # for model summary
library(purrr)

house_data <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/cleanHousePrices.csv")

speed_data <- read_csv("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/Cleaned Data/broadband_speed/clean_broadband.csv")


house_data <- house_data %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

speed_data <- speed_data %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode))) %>%
  rename(AvgDownloadSpeed = `Average download speed (Mbit/s)`)


speed_agg <- speed_data %>%
  group_by(shortPostcode) %>%
  summarise(
    avg_download_speed = mean(AvgDownloadSpeed, na.rm = TRUE)
  )

# Aggregate house data: average price per postcode, take first county
house_agg <- house_data %>%
  group_by(shortPostcode) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    county = first(county)
  )

# Merge aggregated data
merged_agg <- inner_join(house_agg, speed_agg, by = "shortPostcode")

merged_agg <- merged_agg %>%
  filter(county %in% c("WEST YORKSHIRE", "SOUTH YORKSHIRE"))


print(
  ggplot(merged_agg, aes(x = avg_download_speed, y = avg_price, color = county)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "House Price vs Download Speed by County",
      x = "Average Download Speed (Mbps)",
      y = "Average House Price (£)",
      color = "County"
    ) +
    theme_minimal()
)

# Fit linear models per county and get tidy summaries
models <- merged_agg %>%
  group_by(county) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(avg_price ~ avg_download_speed, data = .x)),
    summary = map(model, ~ broom::tidy(.x)),
    glance = map(model, ~ broom::glance(.x))
  )
models %>%
  select(county, summary) %>%
  unnest(summary)

models %>%
  select(county, glance) %>%
  unnest(glance) %>%
  select(county, r.squared, adj.r.squared, p.value, sigma)

#Now let’s add the correlation (Pearson’s r):
merged_agg %>%
  group_by(county) %>%
  summarise(
    correlation = cor(avg_download_speed, avg_price, use = "complete.obs")
  )



# Optional: Check data
print(glimpse(merged_data))
colnames(house_data)
colnames(speed_data)
unique(house_agg$county)

nrow(merged_data)