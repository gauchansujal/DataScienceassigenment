library(tidyverse)
library(janitor)
library(dplyr)
library(corrplot)

# --- STEP 1: Unzip and read data ---

unzip("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/Performancetables_082927.zip", exdir = "unzipped_2021")
csv_2021 <- list.files("unzipped_2021", pattern = "england_ks4final\\.csv$", full.names = TRUE, recursive = TRUE)
SchoolPerformance2021 <- read_csv(csv_2021)

unzip("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/Performancetables_081208.zip", exdir = "unzipped_2022")
csv_2022 <- list.files("unzipped_2022", pattern = "england_ks4final\\.csv$", full.names = TRUE, recursive = TRUE)
SchoolPerformance2022 <- read_csv(csv_2022)

unzip("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/Performancetables_081807.zip", exdir = "unzipped_2023")
csv_2023 <- list.files("unzipped_2023", pattern = "england_ks4final\\.csv$", full.names = TRUE, recursive = TRUE)
SchoolPerformance2023 <- read_csv(csv_2023)

unzip("C:/Users/Sujal/OneDrive/Documents/R lab/data science project/downlode the datasets/Performancetables_081859.zip", exdir = "unzipped_2024")
csv_2024 <- list.files("unzipped_2024", pattern = "england_ks4final\\.csv$", full.names = TRUE, recursive = TRUE)
SchoolPerformance2024 <- read_csv(csv_2024)

# --- STEP 2: Add year and convert all columns to character ---

SchoolPerformance2021 <- SchoolPerformance2021 %>% mutate(across(everything(), as.character), year = "2021")
SchoolPerformance2022 <- SchoolPerformance2022 %>% mutate(across(everything(), as.character), year = "2022")
SchoolPerformance2023 <- SchoolPerformance2023 %>% mutate(across(everything(), as.character), year = "2023")
SchoolPerformance2024 <- SchoolPerformance2024 %>% mutate(across(everything(), as.character), year = "2024")

# --- STEP 3: Bind rows and clean column names ---

SchoolPerformance <- bind_rows(
  SchoolPerformance2021,
  SchoolPerformance2022,
  SchoolPerformance2023,
  SchoolPerformance2024
) # After binding all years' data into SchoolPerformance:

SchoolPerformance <- SchoolPerformance %>% clean_names()

# Then your filtering and cleaning:
cleanSchoolPerformance <- SchoolPerformance %>%
  filter(lea %in% c("370", "371", "372", "373",   # South Yorkshire
                    "380", "381", "382", "383", "384")) %>%
  mutate(
    att8scr = na_if(att8scr, "SUP"),
    att8scr = na_if(att8scr, "NE"),
    att8scr = as.numeric(att8scr),
    
    p8mea = na_if(p8mea, "SUP"),
    p8mea = na_if(p8mea, "NE"),
    p8mea = as.numeric(p8mea),
    
    year = as.integer(year),
    
    lea = as.factor(lea),
    schname = as.factor(schname)
  ) %>%
  filter(!is.na(att8scr), !is.na(p8mea)) %>%
  rename(
    school_name = schname,
    postcode = pcode,
    district_code = lea,
    progress8_score = p8mea,
    attainment8_score = att8scr
  )


# Create output folder if not exists
dir.create("Cleaned Data", showWarnings = FALSE)

# Save cleaned data to CSV
write_csv(cleanSchoolPerformance, "Cleaned Data/finalSchoolPerformance.csv")

# --- STEP 8: View cleaned data ---
View(cleanSchoolPerformance)

# Check colnames first
print(colnames(south_yorkshire_2022))

# Then replace lea with the correct name, e.g. "LEA"
ggplot(south_yorkshire_2022, aes(x = LEA, y = as.numeric(ATT8SCR))) +
  geom_boxplot(fill = "#00BFC4", color = "black") +
  labs(
    title = "Boxplot of Average Attainment 8 Scores (2022)",
    subtitle = "South Yorkshire Districts",
    x = "District (LEA Code)",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

west_yorkshire_2022 <- SchoolPerformance %>%
  filter(year == 2022, lea %in% c("380", "381", "382", "383", "384"))

ggplot(west_yorkshire_2022, aes(x = lea, y = as.numeric(att8scr))) +
  geom_boxplot(fill = "#00BFC4", color = "black") +
  labs(
    title = "Boxplot of Average Attainment 8 Scores (2022)",
    subtitle = "West Yorkshire Districts",
    x = "District (LEA Code)",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()


# Filter for South and West Yorkshire districts
selected_leas <- c("370", "371", "372", "373",   # South Yorkshire
                   "380", "381", "382", "383", "384") # West Yorkshire

# Filter and prepare data
df_plot <- SchoolPerformance %>%
  filter(lea %in% selected_leas) %>%
  mutate(
    year = as.integer(year),
    att8scr = as.numeric(att8scr),
    lea = factor(lea)  # factor for nicer legend
  ) %>%
  filter(!is.na(att8scr), !is.na(year))

# Plot line graph
ggplot(df_plot, aes(x = year, y = att8scr, color = lea, group = lea)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +      # mean line per LEA
  stat_summary(fun = mean, geom = "point", size = 2) +       # mean points per LEA
  labs(
    title = "Average Attainment 8 Score Over Years",
    subtitle = "By Districts in South and West Yorkshire",
    x = "Year",
    y = "Average Attainment 8 Score",
    color = "District (LEA)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(df_plot$year))   # show all years on x-axis

#Sample R code for recommendation system:
# Calculate thresholds
att8_mean <- mean(cleanSchoolPerformance$attainment8_score, na.rm = TRUE)
p8_mean <- mean(cleanSchoolPerformance$progress8_score, na.rm = TRUE)

# Flag schools with good performance
recommendations <- cleanSchoolPerformance %>%
  mutate(
    att8_flag = attainment8_score >= att8_mean,
    p8_flag = progress8_score > 0,
    recommended = att8_flag & p8_flag
  ) %>%
  filter(recommended) %>%
  arrange(district_code, desc(attainment8_score))

# View recommended schools per district
recommendations %>%
  select(school_name, district_code, attainment8_score, progress8_score) %>%
  group_by(district_code) %>%
  slice_max(order_by = attainment8_score, n = 5) %>%  # Top 5 schools per district
  ungroup()




# --- 1. Correlation between attainment8_score and progress8_score ---
correlation <- cor(cleanSchoolPerformance$attainment8_score, cleanSchoolPerformance$progress8_score, use = "complete.obs")
cat("Correlation between Attainment 8 and Progress 8:", correlation, "\n")

# --- 2. Summary statistics by district ---
district_summary <- cleanSchoolPerformance %>%
  group_by(district_code) %>%
  summarise(
    mean_attainment8 = mean(attainment8_score, na.rm = TRUE),
    mean_progress8 = mean(progress8_score, na.rm = TRUE),
    count = n()
  )
print(district_summary)

# --- 3. ANOVA: Does attainment8_score differ by district? ---
anova_attainment <- aov(attainment8_score ~ district_code, data = cleanSchoolPerformance)
summary(anova_attainment)

# ANOVA: Does progress8_score differ by district?
anova_progress <- aov(progress8_score ~ district_code, data = cleanSchoolPerformance)
summary(anova_progress)

# --- 4. Visualization: Trends over years by district ---

ggplot(cleanSchoolPerformance, aes(x = year, y = attainment8_score, color = district_code, group = district_code)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(title = "Average Attainment 8 Score Over Years by District",
       x = "Year",
       y = "Average Attainment 8 Score",
       color = "District") +
  theme_minimal()

ggplot(cleanSchoolPerformance, aes(x = year, y = progress8_score, color = district_code, group = district_code)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(title = "Average Progress 8 Score Over Years by District",
       x = "Year",
       y = "Average Progress 8 Score",
       color = "District") +
  theme_minimal()

# --- 5. Optional: Correlation matrix plot of key numeric variables ---

numeric_vars <- cleanSchoolPerformance %>%
  select(attainment8_score, progress8_score, year) %>%
  drop_na()

cor_matrix <- cor(numeric_vars)
print(cor_matrix)


corrplot(cor_matrix, method = "circle")



