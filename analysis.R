# Load required libraries
library(tidyverse)
library(readxl)
library(ggrepel)

# ============================================================
# Analysis with Top 10% Inequality Data
# ============================================================

# Import datasets
happiness <- read_csv("happiness_data.csv")
inequality <- read_excel("inequality_data.xlsx", sheet = 1)

# Inspect column names
cat("Happiness data columns:\n")
print(names(happiness))
cat("Inequality data columns:\n")
print(names(inequality))

# Clean happiness dataset: rename and trim Country names
happiness <- happiness %>%
  rename(Country = `Country name`) %>%
  mutate(Country = trimws(as.character(Country)))

# Clean inequality dataset: trim Country names
inequality <- inequality %>%
  mutate(Country = trimws(as.character(Country)))

# Merge datasets by Country
merged_data <- inner_join(happiness, inequality, by = "Country")
cat("Merged Data Preview (Top 10%):\n")
print(head(merged_data))
str(merged_data)
print(names(merged_data))

# Convert key variables to numeric
merged_data <- merged_data %>%
  mutate(
    shweal = as.numeric(shweal),
    Ladderscore = as.numeric(Ladderscore)
  )

# Remove outliers using the IQR method for both shweal and Ladderscore
iqr_cleaned <- merged_data %>%
  filter(
    shweal >= quantile(shweal, 0.25, na.rm = TRUE) - 1.5 * IQR(shweal, na.rm = TRUE) &
      shweal <= quantile(shweal, 0.75, na.rm = TRUE) + 1.5 * IQR(shweal, na.rm = TRUE) &
      Ladderscore >= quantile(Ladderscore, 0.25, na.rm = TRUE) - 1.5 * IQR(Ladderscore, na.rm = TRUE) &
      Ladderscore <= quantile(Ladderscore, 0.75, na.rm = TRUE) + 1.5 * IQR(Ladderscore, na.rm = TRUE)
  )

# Visualize the relationship
ggplot(iqr_cleaned, aes(x = shweal, y = Ladderscore)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between Inequality (Top 10%) and Happiness",
       x = "Inequality (share of wealth held by the top 10%)",
       y = "Happiness Score")

# Calculate the correlation coefficient
cor_coef <- cor(iqr_cleaned$shweal, iqr_cleaned$Ladderscore, use = "complete.obs")
print(paste("Correlation coefficient (Top 10%):", cor_coef))

# Fit a linear regression model
model <- lm(Ladderscore ~ shweal, data = iqr_cleaned)
summary(model)




sum(is.na(iqr_cleaned$Country))
which(iqr_cleaned$Country == "")
#if you want to see every country name
ggplot(iqr_cleaned, aes(x = shweal, y = Ladderscore, label = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text_repel(max.overlaps = Inf) +
  labs(
    title = "Relationship between Inequality and Happiness",
    x = "Inequality (share of wealth held by the top 10%)",
    y = "Happiness Score"
  )

#if you do not want to see every country name
ggplot(iqr_cleaned, aes(x = shweal, y = Ladderscore, label = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text_repel() +
  labs(title = "Relationship between Inequality (Top 10%) and Happiness",
       x = "Inequality (share of wealth held by the top 10%)",
       y = "Happiness Score")


# ============================================================
# Analysis with Top 1% Inequality Data
# ============================================================

# Import inequality1 dataset (top 1% data)
inequality1 <- read_excel("inequality1_data.xlsx", sheet = 1)

# Inspect column names
cat("Inequality1 data columns:\n")
print(names(inequality1))

# Clean inequality1 dataset: trim Country names
inequality1 <- inequality1 %>%
  mutate(Country = trimws(as.character(Country)))

# Merge with the happiness dataset (already cleaned)
merged_data1 <- inner_join(happiness, inequality1, by = "Country")
cat("Merged Data Preview (Top 1%):\n")
print(head(merged_data1))
str(merged_data1)
print(names(merged_data1))

# Convert key variables to numeric
merged_data1 <- merged_data1 %>%
  mutate(
    shweal = as.numeric(shweal),
    Ladderscore = as.numeric(Ladderscore)
  )

# Remove outliers using the IQR method
iqr_cleaned1 <- merged_data1 %>%
  filter(
    shweal >= quantile(shweal, 0.25, na.rm = TRUE) - 1.5 * IQR(shweal, na.rm = TRUE) &
      shweal <= quantile(shweal, 0.75, na.rm = TRUE) + 1.5 * IQR(shweal, na.rm = TRUE) &
      Ladderscore >= quantile(Ladderscore, 0.25, na.rm = TRUE) - 1.5 * IQR(Ladderscore, na.rm = TRUE) &
      Ladderscore <= quantile(Ladderscore, 0.75, na.rm = TRUE) + 1.5 * IQR(Ladderscore, na.rm = TRUE)
  )

# Visualize the relationship for Top 1% data
ggplot(iqr_cleaned1, aes(x = shweal, y = Ladderscore)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between Inequality (Top 1%) and Happiness",
       x = "Inequality (share of wealth held by the top 1%)",
       y = "Happiness Score")

# Calculate the correlation coefficient for Top 1% data
cor_coef1 <- cor(iqr_cleaned1$shweal, iqr_cleaned1$Ladderscore, use = "complete.obs")
print(paste("Correlation coefficient (Top 1%):", cor_coef1))

# Fit a linear regression model for Top 1% data
model1 <- lm(Ladderscore ~ shweal, data = iqr_cleaned1)
summary(model1)
