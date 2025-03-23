
# Clearing console
cat("\014")
# Clear environment
rm(list = ls())
# Clear figures
graphics.off()

# Load required libraries
library(tidyverse)
library(readxl)
library(ggrepel)

# ============================================================
# Analysis with Top 10% Inequality Data
# ============================================================


theme_bw() +
  theme(
    plot.title = element_text(size = 12),         # Title font size
    axis.title = element_text(size = 11),         # Axis titles
    axis.text = element_text(size = 9),          # Tick labels
    legend.title = element_text(size = 10),       # Legend title
    legend.text = element_text(size = 10)         # Legend labels
  )


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


#if you want also colors for every region
# Define custom colors for each region (make sure these names exactly match your data)
custom_colors <- c(
  "Western Europe"                   = "#126782",  
  "Middle East and North Africa"     = "#cb0b0a",  
  "North America and ANZ"            = "#27a300",  
  "Latin America and Caribbean"      = "#568d66",  
  "Central and Eastern Europe"       = "#58b4d1",  
  "Southeast Asia"                   = "#fb9017",  
  "Commonwealth of Independent States" = "#a5c1ae",
  "East Asia"                        = "#fd9e02",  
  "Sub-Saharan Africa"               = "#8e0413",  
  "South Asia"                       = "#ffb703"   
)

ggplot(iqr_cleaned, aes(x = shweal, y = Ladderscore, label = Country, color = `Regional indicator`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text_repel() +
  labs(title = "Relationship between Inequality (Top 10%) and Happiness",
       x = "Inequality (share of wealth held by the top 10%)",
       y = "Happiness Score") +
  scale_color_manual(values = custom_colors)



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





######################################################################################################################################

# Plot for Top 10% data with colors and labels
plot_top10_full <- ggplot(iqr_cleaned, aes(x = shweal, y = Ladderscore, 
                                           color = `Regional indicator`, label = Country)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "#d44545", alpha = 0.6) +
  geom_text_repel(max.overlaps = Inf, size = 2, color = "black", segment.color = "grey") +
  labs(title = "Happiness vs Wealth Inequality (Top 10%) by Region",
       x = "Wealth Share of Top 10%",
       y = "Happiness Score") +
  scale_color_manual(values = custom_colors) +
  theme_bw()

# Save as high-res PNG
ggsave("happiness_vs_inequality_top10_full.png", plot = plot_top10_full,
       width = 12, height = 8, dpi = 300)


# Plot for Top 1% data with colors and labels
plot_top1_full <- ggplot(iqr_cleaned1, aes(x = shweal, y = Ladderscore, 
                                           color = `Regional indicator`, label = Country)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "#d44545", alpha = 0.6) +
  geom_text_repel(max.overlaps = Inf, size = 2, color = "black", segment.color = "grey") +
  labs(title = "Happiness vs Wealth Inequality (Top 1%) by Region",
       x = "Wealth Share of Top 1%",
       y = "Happiness Score") +
  scale_color_manual(values = custom_colors) +
  theme_bw()

# Save as high-res PNG
ggsave("happiness_vs_inequality_top1_full.png", plot = plot_top1_full,
       width = 12, height = 8, dpi = 300)

