# Relationship between Wealth Inequality and Happiness

This project explores the relationship between wealth inequality and national happiness. By merging datasets on happiness scores and wealth distribution across countries, the analysis investigates whether greater concentration of wealth, particularly among the top 10%, is associated with lower overall happiness.

## Project Overview

- **Data Sources:**  
  - **Happiness Data:** Contains country-level happiness scores (e.g., from the World Happiness Report).  
  - **Inequality Data:** Contains measures of wealth distribution, including the share of wealth held by the top 10% and the top 1% of the population.

- **Objectives:**  
  - Merge and clean datasets to ensure a consistent, analyzable format.  
  - Identify and remove outliers using methods like the IQR and Z-score techniques.  
  - Visualize the relationship between wealth concentration and happiness through scatter plots and regression lines.  
  - Calculate correlation coefficients and fit linear regression models to quantify the association.

- **Methodology:**  
  - **Data Preparation:** Import and merge data, rename and trim columns, and convert variables to appropriate types.  
  - **Outlier Handling:** Apply outlier detection and cleaning methods to refine the dataset.  
  - **Analysis:** Use scatter plots and regression analysis to examine how the share of wealth held by the top 1% correlates with happiness scores.  
  - **Interpretation:** Assess the strength and direction of the relationship and consider potential implications for policy and societal well-being.

## Key Findings

- **Top 1% Analysis:**  
  Initial results indicate a weak negative correlation between the wealth share of the top 1% and national happiness, suggesting that higher wealth concentration in this group may be slightly associated with lower happiness scores.

- **Top 10% Analysis:**  
  The analysis of the top 10% wealth distribution shows a moderate negative relationship with national happiness. The correlation coefficient is approximately -0.32, and the linear regression model reveals a statistically significant negative slope. This indicates that, on average, as the share of wealth held by the top 10% increases, national happiness scores tend to decrease. However, the moderate strength of this relationship also suggests that other factors likely contribute to overall happiness.

## How to Run the Analysis

1. **Clone the repository:**

   ```bash
   git clone https://github.com/yourusername/happiness-inequality-analysis.git
   cd happiness-inequality-analysis


2. **Install the required R packages:**

   ```bash
   install.packages("tidyverse")
   install.packages("readxl")
   install.packages("ggrepel")
  
3. **Run the R script(s):**

Open the provided R script in RStudio or your preferred R environment and execute it. The script includes data import, cleaning, visualization, and analysis steps.

## Repository Structure

- README.md - This file.
- happiness_data.csv - The dataset containing happiness scores.
- inequality_data.xlsx - The dataset with inequality data for the top 10%.
- inequality1_data.xlsx - The dataset with inequality data for the top 1%.
- analysis.R - The main R script performing the analysis.

