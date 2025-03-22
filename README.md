# happiness-inequality-analysis

# Relationship between Wealth Inequality and Happiness

This project explores the relationship between wealth inequality and national happiness. By merging datasets on happiness scores and wealth distribution across countries, the analysis investigates whether greater concentration of wealth, particularly among the top 1%, is associated with lower overall happiness.

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

Initial results indicate a weak negative correlation between the wealth share of the top 1% and national happiness, suggesting that higher wealth concentration may be slightly associated with lower happiness scores. However, the relationship is complex, and further analysis is needed to account for additional variables and contextual factors.

## How to Run the Analysis

1. **Clone the repository:**

   ```bash
   git clone https://github.com/yourusername/happiness-inequality-analysis.git
   cd happiness-inequality-analysis
