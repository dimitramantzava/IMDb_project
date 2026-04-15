# IMDb Rating Predictor: Web Scraping and Machine Learning Analysis
## Project Overview
This project implements an end-to-end data science workflow in R to predict movie ratings. It covers the entire lifecycle from automated data collection (web scraping) to feature engineering and predictive modeling. 

The dataset was generated using a custom-built scraper that employs a snowball sampling technique, navigating through IMDb's recommendation networks to create a diverse and interconnected dataset.

## Repository Structure
* 0_Get data.R: The scraping engine utilizing rvest and JSON-LD parsing.
* 1_Data clean and transform.R: Data cleaning, currency normalization, and feature engineering.
* 2_Linear regression.R: Model training, 5-fold cross-validation, and performance evaluation.

## Technical Challenges and Scraping Strategy
The data collection phase involved overcoming several technical hurdles related to IMDb's dynamic structure:
* Structural Inconsistencies: To avoid issues with shifting HTML layouts, the script targets hidden JSON-LD metadata scripts for consistent data extraction.
* Data Integrity: High-accuracy rating distributions were achieved by parsing the page source code directly via Regular Expressions (Regex) to map rating values to vote counts.
* Resilience: An auto-save mechanism was implemented (every 20 iterations) to mitigate data loss from connection timeouts or server throttling.
* Diversity and Bias Mitigation: The scraping queue was initialized with 5 diverse seeds (varying in genre and popularity) to ensure a wide representation of cinema and avoid "dead ends" in the recommendation graph.

## Data Preprocessing and Engineering
Raw data was transformed into a model-ready format through the following steps:
* Numeric Conversion: Handled monetary symbols and unit suffixes (K for thousands, M for millions).
* Categorical Encoding: Genres were transformed using one-hot encoding (dummy variables) for mathematical compatibility.
* Noise Reduction: Records with missing financial data (Budget/Gross) were filtered to ensure the reliability of economic impact analysis.

## Machine Learning and Evaluation
Predictive modeling was performed using the caret package, comparing Linear Regression against Random Forest via 5-fold Cross-Validation.

* Linear Regression: Provided a baseline R-squared of 64.2%.
* Random Forest: Captured non-linear relationships more effectively, achieving an R-squared of 70.2% and an RMSE of 0.514. 
* Performance: On average, the model's predictions deviate by only 0.5 stars from the actual rating.

## Key Statistical Insights
The analysis revealed several significant drivers of movie ratings:
* Critic-Audience Alignment: Metascore was the strongest predictor (t-value ~23.04), showing that audience ratings often mirror critical reception.
* The Hype Paradox: While total Gross has a positive impact, a massive Opening Weekend negatively correlates with long-term ratings (p < 0.001), suggesting that aggressive marketing can lead to inflated expectations and subsequent viewer disappointment.
* Content and Format: 
    - Positive sentiment in user reviews correlates directly with higher scores.
    - Longer runtimes (2+ hours) are generally rewarded by audiences, whereas films under 90 minutes often suffer from lower production values or underdeveloped scripts.
    - Maturity ratings: Only the NC-17 rating showed high statistical significance, suggesting that IMDb users may favor more provocative or daring productions.
* Genre Biases: Animation and Drama receive consistent positive bias. Horror films face a negative bias, as audiences tend to be less forgiving of genre clichés and tropes.
