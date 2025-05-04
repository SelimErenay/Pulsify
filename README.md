# Pulsify: A Content-Based Music Recommendation Engine

Welcome to **Pulsify**, a content-based recommendation system that suggests songs similar to a given track based on audio features. Built using R, the project applies data science techniques to analyze music track characteristics and generate meaningful recommendations.

---

## Project Structure

- `Pulsify.Rmd` – Main RMarkdown file containing the entire analysis and implementation.
- `Pulsify.html` – Rendered HTML version of the RMarkdown report.
- `dataset.csv` – The dataset of tracks with their audio features.
- `*_recommendations.csv` – Automatically generated files containing the top 10 similar songs for each seed track.
- `Pulsify.Rproj` – RStudio project file.

---

## How It Works

1. **Data Preprocessing**
   - Normalizes audio features to ensure comparability.
   - Handles missing values if any.
   - Generates exploratory data analysis (EDA) with clear visuals and interpretation.

2. **Recommendation Function**
   - Uses Euclidean distance between feature vectors to compute similarity.
   - Returns the top 10 most similar tracks to a given input.
   - Saves results to a CSV file and displays a bar chart for visual reference.

3. **Evaluation**
   - Runs the recommender on 5 popular songs.
   - Summarizes results with boxplots and histograms.
   - Evaluates diversity and distribution of recommendations.

---

## Key Features

- Clean and insightful EDA
- Content-based filtering using normalized audio features
- Auto-export to `.csv` for each recommendation
- One-click reproducibility via RMarkdown

---

## Requirements

- R 4.x
- Packages:
  - `tidyverse`
  - `ggplot2`
  - `readr`
  - `dplyr`
  - `purrr`

---

##  Example Usage

```r
recommend_track("Blinding Lights")
```

Generates a bar plot and a CSV file named `blinding_lights_recommendations.csv`.

---

## Conclusion

Using normalized audio features and Euclidean distance, **Pulsify** successfully builds a content-based recommendation engine. It evaluates similarity across multiple dimensions and enables both analysis and export of results for deeper insight and sharing.

---

## Authors

**Selim Erenay and Omer Erenay**  
Project developed as part of a data science course assignment.

---


