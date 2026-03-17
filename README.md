# css_final_project

This repository contains the code files and output files for the final project.

## Project description

This project studies whether written policy feedback on the EU “Have Your Say” consultation platform changed from the pre-LLM era to the post-LLM era. The main goal is to compare the linguistic style of feedback texts across time, with a focus on text length, readability, lexical diversity, sentiment, and first-person pronoun use. The project is descriptive and exploratory, and it uses public consultation data scraped from the platform.

## Files in this repository

### Code files

- `scrape_top_initiatives.R`  
  Uses the Have Your Say website to collect initiative-level information and build the initial dataset.

- `fulltext_scraper.ipynb`  
  Collects feedback URLs, samples feedback pages, and scrapes the full text of feedback comments.

- `calculate_text_based_measures.R`  
  Merges the scraped feedback texts with initiative data and calculates text-based measures.

- `visualisations.R`  
  Uses the final dataset to create tables and figures.

### Output files

- `combined_top20.csv`  
  Initiative-level dataset used for the later scraping steps.

- `feedback_url_pool.csv`  
  Pool of collected feedback URLs.

- `feedbacks_full.csv`  
  Scraped full-text feedback data.

- `have_your_say_feedback_corpus_with_text_measures.csv`  
  Final dataset with calculated text measures.

- `A1_initiative_composition.png`  
- `A2_density_core_measures.png`  
- `fig1_wordcount_distribution_boxplot_zoomed.png`  
- `fig2_fkgl_mattr_boxplot_zoomed.png`  
- `fig3_sentiment_comparison_boxplot_zoomed.png`  
- `fig4_firstperson_pronouns_boxplot_zoomed.png`  
- `fig5_time_trends.png`  
- `fig6_coefficient_plot.png`  

These image files are the main visual outputs of the project.

## How to use

Run the code files in this order:

1. `scrape_top_initiatives.R`  
2. `fulltext_scraper.ipynb`  
3. `calculate_text_based_measures.R`  
4. `visualisations.R`  
