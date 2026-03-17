# EU Have Your Say Feedback Corpus

This repository contains a small workflow for building and analysing a corpus of public feedback texts from the European Commission **Have Your Say** platform. The code is organized as a step-by-step pipeline: first identify initiatives and feedback pages, then scrape sampled feedback texts, then compute text-based measures, and finally generate summary tables and figures.

## Repository structure

### 1. `scrape_top_initiatives.R`
This script builds the initiative-level dataset from the **Have Your Say** website. It scrapes closed initiative list pages, extracts metadata such as title, topic, type of act, and feedback period, and then retrieves the feedback count and feedback page URL for selected initiatives. It also creates candidate pools for the pre-period and exports top initiatives based on feedback volume.

Main outputs include files such as:
- `initiatives_page_*.csv`
- `initiatives_all_combined.csv`
- `initiatives_target.csv`
- `pre_candidates.csv`
- `pre_scored.csv`
- batch-level count files for detail pages

### 2. `eu_have_your_say_feedback_fulltext_scraper_requests_sampled_500_per_group.ipynb`
This notebook scrapes feedback full texts from initiative feedback pages using a dynamic Selenium-based workflow. It first collects individual feedback URLs for each initiative, keeps the group label, draws a fixed random sample per group, and then downloads the corresponding feedback texts.

Main outputs include:
- `feedback_url_pool.csv`
- `sampled_feedback_urls_500_per_group.csv`
- `feedbacks_full.csv`
- `failed_feedback_urls.csv`

### 3. `calculate_text_based_measures.R`
This script merges the initiative-level sample with the scraped full texts and computes text-based measures for each feedback entry. The measures include:
- word count
- Flesch-Kincaid Grade Level (FKGL)
- MATTR lexical diversity
- first-person pronoun frequency and proportion
- dictionary-based sentiment measures (positive, negative, anxiety)
- net sentiment

Main outputs include:
- `have_your_say_feedback_corpus.csv`
- `have_your_say_feedback_corpus_with_text_measures.csv`

### 4. `visualisation.R`
This script reads the final corpus with text measures, harmonizes the group labels, and produces descriptive tables and figures for the paper. The outputs are saved into a paper output folder with separate subfolders for figures and tables.

Main outputs include:
- `paper_outputs/figures/*.png`
- `paper_outputs/tables/*.csv`
- `paper_outputs/tables/*.md`

## Suggested workflow

Run the files in the following order:

1. **`scrape_top_initiatives.R`**  
   Build the initiative-level sample and export initiative metadata and feedback counts.

2. **`eu_have_your_say_feedback_fulltext_scraper_requests_sampled_500_per_group.ipynb`**  
   Collect feedback URLs, sample feedback pages by group, and scrape full texts.

3. **`calculate_text_based_measures.R`**  
   Merge initiative data with feedback texts and compute linguistic features.

4. **`visualisation.R`**  
   Create the final descriptive tables and plots.

## Required input files

Depending on where you start in the workflow, the scripts expect some intermediate files to already exist in the working directory. The most important ones are:

- `combined_top20.csv`
- `feedbacks_full.csv`
- `have_your_say_feedback_corpus.csv`
- `have_your_say_feedback_corpus_with_text_measures.csv`

## R package dependencies

The R scripts use packages such as:

- `rvest`
- `xml2`
- `chromote`
- `purrr`
- `dplyr`
- `stringr`
- `tibble`
- `readr`
- `tidyr`
- `quanteda`
- `quanteda.textstats`
- `tidytext`
- `textdata`
- `SnowballC`
- `ggplot2`
- `forcats`
- `scales`
- `patchwork`
- `broom`
- `knitr`

## Python package dependencies

The notebook uses:

- `pandas`
- `requests`
- `beautifulsoup4`
- `tqdm`
- `selenium`
- `webdriver-manager`
- `lxml`

## Notes

- The scraping steps depend on the current structure of the European Commission website, so selectors may need to be updated if the site changes.
- Some scripts assume that input and output files are stored in the same working directory.
- The notebook file name refers to sampling 500 per group, while the actual sample size is controlled inside the notebook by the `SAMPLE_N_PER_GROUP` setting.

## Project goal

The overall goal of this repository is to compare public feedback texts across two time periods and examine differences in length, readability, lexical diversity, sentiment, and first-person pronoun use.
