install.packages(c(
  "rvest", "xml2", "chromote", "purrr", "dplyr",
  "stringr", "tibble", "readr", "tidyr"
))

library(rvest)
library(xml2)
library(chromote)
library(purrr)
library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(tidyr)

# Settings

pages_to_scrape <- 1:44

# Batch size for detail pages: 5 each time.
batch_size <- 5

# File name prefixes
initiative_page_prefix <- "initiatives_page_"
detail_batch_prefix <- "initiative_counts_batch_"
pre_detail_batch_prefix <- "pre_counts_batch_"


# 1. Helper functions

get_text_or_na <- function(node, css) {
  x <- html_element(node, css)
  if (length(x) == 0) return(NA_character_)
  html_text2(x)
}

get_attr_or_na <- function(node, css, attr) {
  x <- html_element(node, css)
  if (length(x) == 0) return(NA_character_)
  html_attr(x, attr)
}

extract_card <- function(card) {
  dts <- card |> html_elements("dt") |> html_text2()
  dds <- card |> html_elements("dd") |> html_text2()
  field_map <- setNames(as.list(dds), dts)
  
  tibble(
    title = get_text_or_na(card, ".ecl-content-block__title"),
    href = get_attr_or_na(card, "a", "href"),
    topic = if ("Topic" %in% names(field_map)) field_map[["Topic"]] else NA_character_,
    type_of_act = if ("Type of act" %in% names(field_map)) field_map[["Type of act"]] else NA_character_,
    feedback_period = if ("Feedback period" %in% names(field_map)) field_map[["Feedback period"]] else NA_character_
  )
}

extract_end_year <- function(x) {
  yrs <- str_extract_all(x, "\\b(19|20)\\d{2}\\b")
  map_int(yrs, \(z) if (length(z) == 0) NA_integer_ else as.integer(tail(z, 1)))
}

get_one_page_by_num <- function(page_num, wait_sec = 8) {
  url <- paste0(
    "https://ec.europa.eu/info/law/better-regulation/have-your-say/initiatives_en",
    "?feedbackStatus=CLOSED&page=", page_num
  )
  
  cat("Scraping initiative list page:", page_num, "\n")
  
  live <- read_html_live(url)
  Sys.sleep(wait_sec)
  
  cards <- html_elements(live, "initivative-item article")
  
  map_dfr(cards, extract_card) |>
    mutate(
      initiative_url = if_else(
        str_detect(href, "^http"),
        href,
        paste0("https://ec.europa.eu", href)
      ),
      page_num = page_num
    ) |>
    select(page_num, title, initiative_url, topic, type_of_act, feedback_period)
}

safe_get_one_page <- function(page_num, max_tries = 3) {
  for (i in 1:max_tries) {
    cat("  Try", i, "for page", page_num, "\n")
    
    res <- tryCatch(
      get_one_page_by_num(page_num, wait_sec = 8 + 2 * (i - 1)),
      error = function(e) e
    )
    
    if (!inherits(res, "error")) {
      return(res)
    }
    
    cat("  Failed page", page_num, "on try", i, ":", conditionMessage(res), "\n")
    Sys.sleep(5 * i)
    gc()
  }
  
  cat("  Giving up page", page_num, "\n")
  
  tibble(
    page_num = integer(),
    title = character(),
    initiative_url = character(),
    topic = character(),
    type_of_act = character(),
    feedback_period = character()
  )
}

get_feedback_count <- function(detail_url, wait_sec = 6) {
  cat("Detail:", detail_url, "\n")
  
  live <- read_html_live(detail_url)
  Sys.sleep(wait_sec)
  
  link_text <- html_elements(live, "a") |> html_text2()
  link_href <- html_elements(live, "a") |> html_attr("href")
  
  idx <- grepl("All feedback and statistics", link_text, ignore.case = TRUE)
  
  matched_text <- link_text[idx]
  matched_href <- link_href[idx]
  
  feedback_label <- if (length(matched_text) == 0) NA_character_ else matched_text[1]
  feedback_href  <- if (length(matched_href) == 0) NA_character_ else matched_href[1]
  
  if (!is.na(feedback_href) && !grepl("^http", feedback_href)) {
    feedback_href <- paste0("https://ec.europa.eu", feedback_href)
  }
  
  feedback_count <- if (is.na(feedback_label)) {
    NA_real_
  } else {
    str_extract(feedback_label, "\\d+") |> as.numeric()
  }
  
  tibble(
    feedback_label = feedback_label,
    feedback_count = feedback_count,
    feedback_page_url = feedback_href
  )
}

safe_get_feedback_count <- function(detail_url, max_tries = 3) {
  for (i in 1:max_tries) {
    cat("  Try", i, "for detail", detail_url, "\n")
    
    res <- tryCatch(
      get_feedback_count(detail_url, wait_sec = 6 + 2 * (i - 1)),
      error = function(e) e
    )
    
    if (!inherits(res, "error")) {
      return(res)
    }
    
    cat("  Failed detail on try", i, ":", conditionMessage(res), "\n")
    Sys.sleep(5 * i)
    gc()
  }
  
  tibble(
    feedback_label = NA_character_,
    feedback_count = NA_real_,
    feedback_page_url = NA_character_
  )
}


# 2. Scrape CLOSED initiative list pages


for (p in pages_to_scrape) {
  out_file <- paste0(initiative_page_prefix, p, ".csv")
  
  if (file.exists(out_file)) {
    cat("Skipping existing file:", out_file, "\n")
    next
  }
  
  df <- safe_get_one_page(p, max_tries = 3)
  write_csv(df, out_file)
  Sys.sleep(4)
}

# =========================
# 3. Combine all list pages
# =========================

page_files <- list.files(
  pattern = paste0("^", initiative_page_prefix, "[0-9]+\\.csv$")
)

initiatives_all <- map_dfr(page_files, read_csv, show_col_types = FALSE) |>
  distinct() |>
  mutate(end_year = extract_end_year(feedback_period))

write_csv(initiatives_all, "initiatives_all_combined.csv")

cat("\n=== initiatives_all summary ===\n")
print(length(page_files))
print(nrow(initiatives_all))
print(range(initiatives_all$end_year, na.rm = TRUE))
print(initiatives_all |> count(end_year) |> arrange(end_year))


# 4. Build the study sample pool


initiatives_target <- initiatives_all |>
  filter(end_year <= 2022 | end_year >= 2024)

write_csv(initiatives_target, "initiatives_target.csv")

cat("\n=== initiatives_target summary ===\n")
print(nrow(initiatives_target))
print(initiatives_target |> count(end_year) |> arrange(end_year))

# =========================
# 5. Build pre_candidates and scrape only their feedback counts
#    in batches of 5
# =========================

pre_candidates <- initiatives_all |>
  filter(end_year <= 2022) |>
  distinct()

write_csv(pre_candidates, "pre_candidates.csv")

cat("\n=== pre_candidates summary ===\n")
print(nrow(pre_candidates))
print(pre_candidates |> count(end_year) |> arrange(end_year))

s <- 1
e <- 5

cat("\n=== Scraping PRE detail batch:", s, "to", e, "===\n")

batch_dfto5 <- pre_candidates |>
  dplyr::slice(s:e) |>
  dplyr::mutate(
    res = purrr::map(initiative_url, safe_get_feedback_count)
  ) |>
  tidyr::unnest(res)

readr::write_csv(batch_dfto5, paste0(pre_detail_batch_prefix, s, "_", e, ".csv"))

s <- 6
e <- 10

cat("\n=== Scraping PRE detail batch:", s, "to", e, "===\n")

batch_dfto10 <- pre_candidates |>
  dplyr::slice(s:e) |>
  dplyr::mutate(
    res = purrr::map(initiative_url, safe_get_feedback_count)
  ) |>
  tidyr::unnest(res)

readr::write_csv(batch_dfto10, paste0(pre_detail_batch_prefix, s, "_", e, ".csv"))

s <- 11
e <- 15

cat("\n=== Scraping PRE detail batch:", s, "to", e, "===\n")

batch_dfto15 <- pre_candidates |>
  dplyr::slice(s:e) |>
  dplyr::mutate(
    res = purrr::map(initiative_url, safe_get_feedback_count)
  ) |>
  tidyr::unnest(res)

readr::write_csv(batch_dfto15, paste0(pre_detail_batch_prefix, s, "_", e, ".csv"))

# =========================
# 6. Combine all pre detail batches
# =========================

pre_batch_files <- list.files(
  pattern = paste0("^", pre_detail_batch_prefix, "[0-9]+_[0-9]+\\.csv$")
)

pre_scored <- map_dfr(
  pre_batch_files,
  read_csv,
  show_col_types = FALSE
) |>
  distinct()

write_csv(pre_scored, "pre_scored.csv")

cat("\n=== pre_scored summary ===\n")
print(nrow(pre_scored))
print(summary(pre_scored$feedback_count))

# =========================
# 7. Export pre candidates and their scores
# =========================

write_csv(pre_candidates, "pre_candidates.csv")
write_csv(pre_scored, "pre_scored.csv")

cat("\n=== pre_candidates summary ===\n")
print(nrow(pre_candidates))
print(pre_candidates |> count(end_year) |> arrange(end_year))

cat("\n=== pre_scored summary ===\n")
print(nrow(pre_scored))
print(pre_scored |> count(end_year) |> arrange(end_year))


# 8. Build top 10 pre / post


top_pre <- pre_scored |>
  filter(!is.na(feedback_count)) |>
  arrange(desc(feedback_count)) |>
  slice_head(n = 10)

# Reuse the existing initiatives_scored file for post.
if (file.exists("initiatives_scored.csv")) {
  initiatives_scored <- read_csv("initiatives_scored.csv", show_col_types = FALSE)
} else if (file.exists("initiatives_scored_first50pages.csv")) {
  initiatives_scored <- read_csv("initiatives_scored_first50pages.csv", show_col_types = FALSE)
} else {
  initiatives_scored <- tibble()
}

top_post <- initiatives_scored |>
  filter(end_year >= 2024, !is.na(feedback_count)) |>
  arrange(desc(feedback_count)) |>
  slice_head(n = 10)

write_csv(top_pre, "top10_pre_upto2022.csv")
write_csv(top_post, "top10_post_from2024.csv")

cat("\n=== top_pre ===\n")
print(top_pre |> select(title, end_year, feedback_count, feedback_page_url))

cat("\n=== top_post ===\n")
print(top_post |> select(title, end_year, feedback_count, feedback_page_url))


# Reuse the existing initiatives_scored file for post.

top_pre <- read_csv("top10_pre_upto2022.csv", show_col_types = FALSE)
top_post <- read_csv("top10_post_from2024.csv", show_col_types = FALSE)

combined_top20 <- bind_rows(
  top_pre |> mutate(group = "pre2022"),
  top_post |> mutate(group = "post2024")
)

write_csv(combined_top20, "combined_top20.csv")
