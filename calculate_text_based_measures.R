
install.packages('SnowballC')
install.packages('quanteda.textstats')
install.packages('tidytext')
install.packages('textdata')

library(readr)
library(dplyr)

library(stringr)
library(quanteda)
library(quanteda.textstats)
library(tidytext)
library(purrr)
library(SnowballC)
library(textdata)

combined_top20 <- read_csv("combined_top20.csv")
feedbacks_full <- read_csv("feedbacks_full.csv")

feedbacks_full_merged <- feedbacks_full %>%
  left_join(
    combined_top20,
    by = c("initiative" = "title")
  ) 

write_csv(feedbacks_full_merged, "have_your_say_feedback_corpus.csv")




#-----------------------------
# 1. Read data
#-----------------------------
df <- read_csv("have_your_say_feedback_corpus.csv")

# Change this if your text column has a different name.
text_var <- "feedback_text"

#-----------------------------
# 2. Basic cleaning
#-----------------------------
df <- df %>%
  mutate(
    doc_id = row_number(),
    text_raw = .data[[text_var]],
    text_raw = if_else(is.na(text_raw), "", text_raw),
    text_raw = str_squish(text_raw)
  )

# Keep only non-empty texts.
df_text <- df %>%
  filter(text_raw != "")

#-----------------------------
# 3. quanteda corpus / tokens
#-----------------------------
corp <- corpus(df_text, text_field = "text_raw")

# Used for length, MATTR, and pronoun measures.
toks_words <- tokens(
  corp,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE
) |>
  tokens_tolower()

#-----------------------------
# 4. Word count
#-----------------------------
word_count <- ntoken(toks_words)

#-----------------------------
# 5. Flesch-Kincaid Grade Level
# FKGL is the readability measure used here.
#-----------------------------
readability_df <- textstat_readability(
  corp,
  measure = c("Flesch.Kincaid")
)

#-----------------------------
# 6. MATTR
# MATTR is more stable than simple TTR for different text lengths.
# Use a window of 20; this can be changed if needed.
#-----------------------------
# Set the window size.
mattr_window <- 20

# Compute MATTR for each text.
mattr_vec <- map_dbl(seq_along(toks_words), function(i) {
  tok_i <- toks_words[i]
  n_i <- ntoken(tok_i)
  
  if (is.na(n_i) || n_i < mattr_window) {
    return(NA_real_)
  }
  
  textstat_lexdiv(
    tok_i,
    measure = "MATTR",
    MATTR_window = mattr_window
  )$MATTR[[1]]
})

mattr_df <- tibble(
  doc_id = df_text$doc_id,
  mattr = mattr_vec
)

#-----------------------------
# 7. First-person pronoun share
# Used as a proxy for psychological involvement.
#-----------------------------
first_pronouns <- c(
  "i", "me", "my", "mine", "myself",
  "we", "us", "our", "ours", "ourselves"
)

pronoun_dfm <- dfm(toks_words)
pronoun_hits <- rowSums(dfm_select(pronoun_dfm, pattern = first_pronouns))
pronoun_prop <- pronoun_hits / pmax(word_count, 1)

#-----------------------------
# 8. Dictionary-based sentiment
# Track positive, negative, and anxiety terms.
# A lexicon-based approach keeps the measure easy to interpret.
#
# Here:
# - positive / negative use NRC
# - anxiety also uses NRC
# - output is scaled by total word count
#-----------------------------
tokens_tbl <- tibble(
  doc_id = df_text$doc_id,
  text = df_text$text_raw
) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(str_detect(word, "[a-z]"))

nrc <- get_sentiments("nrc")

nrc_counts <- tokens_tbl %>%
  inner_join(nrc, by = "word") %>%
  filter(sentiment %in% c("positive", "negative", "fear")) %>%
  mutate(sentiment = case_when(
    sentiment == "fear" ~ "anxiety",
    TRUE ~ sentiment
  )) %>%
  count(doc_id, sentiment, name = "n") %>%
  tidyr::pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  )

# Fill in 0 when no sentiment words are matched.
nrc_counts <- df_text %>%
  select(doc_id) %>%
  left_join(nrc_counts, by = "doc_id") %>%
  mutate(
    positive = coalesce(positive, 0L),
    negative = coalesce(negative, 0L),
    anxiety  = coalesce(anxiety, 0L)
  )

# Convert counts to proportions.
sent_positive_prop <- nrc_counts$positive / pmax(word_count, 1)
sent_negative_prop <- nrc_counts$negative / pmax(word_count, 1)
sent_anxiety_prop  <- nrc_counts$anxiety  / pmax(word_count, 1)

# Also create a simple net sentiment measure.
sent_net <- sent_positive_prop - sent_negative_prop

#-----------------------------
# 9. Merge back to the main data
#-----------------------------
text_measures <- tibble(
  doc_id = df_text$doc_id,
  word_count = as.numeric(word_count),
  fkgl = readability_df$Flesch.Kincaid,
  mattr = mattr_df$mattr,
  first_person_pronoun_n = as.numeric(pronoun_hits),
  first_person_pronoun_prop = as.numeric(pronoun_prop),
  sent_positive_prop = as.numeric(sent_positive_prop),
  sent_negative_prop = as.numeric(sent_negative_prop),
  sent_anxiety_prop = as.numeric(sent_anxiety_prop),
  sent_net = as.numeric(sent_net)
)

df_out <- df %>%
  left_join(text_measures, by = "doc_id")

#-----------------------------
# 10. Save output
#-----------------------------
write_csv(df_out, "have_your_say_feedback_corpus_with_text_measures.csv")
