
# Packages

libs <- c(
  "tidyverse",
  "readr",
  "dplyr",
  "ggplot2",
  "forcats",
  "stringr",
  "scales",
  "patchwork",
  "broom",
  "knitr"
)

installed <- rownames(installed.packages())
for (p in libs) {
  if (!p %in% installed) install.packages(p)
}
invisible(lapply(libs, library, character.only = TRUE))


# 1. Paths
# ------------------------
data_path <- "have_your_say_feedback_corpus_with_text_measures.csv"

# Change this to your paper output folder.
output_dir <- "paper_outputs"
fig_dir <- file.path(output_dir, "figures")
tab_dir <- file.path(output_dir, "tables")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)


# 2. Read data
# ------------------------
df_raw <- read_csv(data_path, show_col_types = FALSE)


# 3. Data cleaning / harmonization
# ------------------------
df <- df_raw %>%
  mutate(
    group = coalesce(.data[["group.x"]], .data[["group.y"]]),
    group = case_when(
      group %in% c("pre2022", "Pre2022", "pre") ~ "Pre-LLM (<=2022)",
      group %in% c("post2024", "Post2024", "post") ~ "Post-LLM (>=2024)",
      TRUE ~ as.character(group)
    ),
    group = factor(group, levels = c("Pre-LLM (<=2022)", "Post-LLM (>=2024)")),
    initiative = as.character(initiative),
    feedback_text = as.character(feedback_text),
    end_year = suppressWarnings(as.numeric(end_year)),
    word_count = suppressWarnings(as.numeric(word_count)),
    fkgl = suppressWarnings(as.numeric(fkgl)),
    mattr = suppressWarnings(as.numeric(mattr)),
    sent_positive_prop = suppressWarnings(as.numeric(sent_positive_prop)),
    sent_negative_prop = suppressWarnings(as.numeric(sent_negative_prop)),
    sent_anxiety_prop = suppressWarnings(as.numeric(sent_anxiety_prop)),
    first_person_pronoun_prop = suppressWarnings(as.numeric(first_person_pronoun_prop))
  ) %>%
  filter(!is.na(group))

# Keep only rows with text.
df_text <- df %>%
  filter(!is.na(feedback_text), str_squish(feedback_text) != "") %>%
  mutate(
    log_word_count = log1p(word_count)
  )

# Reshape sentiment measures to long format.
df_sent_long <- df_text %>%
  select(group, sent_positive_prop, sent_negative_prop, sent_anxiety_prop) %>%
  pivot_longer(
    cols = c(sent_positive_prop, sent_negative_prop, sent_anxiety_prop),
    names_to = "sentiment_type",
    values_to = "value"
  ) %>%
  mutate(
    sentiment_type = recode(
      sentiment_type,
      sent_positive_prop = "Positive",
      sent_negative_prop = "Negative",
      sent_anxiety_prop = "Anxiety"
    ),
    sentiment_type = factor(sentiment_type, levels = c("Positive", "Negative", "Anxiety"))
  )


# 4. Theme and helpers
# ------------------------
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    )
)

save_plot <- function(plot_obj, filename, width = 8, height = 5, dpi = 320) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
}

extract_post_coef <- function(model, model_name) {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(term == "groupPost-LLM (>=2024)") %>%
    mutate(outcome = model_name)
}

# Set zoom ranges using the 5th to 95th percentile.
zoom_limits <- function(x, lower = 0.05, upper = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(NA, NA))
  quantile(x, probs = c(lower, upper), na.rm = TRUE)
}


zoom_word_count <- zoom_limits(df_text$word_count, 0.05, 0.95)
zoom_fkgl <- zoom_limits(df_text$fkgl, 0.05, 0.95)
zoom_mattr <- zoom_limits(df_text$mattr, 0.05, 0.95)
zoom_first_person <- zoom_limits(df_text$first_person_pronoun_prop, 0.05, 0.95)

zoom_sent <- df_sent_long %>%
  group_by(sentiment_type) %>%
  summarise(
    ymin = quantile(value, 0.05, na.rm = TRUE),
    ymax = quantile(value, 0.95, na.rm = TRUE),
    .groups = "drop"
  )


# 5. Table 1: Descriptive summary
# ------------------------
table1 <- df_text %>%
  group_by(group) %>%
  summarise(
    n_feedbacks = n(),
    n_initiatives = n_distinct(initiative),
    mean_word_count = mean(word_count, na.rm = TRUE),
    median_word_count = median(word_count, na.rm = TRUE),
    mean_fkgl = mean(fkgl, na.rm = TRUE),
    median_fkgl = median(fkgl, na.rm = TRUE),
    mean_mattr = mean(mattr, na.rm = TRUE),
    median_mattr = median(mattr, na.rm = TRUE),
    mean_positive = mean(sent_positive_prop, na.rm = TRUE),
    mean_negative = mean(sent_negative_prop, na.rm = TRUE),
    mean_anxiety = mean(sent_anxiety_prop, na.rm = TRUE),
    mean_first_person = mean(first_person_pronoun_prop, na.rm = TRUE),
    missing_fkgl = sum(is.na(fkgl)),
    missing_mattr = sum(is.na(mattr)),
    .groups = "drop"
  )

write_csv(table1, file.path(tab_dir, "table1_descriptive_summary.csv"))
cat(knitr::kable(table1, digits = 3), file = file.path(tab_dir, "table1_descriptive_summary.md"))


# ------------------------
# 6. Figure A1: Initiative composition (put in appendix)
# ------------------------
library(stringr)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(patchwork)

fig1_data <- df_text %>%
  count(group, initiative, name = "n_feedbacks") %>%
  group_by(group) %>%
  mutate(
    pct = n_feedbacks / sum(n_feedbacks),
    pct_label = percent(pct, accuracy = 0.1),
    initiative_wrap = str_wrap(initiative, width = 45)
  ) %>%
  ungroup()

pre_max <- max(fig1_data$n_feedbacks)

figA1_pre <- fig1_data %>%
  filter(group == "Pre-LLM (<=2022)") %>%
  mutate(initiative_wrap = fct_reorder(initiative_wrap, n_feedbacks)) %>%
  ggplot(aes(x = n_feedbacks, y = initiative_wrap)) +
  geom_col(width = 0.7, fill = "grey60") +
  geom_text(aes(label = n_feedbacks), hjust = -0.15, size = 3.5) +
  geom_text(
    aes(x = n_feedbacks + pre_max * 0.10, label = pct_label),
    hjust = 0, size = 3.5, color = "grey30"
  ) +
  expand_limits(x = pre_max * 1.28) +
  labs(
    title = "Pre-LLM initiatives",
    x = "Number of feedback texts",
    y = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 30, 10, 20)
  )
figA1_post <- fig1_data %>%
  filter(group == "Post-LLM (>=2024)") %>%
  mutate(initiative_wrap = fct_reorder(initiative_wrap, n_feedbacks)) %>%
  ggplot(aes(x = n_feedbacks, y = initiative_wrap)) +
  geom_col(width = 0.7, fill = "grey35") +
  geom_text(aes(label = n_feedbacks), hjust = -0.15, size = 3.5) +
  geom_text(
    aes(x = n_feedbacks + pre_max * 0.10, label = pct_label),
    hjust = 0, size = 3.5, color = "grey30"
  ) +
  expand_limits(x = pre_max * 1.28) +
  labs(
    title = "Post-LLM initiatives",
    x = "Number of feedback texts",
    y = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 30, 10, 20)
  )


figA1 <- figA1_pre / figA1_post +
  plot_annotation(
    title = "Appendix Figure A1. Corpus composition by initiative",
    subtitle = "Number and share of feedback texts per initiative in each era group"
  )

save_plot(figA1, "A1_initiative_composition.png", width = 12, height = 10)
# -----

# Pre group
figA1_pre <- fig1_data %>%
  filter(group == "Pre-LLM (<=2022)") %>%
  mutate(initiative_wrap = forcats::fct_reorder(initiative_wrap, n_feedbacks)) %>%
  ggplot(aes(x = n_feedbacks, y = initiative_wrap)) +
  geom_col(width = 0.7, fill = "grey60") +
  geom_text(aes(label = n_feedbacks), hjust = -0.15, size = 3.5) +
  expand_limits(x = max(fig1_data$n_feedbacks) * 1.12) +
  labs(
    title = "Pre-LLM initiatives",
    x = "Number of feedback texts",
    y = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 20, 10, 20)
  )

# Post group
figA1_post <- fig1_data %>%
  filter(group == "Post-LLM (>=2024)") %>%
  mutate(initiative_wrap = forcats::fct_reorder(initiative_wrap, n_feedbacks)) %>%
  ggplot(aes(x = n_feedbacks, y = initiative_wrap)) +
  geom_col(width = 0.7, fill = "grey35") +
  geom_text(aes(label = n_feedbacks), hjust = -0.15, size = 3.5) +
  expand_limits(x = max(fig1_data$n_feedbacks) * 1.12) +
  labs(
    title = "Post-LLM initiatives",
    x = "Number of feedback texts",
    y = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 20, 10, 20)
  )

figA1 <- fig1_pre / fig1_post +
  plot_annotation(
    title = "Appendix Figure A1. Corpus composition by initiative",
    subtitle = "Number of feedback texts per initiative in each era group"
  )

save_plot(figA1, "A1_initiative_composition.png", width = 12, height = 10)



# 7. Figure 1: Word count distribution (boxplot + zoomed y-axis)
# ------------------------
fig1 <- ggplot(df_text, aes(x = group, y = word_count, fill = group)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.2, show.legend = FALSE) +
  coord_cartesian(ylim = zoom_word_count) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Figure 1. Word count distribution by era",
    x = NULL,
    y = "Word count"
  )

save_plot(fig1, "fig1_wordcount_distribution_boxplot_zoomed.png", width = 8, height = 5)

# Appendix: log-scale version.
fig2_log <- ggplot(df_text, aes(x = group, y = word_count, fill = group)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.2, show.legend = FALSE) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Appendix Figure A1. Word count distribution by era (log scale)",
    subtitle = "Boxplots on a log-scaled y-axis",
    x = NULL,
    y = "Word count (log scale)"
  )

save_plot(fig2_log, "appendix_figureA1_wordcount_log_boxplot.png", width = 8, height = 5)


# 8. Figure 2: FKGL and MATTR (boxplots + zoomed y-axis)
# ------------------------
fig2a <- ggplot(df_text, aes(x = group, y = fkgl, fill = group)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.2, show.legend = FALSE) +
  coord_cartesian(ylim = zoom_fkgl) +
  labs(
    title = "Readability (FKGL)",
    subtitle = "Central distribution shown",
    x = NULL,
    y = "Flesch-Kincaid Grade Level"
  )

fig2b <- ggplot(df_text, aes(x = group, y = mattr, fill = group)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.2, show.legend = FALSE) +
  coord_cartesian(ylim = zoom_mattr) +
  labs(
    title = "Lexical diversity (MATTR)",
    subtitle = "Central distribution shown",
    x = NULL,
    y = "MATTR"
  )

fig2 <- fig2a + fig2b + plot_annotation(
  title = "Figure 2. Readability and lexical diversity by era"
)

save_plot(fig2, "fig2_fkgl_mattr_boxplot_zoomed.png", width = 12, height = 5)


# 9. Figure 3: Sentiment comparison (boxplots + zoomed y-axis)
# ------------------------
# Set separate y-axis ranges for each facet.
facet_limits <- zoom_sent %>%
  mutate(
    group = "Pre-LLM (<=2022)",
    x = 1
  ) %>%
  bind_rows(
    zoom_sent %>%
      mutate(
        group = "Post-LLM (>=2024)",
        x = 2
      )
  )

fig3 <- ggplot(df_sent_long, aes(x = group, y = value, fill = group)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.2, show.legend = FALSE) +
  facet_wrap(~ sentiment_type, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  ggh4x::facetted_pos_scales(
    y = list(
      sentiment_type == "Positive" ~ scale_y_continuous(
        limits = c(0, 0.40),
        breaks = c(0, 0.10, 0.20, 0.30),
        labels = percent_format(accuracy = 0.1)
      ),
      sentiment_type == "Negative" ~ scale_y_continuous(
        limits = c(0, 0.52),
        breaks = c(0, 0.10, 0.20, 0.30, 0.40, 0.50),
        labels = percent_format(accuracy = 0.1)
      ),
      sentiment_type == "Anxiety" ~ scale_y_continuous(
        limits = c(0, 0.26),
        breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25),
        labels = percent_format(accuracy = 0.1)
      )
    )
  ) +
  labs(
    title = "Figure 3. Sentiment profile by era",
    subtitle = "Boxplots with y-axes focused on the central distribution within each sentiment type",
    x = NULL,
    y = "Proportion"
  )
save_plot(fig3, "fig3_sentiment_comparison_boxplot_zoomed.png", width = 12, height = 5)


# 10. Figure 4: First-person pronoun usage (boxplot + zoomed y-axis)
# ------------------------
fig4 <- ggplot(df_text, aes(x = group, y = first_person_pronoun_prop, fill = group)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.2, show.legend = FALSE) +
  coord_cartesian(ylim = zoom_first_person) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Figure 5. First-person pronoun usage by era",
    subtitle = "Boxplots with y-axis focused on the central distribution (5th-95th percentile)",
    x = NULL,
    y = "Proportion"
  )

save_plot(fig4, "fig4_firstperson_pronouns_boxplot_zoomed.png", width = 8, height = 5)

# ------------------------
# 11. Figure 5: Time trends
# ------------------------
time_data <- df_text %>%
  filter(!is.na(end_year)) %>%
  group_by(end_year) %>%
  summarise(
    mean_word_count = mean(word_count, na.rm = TRUE),
    mean_fkgl = mean(fkgl, na.rm = TRUE),
    mean_mattr = mean(mattr, na.rm = TRUE),
    mean_first_person = mean(first_person_pronoun_prop, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(mean_word_count, mean_fkgl, mean_mattr, mean_first_person),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(
      measure,
      mean_word_count = "Word count",
      mean_fkgl = "FKGL",
      mean_mattr = "MATTR",
      mean_first_person = "First-person pronoun proportion"
    )
  )

fig5 <- ggplot(time_data, aes(x = end_year, y = value)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2023, linetype = "dashed") +
  facet_wrap(~ measure, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = sort(unique(time_data$end_year))) +
  labs(
    title = "Figure 5. Time trends in average text features",
    subtitle = "Average text-based measures by consultation end year; dashed line marks 2023",
    x = "Consultation end year",
    y = "Average value"
  )

save_plot(fig5, "fig5_time_trends.png", width = 12, height = 7)

# 12. Figure 6: Coefficient plot
# ------------------------
m1 <- lm(log1p(word_count) ~ group, data = df_text)
m2 <- lm(fkgl ~ group, data = df_text)
m3 <- lm(mattr ~ group, data = df_text)
m4 <- lm(sent_positive_prop ~ group, data = df_text)
m5 <- lm(sent_negative_prop ~ group, data = df_text)
m6 <- lm(sent_anxiety_prop ~ group, data = df_text)
m7 <- lm(first_person_pronoun_prop ~ group, data = df_text)

coef_df <- bind_rows(
  extract_post_coef(m1, "log(Word count + 1)"),
  extract_post_coef(m2, "FKGL"),
  extract_post_coef(m3, "MATTR"),
  extract_post_coef(m4, "Positive proportion"),
  extract_post_coef(m5, "Negative proportion"),
  extract_post_coef(m6, "Anxiety proportion"),
  extract_post_coef(m7, "First-person pronoun proportion")
) %>%
  mutate(outcome = factor(
    outcome,
    levels = rev(c(
      "log(Word count + 1)",
      "FKGL",
      "MATTR",
      "Positive proportion",
      "Negative proportion",
      "Anxiety proportion",
      "First-person pronoun proportion"
    ))
  ))

fig6 <- ggplot(coef_df, aes(x = estimate, y = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  labs(
    title = "Figure 6. Descriptive coefficient plot for post-LLM indicator",
    subtitle = "Coefficients from separate bivariate regressions with reference group pre-LLM",
    x = "Coefficient estimate for Post-LLM group",
    y = NULL
  )

save_plot(fig6, "fig6_coefficient_plot.png", width = 9, height = 6)

# 13. Optional appendix plots A2
# ------------------------
appendix_density_data <- df_text %>%
  select(group, word_count, fkgl, mattr, first_person_pronoun_prop) %>%
  pivot_longer(
    cols = c(word_count, fkgl, mattr, first_person_pronoun_prop),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(
      measure,
      word_count = "Word count",
      fkgl = "FKGL",
      mattr = "MATTR",
      first_person_pronoun_prop = "First-person pronoun proportion"
    )
  )

figA2 <- ggplot(appendix_density_data, aes(x = value, color = group, fill = group)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ measure, scales = "free", ncol = 2) +
  labs(
    title = "Appendix Figure A2. Density plots of core text measures",
    x = NULL,
    y = "Density"
  )

save_plot(figA2, "A2_density_core_measures.png", width = 12, height = 7)


# 14. Export model table
# ------------------------
model_table <- coef_df %>%
  select(outcome, estimate, conf.low, conf.high)

write_csv(model_table, file.path(tab_dir, "table2_model_coefficients.csv"))


# 15. Console summary
# ------------------------
message("Done.")
message("Outputs saved to: ", normalizePath(output_dir))
message("Figures saved in: ", normalizePath(fig_dir))
message("Tables saved in: ", normalizePath(tab_dir))
