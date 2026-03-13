##########################################
#### Figure 3 - Case study barcharts  ####
##########################################

# Generates combined barchart figure for case study data
# All 8 scenarios appear in both gen_time and inc_period barcharts

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "lshtm_theme.R"))
source(here("R", "plot_casestudy.R"))
source(here("scripts", "datacollect_casestudy.R"))

diseases <- c("covid", "ebola", "cholera")
disease_labels <- c(covid = "COVID-19 in England",
                    ebola = "Ebola in Sierra Leone",
                    cholera = "Cholera in Yemen")
data_freq_map <- c(covid = "day", ebola = "day", cholera = "week")

# Weight prior variants to load
wp_variants <- expand.grid(
  weight = c("TRUE", "FALSE"),
  vary = c("both", "gt", "inc"),
  stringsAsFactors = FALSE
)

# Pretty labels and ordering for all 8 scenarios
pretty_map <- c(
  "no delay"          = "No delay",
  "correct"           = "From the literature",
  "wp_gt_true"        = "From the data,\nGT only (weighting)",
  "wp_gt_false"       = "From the data,\nGT only (no weighting)",
  "wp_inc_true"       = "From the data,\nINC only (weighting)",
  "wp_inc_false"      = "From the data,\nINC only (no weighting)",
  "wp_both_true"      = "From the data,\nboth (weighting)",
  "wp_both_false"     = "From the data,\nboth (no weighting)"
)

order_levels <- names(pretty_map)

# Store barcharts per disease
barcharts <- list()

for (disease in diseases) {
  message(paste("=== Processing", disease, "==="))

  ## Load case data
  case_data <- casestudydata[[disease]]
  if ("confirm" %in% names(case_data)) {
    sim_data <- case_data |>
      mutate(variable = "reported_cases") |>
      rename(value = confirm)
  } else {
    sim_data <- case_data |>
      mutate(variable = "reported_cases") |>
      rename(value = cases)
  }

  freq <- data_freq_map[disease]

  ## Load casestudy results (no delay + correct only)
  res_samples_cs <- read_latest(here("results/casestudy"),
    paste0("res_", disease, "_casestudy_latest_all_samples"))
  res_id_cs <- read_latest(here("results/casestudy"),
    paste0("res_", disease, "_casestudy_latest_all_id"))

  res_samples_correct <- res_samples_cs |>
    left_join(res_id_cs, by = c("result_list", "gt", "inc")) |>
    filter(inc_period %in% c("no delay", "correct"),
           gen_time %in% c("no delay", "correct"))

  ## Load all weight prior variants
  wp_samples_list <- list()
  for (i in seq_len(nrow(wp_variants))) {
    w <- wp_variants$weight[i]
    v <- wp_variants$vary[i]
    label <- paste0("wp_", v, "_", tolower(w))

    wp_samp <- tryCatch(
      read_latest(here("results/weightprior"),
        paste0("res_", disease, "_weightprior_", w, "_", v, "_latest_all_samples")),
      error = function(e) {
        message(paste("  Missing:", disease, w, v))
        NULL
      }
    )

    if (!is.null(wp_samp)) {
      wp_samp <- wp_samp |>
        rename(timepoint = result_list) |>
        mutate(gen_time = label,
               inc_period = label)

      wp_samples_list[[label]] <- wp_samp
    }
  }

  ## Prepare casestudy baseline samples
  res_baseline <- res_samples_correct |>
    select(-any_of(c("result_list", "gt", "inc")))

  ## Combine all
  wp_combined <- bind_rows(wp_samples_list) |>
    select(-any_of(c("gt", "inc", "vary")))

  res_all <- bind_rows(res_baseline, wp_combined) |>
    filter(type == "forecast")

  ## Trim to 8 timepoints
  res_all <- res_all |> filter(timepoint <= 8)

  ## Weekly aggregation for cholera
  if (freq == "week") {
    res_all <- res_all |>
      mutate(
        week = sim_data$date[findInterval(date, sim_data$date, left.open = TRUE) + 1]
      ) |>
      group_by(week, gen_time, inc_period, timepoint, type, model, sample) |>
      summarise(prediction = sum(prediction), .groups = "drop") |>
      rename(date = week)
  }

  ## Score: join with observed data
  res_end <- res_all |>
    group_by(timepoint, gen_time, inc_period) |>
    filter(date == max(date)) |>
    ungroup()

  res_end <- sim_data |>
    filter(variable == "reported_cases") |>
    rename(true_value = value) |>
    select(-variable) |>
    right_join(res_end, by = "date") |>
    filter(!is.na(true_value))

  ## Create forecast object, log-transform, score
  res_end_score <- res_end |>
    select(date, true_value, prediction, sample, model,
           gen_time, inc_period, timepoint, type)

  fc <- as_forecast_sample(
    data = res_end_score,
    forecast_unit = c("date", "type", "gen_time", "inc_period", "timepoint"),
    observed = "true_value",
    predicted = "prediction",
    sample_id = "sample"
  )
  fc <- transform_forecasts(fc, fun = log_shift, offset = 1, label = "log")
  scores <- fc |> filter(scale == "log") |> score()

  scores_long <- scores |>
    select(date, type, gen_time, inc_period, timepoint, scale,
           overprediction, underprediction, dispersion) |>
    pivot_longer(cols = c(overprediction, underprediction, dispersion),
                 names_to = "measure", values_to = "value")

  mean_gt <- scores_long |>
    group_by(gen_time, measure) |>
    summarise(value = mean(value), .groups = "drop") |>
    mutate(gen_time = factor(gen_time, levels = order_levels))

  mean_inc <- scores_long |>
    group_by(inc_period, measure) |>
    summarise(value = mean(value), .groups = "drop") |>
    mutate(inc_period = factor(inc_period, levels = order_levels))

  ## Create barcharts
  label <- disease_labels[disease]
  title_theme <- theme(plot.title = element_text(hjust = 0.5, size = 14))

  barcharts[[paste0("gt_", disease)]] <- ggplot(mean_gt) +
    geom_bar(aes(x = gen_time, y = value, fill = measure), stat = "identity") +
    xlab("Generation time") + ylab("CRPS") +
    ggtitle(label) + title_theme +
    lshtm_theme() +
    scale_fill_manual(values = lshtm_pal) +
    scale_x_discrete(labels = pretty_map) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none", axis.title.y = element_blank())

  barcharts[[paste0("inc_", disease)]] <- ggplot(mean_inc) +
    geom_bar(aes(x = inc_period, y = value, fill = measure), stat = "identity") +
    xlab("Incubation period") + ylab("CRPS") +
    ggtitle(label) + title_theme +
    lshtm_theme() +
    scale_fill_manual(values = lshtm_pal) +
    scale_x_discrete(labels = pretty_map) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none", axis.title.y = element_blank())

  rm(res_samples_cs, res_id_cs, res_samples_correct, wp_samples_list,
     res_baseline, wp_combined, res_all, res_end); gc()
}

#########################################
#### Combine into grid figure        ####
#########################################

# Shared legend
legend_bar <- cowplot::get_legend(
  ggplot(data.frame(x = rep("a", 3), y = c(1, 1, 1),
    measure = factor(c("dispersion", "overprediction", "underprediction"),
                     levels = c("dispersion", "overprediction", "underprediction"))),
    aes(x = x, y = y, fill = measure)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = lshtm_pal, name = "Measure") +
    theme(legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
)

# Remove titles from sub-plots (will use row/col headers)
no_title <- theme(plot.title = element_blank())

# 3 rows (diseases) x 2 cols (gen_time, inc_period)
plot_grid_body <- cowplot::plot_grid(
  barcharts$gt_covid    + no_title, barcharts$inc_covid    + no_title,
  barcharts$gt_ebola    + no_title, barcharts$inc_ebola    + no_title,
  barcharts$gt_cholera  + no_title, barcharts$inc_cholera  + no_title,
  ncol = 2
)

# Row labels
row_labels <- cowplot::plot_grid(
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "COVID-19\nin England",
                       size = 5, fontface = "bold") + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Ebola in\nSierra Leone",
                       size = 5, fontface = "bold") + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cholera\nin Yemen",
                       size = 5, fontface = "bold") + theme_void(),
  ncol = 1
)

body_with_rows <- cowplot::plot_grid(
  row_labels, plot_grid_body,
  ncol = 2, rel_widths = c(0.1, 1)
)

# Top headers
top_headers <- cowplot::plot_grid(
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Generation time",
                       size = 6, fontface = "bold") + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Incubation period",
                       size = 6, fontface = "bold") + theme_void(),
  ncol = 2
)

top_headers_padded <- cowplot::plot_grid(
  ggplot() + theme_void(), top_headers,
  ncol = 2, rel_widths = c(0.1, 1)
)

main_plot <- cowplot::plot_grid(
  top_headers_padded, body_with_rows,
  ncol = 1, rel_heights = c(0.04, 1)
)

combined_barcharts <- cowplot::plot_grid(
  main_plot, legend_bar,
  ncol = 2, rel_widths = c(1, 0.12)
)

ggsave(here("figures", "fig3_casestudy_combined_barcharts.png"),
       combined_barcharts, width = 16, height = 12)

message("\n=== Figure 3 casestudy generation complete ===\n")
