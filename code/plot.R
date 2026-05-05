
# ── Shared theme ─────────────────────────────────────────────────────────────
base_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 2)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey50", margin = margin(b = 10)),
      axis.title = element_text(size = 9,  color = "grey35"),
      axis.text = element_text(size = 8,  color = "grey45"),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 9, face = "bold", color = "grey20", margin = margin(b = 4)),
      strip.background = element_rect(fill = "grey96", color = NA),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.8, "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 16, 10, 16)
    )
}

# ── Shared palettes ──────────────────────────────────────────────────────────
PAL_CLASS_NIR <- c(
  "very good/good" = "#2E5D8E",
  "impending spoilage/spoiled" = "#C0392B"
  )

ggplot_graphical_comparation <- function(imputed_data, imputed_pos, x, y,
  x_ignore = FALSE, y_ignore = FALSE, dataset_id = NULL, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(imputed_data, types = "numeric")
  assertInteger(imputed_pos, lower = 1, any.missing = FALSE)
  assertString(x)
  assertString(y)
  assertSubset(x, colnames(imputed_data))
  assertSubset(y, colnames(imputed_data))
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Palette & tipo ──────────────────────────────────────────────────────
  col_real <- "#2E5D8E"
  col_imputed <- "#C0392B"

  imputed_data$type <- factor(
    ifelse(seq_len(nrow(imputed_data)) %in% imputed_pos, "Imputed", "Real"),
    levels = c("Real", "Imputed")
  )

  # ── 3. MSE & KS test ───────────────────────────────────────────────────────
  # Fit linear model on real data (x ~ y), predict on imputed, compute MSE
  real_data <- imputed_data[type == "Real"]
  imputed_rows <- imputed_data[type == "Imputed"]

  lm_real <- lm(as.formula(paste(y, "~", x)), data = real_data)
  pred_on_imputed <- predict(lm_real, newdata = imputed_rows)
  mse_val <- mean((imputed_rows[[y]] - pred_on_imputed)^2)

  # KS test: compare distribution of y between real and imputed
  ks_result <- ks.test(real_data[[y]], imputed_rows[[y]])
  ks_d <- round(ks_result$statistic, 4)
  ks_p <- round(ks_result$p.value,   4)

  # Format annotation string
  stats_label <- paste0(
    "MSE (imputed vs lm): ", round(mse_val, 5), "\n",
    "KS D = ", ks_d, " p = ", ks_p
  )

  # ── 4. Theme ───────────────────────────────────────────────────────────────
  col_scale_point <- scale_color_manual(
    values = c("Real" = col_real, "Imputed" = col_imputed)
  )

  col_scale_fill <- scale_fill_manual(
    values = c("Real" = col_real, "Imputed" = col_imputed)
  )

  # ── 5. Scatter plot ────────────────────────────────────────────────────────
  n_imputed <- sum(imputed_data$type == "Imputed")
  n_real <- sum(imputed_data$type == "Real")

  p1 <- ggplot(imputed_data, aes(x = .data[[x]], y = .data[[y]], color = type)) +
    geom_point(data = imputed_data[type == "Real"], alpha = 0.5, size = 1.8) +
    geom_point(data = imputed_data[type == "Imputed"], alpha = 0.9, size = 2.2, shape = 18) +
    geom_smooth(method = "gam", se = TRUE, color = "grey30", fill = "grey85", linewidth = 0.7) +
    col_scale_point +
    annotate("text", x = -Inf, y = Inf, label = stats_label, hjust = -0.1, vjust = 1.5,
      size = 2.8, color = "grey30", family = "mono", lineheight = 1.3) +
    labs(title = paste0(x, " vs ", y), x = x, y = y) +
    base_theme()

  # ── 6. Density plot ────────────────────────────────────────────────────────
  make_density <- function(variable, show_legend = FALSE) {
    ks_ann <- if (variable == y) {
      list(
        annotate("text", x = Inf, y = Inf, label = paste0("KS D = ", ks_d, "\np = ", ks_p),
          hjust = 1.05, vjust = 1.5, size = 2.8, color = "grey30", family = "mono", lineheight = 1.3
        )
      )
    } else {
      NULL
    }

    p <- ggplot(imputed_data, aes(x = .data[[variable]], fill = type, color = type)) +
      geom_density(alpha = 0.18, linewidth = 0.8) +
      col_scale_fill +
      col_scale_point +
      labs(
        title = paste0("Distribution of ", variable),
        x = variable, y = "Density",
        fill = NULL, color = NULL
      ) +
      base_theme()

    if (!is.null(ks_ann)) {
      p <- p + ks_ann
      p
    }
  }

  p2 <- if (!x_ignore) make_density(x) else NULL
  p3 <- if (!y_ignore) make_density(y) else NULL

  # ── 7. Layout ──────────────────────────────────────────────────────────────
  legend_patch <- ggplot(imputed_data, aes(x = .data[[x]], fill = type, color = type)) +
    geom_density(alpha = 0.18, linewidth = 0.8) +
    col_scale_fill +
    col_scale_point +
    labs(fill = NULL, color = NULL) +
    theme_void() +
    theme(
      legend.position  = "top",
      legend.direction = "horizontal",
      legend.text = element_text(size = 10, color = "grey20"),
      legend.key.size = unit(0.8, "lines"),
      plot.background = element_rect(fill = "white", color = NA)
    )

  legend <- get_legend(legend_patch)

  bottom <- if (!x_ignore & !y_ignore) {
    p2 | p3
  } else if (x_ignore) {
    p3
  } else {
    p2
  }

  plot <- wrap_elements(legend) / (p1 / bottom) +
    plot_layout(heights = c(0.04, 1)) +
    plot_annotation(
      title = paste0("Imputation diagnostics — dataset ", dataset_id),
      subtitle = paste0(
        "n = ", format(nrow(imputed_data), big.mark = ","),
        " | imputed = ", n_imputed,
        " (", round(100 * n_imputed / nrow(imputed_data), 1), "%)",
        " | MSE = ", round(mse_val, 5),
        " KS D = ", ks_d, " p = ", ks_p
      ),
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 2)),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey50", margin = margin(b = 8)),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  # ── 8. Save ────────────────────────────────────────────────────────────────
  if (save) {
    if (is.null(dataset_id)) dataset_id <- format(Sys.time(), "%H%M%S")
    filename <- paste0("imputation_", dataset_id, "_", paste0(strsplit(y, " ")[[1]], collapse = ""), ".png")

    ggsave(filename = file.path(out_dir, filename), plot = plot, width = 12, height = 10, dpi = 300)
    message("Saved: ", filename)
  }

  # ── 9. Return plot + stats invisibly ───────────────────────────────────────
  invisible(
    list(
      plot = plot,
      mse = mse_val,
      ks_stat   = ks_d,
      ks_pvalue = ks_p
    )
  )
}

ggplot_spectral_comparison <- function(raw_data, smoothed_data, n_samples = 6,
  out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(raw_data)
  assertDataTable(smoothed_data)
  assertCount(n_samples, positive = TRUE)
  assertString(out_dir)
  assertFlag(save)

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Random sample ───────────────────────────────────────────────────────
  set.seed(123)
  sample_ids <- sample(seq_len(nrow(raw_data)), n_samples)

  # ── 3. prepare_long (pure data.table) ──────────────────────────────────────
  prepare_long <- function(dt, type_label) {
    dt[, sampleID := as.factor(seq_len(.N))]
    dt <- dt[sampleID %in% sample_ids]
    dt_long <- melt(dt, id.vars = "sampleID", variable.name = "wavelength", value.name = "intensity")

    dt_long[, `:=`(
      wavelength = as.numeric(str_extract(as.character(wavelength), "[0-9]+")),
      type = type_label
    )]

    dt_long[]
  }

  # ── 4. Combine and factor ──────────────────────────────────────────────────
  dt_plot <- rbindlist(list(
    prepare_long(raw_data, "raw"),
    prepare_long(smoothed_data, "smoothed")
  ))

  dt_plot[, type := factor(type, levels = c("raw", "smoothed"))]

  # ── 5. Plot ────────────────────────────────────────────────────────────────
  p <- ggplot(dt_plot, aes(x = wavelength, y = intensity, color = type, alpha = type)) +
    geom_line(aes(group = interaction(sampleID, type)), linewidth = 0.7) +
    facet_wrap(~ sampleID, scales = "free_y", labeller = label_both) +
    scale_color_manual(values = c("#2E5D8E", "#C0392B"), labels = c("Raw", "Smoothed / SNV")) +
    scale_alpha_manual(values = c("raw" = 0.35, "smoothed" = 1), labels = c("Raw", "Smoothed / SNV")) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.2)), alpha = "none") +
    labs(
      title = "Spectral Smoothing / SNV Comparison",
      subtitle = paste("Showing", n_samples, "randomly selected spectra"),
      x = "Wavelength / wavenumber",
      y = "Absorbance / intensity"
    ) +
    base_theme()

  # ── 6. Save ────────────────────────────────────────────────────────────────
  if (save) {
    ggsave(filename = file.path(out_dir, "spectral_smoothing_comparison.png"), plot = p, width = 14, height = 8, dpi = 300)
    message("Saved: spectral_smoothing_comparison.png")
  }

  invisible(p)
}

ggplot_permutation_test <- function(perm_results_list, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertList(perm_results_list)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  plots <- lapply(perm_results_list, function(res) {
    dt_perm <- data.table(bacc = res$perm_bacc)
    obs <- res$summary$obs_bacc
    p_val <- res$summary$p_value
    label <- res$summary$model

    pct_obs <- round(mean(res$perm_bacc < obs) * 100, 1)

    # ── 2. Plot ──────────────────────────────────────────────────────────────
    ggplot(dt_perm, aes(x = bacc)) +
      geom_histogram(bins = 30, fill = "#AED6F1", color = "white", linewidth = 0.3) +
      geom_vline(xintercept = obs, color = "#C0392B", linewidth = 1.2, linetype = "dashed") +
      annotate("label", x = obs, y = Inf, label = sprintf("Observed\nBAcc = %.3f\np = %.3f", obs, p_val),
        vjust = 1.4, hjust = ifelse(obs > 0.55, 1.1, -0.1), size = 3, color = "#C0392B", fill = "white",
        fontface = "italic", label.size = 0.3) +
      labs(
        title = label,
        subtitle = sprintf("Permutation distribution (n = %d) | %.1f%% of permutations < observed", length(res$perm_bacc), pct_obs),
        x = "Balanced Accuracy (permuted)",
        y = "Count"
      ) +
      base_theme() +
      theme(legend.position = "none")
  })

  combined <- wrap_plots(plots, nrow = 1) +
    plot_annotation(
      title = "Permutation Tests — NIR Classification",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 6)),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  if (save) {
    ggsave(file.path(out_dir, "permutation_tests.png"), plot = combined, width = 6 * length(perm_results_list), height = 5, dpi = 300)
    message("Saved: permutation_tests.png")
  }

  invisible(combined)
}

ggplot_performance_summary <- function(results, perm_IMP = NULL, perm_REAL = NULL,
  out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(results)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  dt <- results[
    results[, .I[which.max(bacc)],
  by = .(dataset, learner_base = gsub("_auc|_anova|_mrmr", "", learner))]$V1
  ][, learner_base := gsub("_auc|_anova|_mrmr", "", learner)]

  model_order <- dt[, .(mean_bacc = mean(bacc)), by = learner_base][
    order(mean_bacc), learner_base]
  dt[, learner_base := factor(learner_base, levels = model_order)]

  dt[, dataset_label := factor(
    ifelse(dataset == "IMP", "Imputed dataset (K ≤ 0.5, n = 49)", "Complete cases (K ≤ 0.4, n = 30)"),
    levels = c("Imputed dataset (K ≤ 0.5, n = 49)", "Complete cases (K ≤ 0.4, n = 30)")
  )]

  dt[, perm_sig := FALSE]

  if (!is.null(perm_IMP)) {
    sig_lrn <- gsub("_auc|_anova|_mrmr", "", perm_IMP$summary$model)
    if (perm_IMP$summary$p_value < 0.05)
      dt[dataset == "IMP" & learner_base == sig_lrn, perm_sig := TRUE]
  }

  if (!is.null(perm_REAL)) {
    sig_lrn <- gsub("_auc|_anova|_mrmr", "", perm_REAL$summary$model)
    if (perm_REAL$summary$p_value < 0.05)
      dt[dataset == "REAL" & learner_base == sig_lrn, perm_sig := TRUE]
  }

  pal <- c(
    "Balanced Acc." = "#2E5D8E",
    "AUC" = "#27AE60",
    "Sensitivity" = "#E67E22",
    "Specificity" = "#8E44AD"
  )

  size_map  <- c("Balanced Acc." = 4.5, "AUC" = 3.5, "Sensitivity" = 2.5, "Specificity" = 2.5)
  shape_map <- c("Balanced Acc." = 19, "AUC" = 18, "Sensitivity" = 17, "Specificity" = 25)

  dt_long <- melt(dt,
    id.vars = c("dataset_label", "learner_base", "perm_sig"),
    measure.vars = c("bacc", "auc", "sens", "spec"),
    variable.name = "metric", value.name = "value")

  dt_long[, metric := factor(metric,
   levels = c("bacc", "auc", "sens", "spec"), 
   labels = c("Balanced Acc.", "AUC", "Sensitivity", "Specificity"))]

  # ── 2. Plot ────────────────────────────────────────────────────────────────
  p <- ggplot() +
    geom_vline(xintercept = 0.5, color = "grey65", linetype = "dashed", linewidth = 0.6) +
    geom_hline(data = dt_long[metric == "Balanced Acc."], aes(yintercept = as.numeric(learner_base)), color = "grey92", linewidth = 0.4) +
    geom_point(
      data = dt_long[metric %in% c("Sensitivity", "Specificity")],
      aes(x = value, y = learner_base, color = metric, shape = metric, size = metric), alpha = 0.75) +
    geom_point(
      data = dt_long[metric == "AUC"],
      aes(x = value, y = learner_base, color = metric, shape = metric, size = metric), alpha = 0.9) +
    geom_point(
      data = dt_long[metric == "Balanced Acc."],
      aes(x = value, y = learner_base, color = metric, shape = metric, size = metric), alpha = 1) +
    geom_text(
      data = dt[perm_sig == TRUE],
      aes(x = bacc + 0.045, y = learner_base, label = "*"), color = "#C0392B", size = 4.5, vjust = 0.4) +
    annotate("text", x = 0.502, y = 0.55, label = "chance", color = "grey55",
      size = 2.6, hjust = 0, fontface = "italic") +
    facet_wrap(~dataset_label) +
    scale_color_manual(values = pal, name = NULL) +
    scale_shape_manual(values = shape_map, name = NULL) +
    scale_size_manual(values = size_map, name = NULL) +
    scale_x_continuous(limits = c(0.2, 1.02), breaks = seq(0.2, 0.8, 0.2)) +
    guides(
      color = guide_legend(override.aes = list(size = 3.5), nrow = 1),
      shape = guide_legend(override.aes = list(size = 3.5), nrow = 1),
      size = "none"
    ) +
    labs(
      title = "Classification Performance: All Classifiers",
      subtitle = paste0(
        "Best FS criterion per classifier | ",
        if (any(dt$perm_sig)) "* = permutation test p < 0.05 | " else "",
        "Dashed = chance level (BAcc = 0.5)"
      ),
      x = "Metric value",
      y = NULL
    ) +
    base_theme() +
    theme(
      legend.position = "top",
      legend.key.size = unit(0.4, "cm"),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.y = element_blank()
    )

  if (save) {
    ggsave(file.path(out_dir, "performance_summary.png"), plot = p, width = 13, height = 7, dpi = 300)
    message("Saved: performance_summary.png")
  }

  invisible(p)
}

ggplot_sensitivity_confint <- function(summary, metric_filter = "bacc", out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(summary)
  assertString(metric_filter)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  dt <- summary[metric == metric_filter]
  setorder(dt, -mean)
  dt[, learner := factor(learner, levels = rev(learner))]

  metric_labels <- c(
    bacc = "Balanced Accuracy",
    auc  = "AUC",
    sens = "Sensitivity",
    spec = "Specificity",
    acc  = "Accuracy"
  )

  # ── 2. Plot ────────────────────────────────────────────────────────────────
  p <- ggplot(dt, aes(x = mean, y = learner)) +
    geom_vline(xintercept = 0.5, color = "grey70", linetype = "dashed", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_up), height = 0.3, color = "#2E5D8E", linewidth = 0.8, alpha = 0.7) +
    geom_point(aes(size = 1/sd), color = "#2E5D8E", alpha = 0.9, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.3f ± %.3f", mean, sd)), hjust = -0.2, vjust = -2, size = 3, color = "grey35") +
    scale_x_continuous(
      limits = c(0.3, 0.85),
      breaks = seq(0.3, 0.8, 0.1)
    ) +
    labs(
      title = paste0("Sensitivity Analysis — ", metric_labels[metric_filter]),
      subtitle = "Mean ± SD across 10 imputed datasets (95% CI shown)",
      x = metric_labels[metric_filter],
      y = NULL
    ) +
    base_theme() +
    theme(
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3)
    )

  if (save) {
    fname <- paste0("sensitivity_ci_", metric_filter, ".png")
    ggsave(file.path(out_dir, fname), plot = p, width = 10, height = 6, dpi = 300)
    message("Saved: ", fname)
  }

  invisible(p)
}

ggplot_band_frequency <- function(bands, top_n = 5, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(bands)
  assertNumber(top_n, lower = 1)
  assertString(out_dir)
  assertFlag(save)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  dt <- bands[, .SD[order(-pct)][seq_len(min(.N, top_n))], by = .(dataset, learner)]
  dt[, nm := as.numeric(gsub("W", "", as.character(banda)))]
  dt[, banda_label := paste0(nm, " nm")]

  dt[, banda_label := factor(banda_label, levels = unique(banda_label[order(nm)]))]

  dt[, dataset_label := factor(
    ifelse(dataset == "IMP", "Imputed dataset (K ≤ 0.5, n = 49)", "Complete cases (K ≤ 0.4, n = 30)"),
    levels = c("Imputed dataset (K ≤ 0.5, n = 49)", "Complete cases (K ≤ 0.4, n = 30)")
  )
  ]

  dt[, region := fcase(
    nm >= 1440 & nm <= 1490, "1440–1490 nm (O-H/N-H)",
    nm >= 1520 & nm <= 1560, "1520–1560 nm (protein N-H)",
    nm >= 1630 & nm <= 1700, "1630–1700 nm (C=O lipids)",
    nm >= 1750 & nm <= 1810, "1750–1810 nm (C-H fatty acids)",
    nm >= 2130 & nm <= 2310, "2130–2310 nm (C-H/C-O esters)",
    default = "Other"
  )]

  pal_region <- c(
    "1440–1490 nm (O-H/N-H)" = "#2E5D8E",
    "1520–1560 nm (protein N-H)" = "#1A8A6E",
    "1630–1700 nm (C=O lipids)" = "#C0392B",
    "1750–1810 nm (C-H fatty acids)" = "#D4A017",
    "2130–2310 nm (C-H/C-O esters)"  = "#7D3C98",
    "Other" = "#AAB7B8"
  )

  p <- ggplot(dt, aes(x = pct, y = reorder(banda_label, nm), fill = region)) +
    geom_col(alpha = 0.88, width = 0.7) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 2.8, color = "grey30") +
    facet_grid(learner ~ dataset_label, scales = "free_y", space = "free_y") +
    scale_fill_manual(values = pal_region, name = "Spectral region", guide = guide_legend(ncol = 1)) +
    scale_x_continuous(limits = c(0, max(dt$pct) * 1.25), breaks = seq(0, 40, 10), labels = function(x) paste0(x, "%")) +
    labs(
      title = paste0("Top ", top_n, " Selected Wavelengths per Classifier"),
      subtitle = "Selection frequency across CV folds (best FS criterion per classifier)",
      x = "Selection frequency (%)",
      y = "Wavelength"
    ) +
    base_theme() +
    theme(
      strip.text.x = element_text(size = 9, face = "bold"),
      strip.text.y = element_text(size = 8, angle = 0, hjust = 0),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      axis.text.y = element_text(size = 7)
    )

  if (save) {
    ggsave(file.path(out_dir, "band_frequency.png"), plot = p, width  = 14, height = max(8, nrow(unique(dt[, .(learner)])) * 1.8), dpi = 300)
    message("Saved: band_frequency.png")
  }

  invisible(p)
}

ggplot_lmm_diagnostics <- function(obj, group_var = NULL, xvar = NULL,
  model_name = "lmm", out_dir = "plot/", save = TRUE, summaries = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertClass(obj, classes = "lmerMod")
  assertString(out_dir)
  assertFlag(save)
  assertFlag(summaries)

  if (save && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ── 2. Residui e quantità diagnostiche ─────────────────────────────────────
  res <- as.numeric(residuals(obj, type = "pearson"))
  fitted_v <- as.numeric(fitted(obj))
  n <- length(res)

  if (is.null(xvar)) {
    xvar_vals <- seq_along(res)
    xlab <- "Index"
  } else {
    xvar_vals <- xvar
    xlab <- deparse(substitute(xvar))
  }

  diag_dt <- data.table(
    residuals = res,
    fitted = fitted_v,
    xvar = xvar_vals,
    obs = seq_len(n)
  )

  diag_dt <- diag_dt[is.finite(residuals)]
  n <- nrow(diag_dt)

  # ── 3. Parametri adattativi ────────────────────────────────────────────────
  pt_size <- ifelse(n > 50000, 0.3, ifelse(n > 10000, 0.6, 1.5))
  pt_alpha <- ifelse(n > 50000, 0.08, ifelse(n > 10000, 0.2, 0.6))
  loess_span <- ifelse(n > 10000, 0.3, 0.75)
  outlier_idx <- abs(diag_dt$residuals) > 2.5

  # ── 4. Palette e tema ──────────────────────────────────────────────────────
  pal <- c(
    p1 = "#3D5A80", p2 = "#5C7FA3",
    p3 = "#2E8B57", p4 = "#6B8F71",
    p5 = "#8B6914", p6 = "#B8965A"
  )
  accent <- "#9B1B30"

  # ── 5. Layer riutilizzabili ────────────────────────────────────────────────
  smooth_layer <- function(x_var, y_var)
    geom_smooth(aes(x = .data[[x_var]], y = .data[[y_var]]), method = "loess", 
      formula = y ~ x, span = loess_span, se = TRUE, color = accent, fill = accent,
      alpha = 0.12, linewidth = 0.8)

  outlier_layer <- function(x_var, y_var) {
    if (!any(outlier_idx)) return(NULL)
    list(
      geom_point(data = diag_dt[outlier_idx], aes(x = .data[[x_var]], y = .data[[y_var]]),
        color = accent, size = 2, shape = 1, stroke = 0.8),
      if (requireNamespace("ggrepel", quietly = TRUE))
        ggrepel::geom_text_repel(
          data = diag_dt[outlier_idx],
          aes(x = .data[[x_var]], y = .data[[y_var]], label = obs),
          size = 2.5, color = accent, segment.color = "gray70", max.overlaps = 15)
    )
  }

  ref_bands <- list(
    geom_hline(yintercept = c(-2.5, 2.5), linetype = "dashed", color = "gray50", linewidth = 0.4),
    geom_hline(yintercept = 0, linetype = "dashed", color = accent, linewidth = 0.7)
  )

  # ── 6. Panel 1: Residui vs Fitted ──────────────────────────────────────────
  p1 <- ggplot(diag_dt) +
    geom_point(aes(x = fitted, y = residuals), color = pal["p1"], size = pt_size, alpha = pt_alpha) +
    ref_bands +
    smooth_layer("fitted", "residuals") +
    outlier_layer("fitted", "residuals") +
    labs(
      title = "Residuals vs Fitted",
      subtitle = "Should show no pattern",
      x = "Fitted values",
      y = "Pearson residuals"
    ) +
    base_theme()

  # ── 7. Panel 2: Residui vs xvar ────────────────────────────────────────────
  p2 <- ggplot(diag_dt) +
    geom_point(aes(x = xvar, y = residuals), color = pal["p2"], size = pt_size, alpha = pt_alpha) +
    ref_bands +
    smooth_layer("xvar", "residuals") +
    outlier_layer("xvar", "residuals") +
    labs(
      title = paste("Residuals vs", xlab),
      subtitle = "Linearity & independence check",
      x = xlab,
      y = "Pearson residuals"
    ) +
    base_theme()

  # ── 8. Panel 3: Istogramma + densita' ──────────────────────────────────────
  sw <- shapiro.test(diag_dt$residuals)
  lt <- nortest::lillie.test(diag_dt$residuals)
  bw <- tryCatch(
    2 * IQR(diag_dt$residuals) / n ^ (1 / 3),
    error = function(e) diff(range(diag_dt$residuals)) / 15)

  if (!is.finite(bw) || bw==0) bw <- diff(range(diag_dt$residuals)) / 15

  ann <- sprintf("Shapiro-Wilk\nW=%.4f, p=%.4f\n\nLilliefors\nD=%.4f, p=%.4f",
    sw$statistic, sw$p.value, lt$statistic, lt$p.value)

  p3 <- ggplot(diag_dt, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = bw, fill = pal["p3"], alpha = 0.45, color = "white") +
    geom_density(color = pal["p3"], linewidth = 0.9) +
    stat_function(fun = dnorm, args = list(mean = mean(diag_dt$residuals), sd = sd(diag_dt$residuals)),
      color = accent, linewidth = 0.9, linetype = "dashed") +
    geom_rug(color = "gray40", alpha = 0.3, linewidth = 0.3) +
    annotate("label", x = Inf, y = Inf, label = ann,
      hjust = 1.05, vjust = 1.15, size = 2.6, color = "grey30",
      fill = "white", fontface = "italic", family = "mono",
      label.size = 0.3) +
    labs(
      title = "Residual Distribution",
      subtitle = "Blue = empirical | Red dashed = Normal reference",
      x = "Pearson residuals",
      y = "Density"
    ) +
    base_theme()

  # ── 9. Panel 4: Q-Q plot ───────────────────────────────────────────────────
  p4 <- if (requireNamespace("qqplotr", quietly = TRUE)) {
    ggplot(diag_dt, aes(sample = residuals)) +
      qqplotr::stat_qq_band(distribution = "norm", fill = pal["p4"], alpha = 0.25) +
      qqplotr::stat_qq_line(distribution = "norm", color = accent, linewidth = 0.8) +
      qqplotr::stat_qq_point(distribution = "norm", color = pal["p4"], size = pt_size, alpha = min(pt_alpha * 2, 0.8)) +
      labs(
        title = "Normal Q-Q Plot",
        subtitle = "With 95% confidence band",
        x = "Theoretical quantiles",
        y = "Sample quantiles"
      ) +
      base_theme()
  } else {
    ggplot(diag_dt, aes(sample = residuals)) +
      stat_qq(color = pal["p4"], size = pt_size, alpha = min(pt_alpha * 2, 0.8)) +
      stat_qq_line(color = accent, linewidth = 0.8) +
      labs(
        title = "Normal Q-Q Plot",
        subtitle = "Install {qqplotr} for confidence bands",
        x = "Theoretical quantiles",
        y = "Sample quantiles") +
      base_theme()
  }

  # ── 10. Panel 5: ACF residui ───────────────────────────────────────────────
  acf_vals <- acf(diag_dt$residuals, plot = FALSE, lag.max = min(20, n - 1))
  acf_dt <- data.table(lag = as.numeric(acf_vals$lag[-1]), acf = as.numeric(acf_vals$acf[-1]))
  ci_acf <- qnorm(0.975) / sqrt(n)

  p5 <- ggplot(acf_dt, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_hline(yintercept = c(-ci_acf, ci_acf), linetype = "dashed", color = accent, linewidth = 0.5) +
    geom_segment(aes(xend = lag, yend = 0), color = pal["p5"], linewidth = 0.7) +
    geom_point(color = pal["p5"], size = 1.8) +
    annotate("text", x = max(acf_dt$lag) * 0.85, y = ci_acf,
      label = "95% CI", vjust = -0.6, size = 2.8, color = accent, fontface = "italic") +
    labs(
      title = "ACF of Residuals",
      subtitle = "Should lie within confidence bands (no autocorrelation)",
      x = "Lag",
      y = "Autocorrelation"
    ) +
    base_theme()

  # ── 11. Panel 6: Caterpillar random effects ────────────────────────────────
  re_list <- ranef(obj)
  grp_name <- if (!is.null(group_var) && group_var %in% names(re_list)) group_var else names(re_list)[1]

  re_dt <- as.data.table(re_list[[grp_name]], keep.rownames = "level")
  re_col <- setdiff(names(re_dt), "level")[1]
  setnames(re_dt, re_col, "estimate")
  re_dt[, level := factor(level, levels = level[order(as.numeric(level))])]

  se_re <- tryCatch(
    as.numeric(arm::se.ranef(obj)[[grp_name]][,1]),
    error=function(e) rep(NA_real_, nrow(re_dt)))

  re_dt[, `:=`(se = se_re, ci_lo = estimate - 1.96 * se_re, ci_hi = estimate + 1.96 * se_re)]

  p6 <- ggplot(re_dt, aes(x = estimate, y = level)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = accent, linewidth = 0.7) +
    {if (!any(is.na(re_dt$se)))
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.3, color = pal["p6"], linewidth = 0.7, alpha = 0.7)
    } +
    geom_point(size = 3, color = pal["p6"]) +
    geom_segment(aes(x = 0, xend = estimate, yend = level), color = pal["p6"], linewidth = 0.7, alpha = 0.6) +
    labs(
      title = sprintf("Random Effects: %s", grp_name),
      subtitle = ifelse(!any(is.na(re_dt$se)), "Conditional modes +/- 95% CI", "Conditional modes"),
      x = "Random intercept",
      y = grp_name) +
    base_theme()

  # ── 12. Composizione finale ────────────────────────────────────────────────
  wp <- wrap_plots(p1, p2, p3, p4, p5, p6, ncol=2) +
    plot_annotation(
      title = paste0("LMM Diagnostics — ", model_name),
      subtitle = paste0(deparse(formula(obj))[1], " | n = ", n),
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )

  # ── 13. Summary in console ─────────────────────────────────────────────────
  if (isTRUE(summaries)) {
    r <- diag_dt$residuals
    m1 <- mean(r)
    m2 <- var(r)
    skew <- mean((r - m1)^3) / m2^(3 / 2)
    kurt <- mean((r - m1)^4) / m2^2
    fill <- cor(sort(r), qnorm(ppoints(length(r))))

    summary_dt <- data.table(
      Statistic = c("Mean","Variance","Skewness","Kurtosis","Filliben corr."),
      Value = round(c(m1, m2, skew, kurt, fill), 4),
      Reference = c("~0","~1","~0","~3","~1")
    )

    cat("\n──────────────────────────────────────────\n")
    cat("\tLMM Residuals Summary\n")
    print(summary_dt, row.names=FALSE)
    
    cat(sprintf("\nShapiro-Wilk: W=%.4f, p=%.4f %s\n",
      sw$statistic, sw$p.value, ifelse(sw$p.value>0.05, "[OK]", "[p<0.05]")))
    cat(sprintf("Lilliefors: D=%.4f, p=%.4f %s\n",
      lt$statistic, lt$p.value, ifelse(lt$p.value>0.05, "[OK]", "[p<0.05]")))
    cat("──────────────────────────────────────────\n\n")
  }

  # ── 14. Save ───────────────────────────────────────────────────────────────
  if (save) {
    filename <- paste0(model_name, "_diagnostics.png")
    filepath <- file.path(out_dir, filename)

    ggsave(filepath, plot = wp, width = 14, height = 18, dpi = 300)
    message("Saved: ", filepath)
  }

  invisible(wp)
}

ggplot_lmm_results <- function(df_model, obj, predictor, out_dir = "plot/", save = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertClass(obj, classes = "lmerMod")
  assertDataTable(df_model)
  assertNames(colnames(df_model), must.include = c("day", predictor))
  assertString(predictor)

  dt <- as.data.table(df_model)
  dt[, day_num := as.numeric(as.character(day))]
  beta <- fixef(obj)[predictor]
  int <- fixef(obj)["(Intercept)"]
  r2_m <- MuMIn::r.squaredGLMM(obj)[1]
  p_val <- coef(summary(obj))[predictor, "Pr(>|t|)"]

  x_seq <- seq(min(dt[[predictor]]), max(dt[[predictor]]), length.out = 100)
  df_lmm_line <- data.table(x = x_seq, y = int + beta * x_seq, line_type = "LMM marginal effect")

  n_days <- length(unique(dt$day_num))
  pal_days <- setNames(
    colorRampPalette(c("#2E5D8E", "#27AE60", "#C0392B"))(n_days),
    sort(unique(dt$day_num))
  )

  p <- ggplot(dt, aes(x = .data[[predictor]], y = Kvalue)) +
    geom_smooth(aes(color = factor(day_num), group = factor(day_num),
      linetype = "Within-day OLS"), method = "lm", se = FALSE, linewidth = 0.6, alpha = 0.5) +
    geom_point(aes(color = factor(day_num)), size = 3, alpha = 0.9) +
    geom_line(data = df_lmm_line, aes(x = x, y = y, linetype = "LMM marginal effect"),
      color = "grey20", linewidth = 1.2, inherit.aes = FALSE) +
    scale_linetype_manual(values = c("LMM marginal effect" = "solid"), name = NULL,
      guide  = guide_legend(override.aes = list(color = "grey20", linewidth = 1.2))) +
    scale_color_manual(values = pal_days, name = "Storage day") +
    scale_linetype_manual(
      values = c("LMM marginal effect" = "solid", "Within-day OLS" = "dashed"),
      name = NULL,
      guide = guide_legend(override.aes = list(color = c("grey20", "grey50"), linewidth = c(1.2, 0.6)))
    ) +
    annotate("label", x = min(dt[[predictor]]), y = max(dt$Kvalue),
      label = sprintf("LMM marginal fit\n\u03b2 = %.3f, p = %.3f\nR\u00b2 marginal = %.3f", beta, p_val, r2_m),
      hjust = -0.05, vjust = 1.1, size = 3.2, color = "grey20", fill = "white", fontface = "italic", label.size = 0.3) +
    guides(
      color = guide_legend(title = "Storage day", override.aes = list(linetype = "blank", shape = 16)),
      linetype = guide_legend(title = NULL, override.aes = list(color = c("grey20", "grey50"), linewidth = c(1.2, 0.6)))
    ) +
    labs(
      title = sprintf("%s vs K-value", predictor),
      subtitle = "Dashed = within-day OLS | Solid = LMM marginal effect",
      x = sprintf("Mean NIR absorbance (%s, SNV)", predictor),
      y = "K-value"
    ) +
    base_theme() +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(size = 9, face = "bold"),
      legend.spacing.y = unit(0.3, "cm")
    )

  if (save) {
    ggsave(file.path(out_dir, "lmm_results.png"), plot = p, width = 9, height = 6, dpi = 300)
    message("Saved: lmm_results.png")
  }

  invisible(p)
}
