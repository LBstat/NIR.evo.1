
smoothing <- function(data, window = 11, poly = 2, m = 0) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(data, types = "numeric")
  assertNumber(window)
  assertNumber(poly, lower = 1)
  assertNumber(m, lower = 0)

  if (window %% 2 != 1) stop("Window must be odd")

  # ── 2. Store names ─────────────────────────────────────────────────────────
  orig_names <- colnames(data)

  # ── 3. Smoothing ───────────────────────────────────────────────────────────
  smoothed <- t(apply(data, 1, function(x) sgolayfilt(x, p = poly, n = window, m = m)))

  # ── 4. Converting ──────────────────────────────────────────────────────────
  smoothed_dt <- as.data.table(smoothed)
  colnames(smoothed_dt) <- orig_names

  smoothed_dt
}

normalization <- function(x) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertNumeric(x, any.missing = FALSE)

  (x - mean(x)) / sd(x)
}

preprocess_nir <- function(data) {

  #  ── 1. Assertions ─────────────────────────────────────────────────────────
  assertDataTable(data)

  sm <- smoothing(data)
  nz <- as.data.table(t(apply(sm, 1, normalization)))
  colnames(nz) <- paste0("W", colnames(data))

  nz
}

calculate_kvalue <- function(data) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(data)
  assertNames(colnames(data), must.include = c("ATP", "ADP", "AMP", "IMP", "Inosina", "ipoxantina"))

  num <- data$Inosina + data$ipoxantina
  den <- data$ATP + data$ADP + data$AMP + data$IMP + data$Inosina + data$ipoxantina

  num / den
}

get_kvalue <- function(imp, idx) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertClass(imp, classes = "mids")
  assertNumber(idx, lower = 1)

  data <- as.data.table(complete(imp, idx))
  calculate_kvalue(data)
}

make_dataset <- function(spectra, kvalue, threshold, label, print = TRUE) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(spectra)
  assertNumeric(kvalue, any.missing = FALSE)
  assertNumber(threshold)
  assertString(label)
  assertFlag(print)

  # ── 2. Dataset making ──────────────────────────────────────────────────────
  data <- cbind(
    spectra, class = factor(
      ifelse(kvalue <= threshold, POS_LABEL, NEG_LABEL), levels = CLASS_LEVELS
    )
  )

  if (print) {
    cat(
      sprintf("%-40s n=%-3d | pos=%d | neg=%d\n", label, nrow(data),
      sum(data$class == POS_LABEL), sum(data$class == NEG_LABEL))
    )
  }

  data
}

make_task <- function(data, id) {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertDataTable(data)
  assertString(id)

  TaskClassif$new(id = id, backend = as.data.frame(data), target = "class", positive = POS_LABEL)
}

compute_fs_scores <- function(X, y, method = "auc") {
  pos_idx <- which(y == POS_LABEL)
  neg_idx <- which(y == NEG_LABEL)

  if (method == "auc") {
    sapply(seq_len(ncol(X)), function(j) {
      ps <- X[pos_idx, j]
      ns <- X[neg_idx, j]
      a <- mean(outer(ps, ns, ">")) + 0.5 * mean(outer(ps, ns, "=="))
      max(a, 1 - a)
    })
  } else if (method == "anova") {
    sapply(seq_len(ncol(X)), function(j)
      summary(aov(X[, j] ~ y))[[1]][["F value"]][1])
  } else if (method == "mrmr") {
    y_bin <- as.numeric(y == POS_LABEL)
    rel <- sapply(seq_len(ncol(X)), function(j) abs(cor(X[, j], y_bin)))
    cor_X <- abs(cor(X)); diag(cor_X) <- 0
    rel - colMeans(cor_X)
  }
}

run_cv_fs <- function(classifier, task, ri, fs_method = "auc") {

  # ── 1. Assertions ──────────────────────────────────────────────────────────
  assertClass(classifier, classes = "Classifier")

  X_all <- as.matrix(task$data(cols = task$feature_names))
  storage.mode(X_all) <- "numeric"
  y_all <- task$data(cols = task$target_names)[[1]]
  feat_names <- task$feature_names
  n <- task$nrow
  label <- classifier$name

  cat(sprintf("\n  [%s | fs=%s] n=%d | iters=%d\n", label, fs_method, n, ri$iters))

  probs_sum <- numeric(n)
  probs_cnt <- integer(n)
  truth_vec <- character(n)
  thr_vec <- numeric(ri$iters)
  bands_lst <- vector("list", ri$iters)

  for (i in seq_len(ri$iters)) {
    set.seed(123 + i)
    cat(sprintf("  iter %d/%d\r", i, ri$iters)); flush.console()

    train_idx <- ri$train_set(i)
    test_idx <- ri$test_set(i)
    X_train <- X_all[train_idx, , drop = FALSE]
    y_train <- y_all[train_idx]
    X_test <- X_all[test_idx, , drop = FALSE]
    y_test <- y_all[test_idx]

    # ── Class balancing ──────────────────────────────────────────────────────
    tab <- table(y_train)
    n_maj <- max(tab); n_min <- min(tab)
    min_cls <- names(which.min(tab))

    if (n_maj > n_min && n_min > 0) {
      min_rows <- which(y_train == min_cls)
      extra <- min_rows[sample(length(min_rows), n_maj - n_min, replace = TRUE)]
      X_train <- X_train[c(seq_len(nrow(X_train)), extra), , drop = FALSE]
      y_train <- factor(c(as.character(y_train), as.character(y_train[extra])), levels = CLASS_LEVELS)
    } else {
      y_train <- factor(as.character(y_train), levels = CLASS_LEVELS)
    }

    # ── Feature selection scores ─────────────────────────────────────────────
    fs_scores <- tryCatch(
      compute_fs_scores(X_train, y_train, fs_method),
      error = function(e) rep(1, ncol(X_train))
    )

    # ── Inner 3-fold tuning: nfeat + hyperparameters ─────────────────────────
    inner_folds <- sample(rep(1:3, length.out = nrow(X_train)))
    grid <- classifier$full_grid()
    best_bacc <- -Inf
    best_nfeat <- 1L
    best_hp <- grid[1, , drop = FALSE]

    for (cfg_row in seq_len(nrow(grid))) {
      nf  <- grid$nfeat[cfg_row]
      hp  <- grid[cfg_row, setdiff(names(grid), "nfeat"), drop = FALSE]

      top_idx <- order(fs_scores, decreasing = TRUE)[seq_len(nf)]
      X_sel <- X_train[, top_idx, drop = FALSE]

      bacc_inner <- mean(sapply(1:3, function(k) {
        tr <- which(inner_folds != k)
        te <- which(inner_folds == k)
        if (length(unique(y_train[te])) < 2) return(0.5)

        clf_k <- classifier$clone()
        clf_k$safe_fit(X_sel[tr, , drop = FALSE], y_train[tr], hp)
        if (is.null(clf_k$fit_obj)) return(0.5)

        prob_k <- clf_k$safe_predict(X_sel[te, , drop = FALSE], length(te))
        pred_k <- factor(ifelse(prob_k >= 0.5, POS_LABEL, NEG_LABEL), levels = CLASS_LEVELS)
        sens_k <- mean(pred_k[y_train[te] == POS_LABEL] == POS_LABEL)
        spec_k <- mean(pred_k[y_train[te] == NEG_LABEL] == NEG_LABEL)
        (sens_k + spec_k) / 2
      }))

      if (bacc_inner > best_bacc) {
        best_bacc <- bacc_inner
        best_nfeat <- nf
        best_hp <- hp
      }
    }

    # ── Final fit with optimal hyperparameters ───────────────────────────────
    top_final <- order(fs_scores, decreasing = TRUE)[seq_len(best_nfeat)]
    X_train_fs <- X_train[, top_final, drop = FALSE]
    X_test_fs <- X_test[,  top_final, drop = FALSE]
    bands_lst[[i]] <- feat_names[top_final]

    clf_final <- classifier$clone()
    clf_final$safe_fit(X_train_fs, y_train, best_hp)

    if (is.null(clf_final$fit_obj)) {
      thr_vec[i] <- 0.5
      probs_sum[test_idx] <- probs_sum[test_idx] + 0.5
      probs_cnt[test_idx] <- probs_cnt[test_idx] + 1L
      truth_vec[test_idx] <- as.character(y_test)
      next
    }

    # ── Youden threshold training ────────────────────────────────────────────
    prob_tr <- clf_final$safe_predict(X_train_fs, nrow(X_train_fs))
    if (length(unique(as.numeric(y_train == POS_LABEL))) > 1) {
      roc_tr <- pROC::roc(as.numeric(y_train == POS_LABEL), prob_tr, quiet = TRUE)
      youden <- which.max(roc_tr$sensitivities + roc_tr$specificities - 1)
      thr_vec[i] <- roc_tr$thresholds[youden]
    } else {
      thr_vec[i] <- 0.5
    }

    # ── Test ─────────────────────────────────────────────────────────────────
    prob_te <- clf_final$safe_predict(X_test_fs, length(test_idx))
    probs_sum[test_idx] <- probs_sum[test_idx] + prob_te
    probs_cnt[test_idx] <- probs_cnt[test_idx] + 1L
    truth_vec[test_idx] <- as.character(y_test)
  }

  cat(sprintf("  iter %d/%d — done\n", ri$iters, ri$iters))

  # ── Average results ────────────────────────────────────────────────────────
  probs_avg <- ifelse(probs_cnt > 0, probs_sum / probs_cnt, 0.5)
  thr_mean <- mean(thr_vec[is.finite(thr_vec)], na.rm = TRUE)
  if (!is.finite(thr_mean)) thr_mean <- 0.5

  truth_f <- factor(truth_vec, levels = CLASS_LEVELS)
  pred_f  <- factor(ifelse(probs_avg >= thr_mean, POS_LABEL, NEG_LABEL), levels = CLASS_LEVELS)

  acc <- mean(pred_f == truth_f)
  sens <- mean(pred_f[truth_f == POS_LABEL] == POS_LABEL)
  spec <- mean(pred_f[truth_f == NEG_LABEL] == NEG_LABEL)
  bacc <- (sens + spec) / 2
  auc <- tryCatch(
    as.numeric(pROC::auc(pROC::roc(
      as.numeric(truth_f == POS_LABEL), probs_avg, quiet = TRUE))),
    error = function(e) NA_real_
  )

  all_bands <- unlist(bands_lst)
  band_freq <- sort(table(all_bands), decreasing = TRUE)
  band_pct <- round(prop.table(band_freq) * 100, 1)

  cat(sprintf("  %-12s | bacc=%.3f | auc=%.3f | sens=%.3f | spec=%.3f\n", label, bacc, auc, sens, spec))
  cat(sprintf("  Top 5: %s\n", paste(sprintf("%s(%.0f%%)", names(head(band_pct, 5)),  head(band_pct, 5)), collapse = ", ")))

  list(
    metrics = data.table(
      learner = label, fs_method = fs_method,
      threshold = round(thr_mean, 3),
      acc = round(acc, 3), bacc = round(bacc, 3),
      auc = round(auc, 3), sens = round(sens, 3),
      spec = round(spec, 3)
    ),
    band_freq = band_freq,
    band_pct = band_pct,
    probs = probs_avg,
    truth = truth_f
  )
}

run_step1_complete <- function(task, ri, dataset_label,
  fs_methods = c("auc", "anova", "mrmr")) {

  cat(sprintf("\n\u2550\u2550 %s \u2550\u2550\n", dataset_label))

  all_results <- lapply(models_all_fs, function(clf) {
    lapply(fs_methods, function(fsm) {
      lbl <- sprintf("%s_%s", clf$name, fsm)
      res <- tryCatch(
        run_cv_fs(clf$clone(), task, ri, fs_method = fsm),
        error = function(e) {
          message(sprintf(" %s error: %s", lbl, e$message))
          NULL
        }
      )
      if (is.null(res)) return(NULL)
      res$metrics[, learner := lbl]
      res$metrics[, dataset := dataset_label]
      res
    })
  })

  all_flat <- Filter(Negate(is.null), unlist(all_results, recursive = FALSE))
  metrics_dt <- rbindlist(lapply(all_flat, function(r) r$metrics))
  probs_list <- setNames(
    lapply(all_flat, function(r) list(probs = r$probs, truth = r$truth)),
    sapply(all_flat, function(r) r$metrics$learner)
  )

  list(metrics = metrics_dt, probs = probs_list)
}

extract_band_freq <- function(task, ri, dataset_label, best_cfg = NULL) {

  if (is.null(best_cfg)) {
    best_cfg <- if (dataset_label == "IMP") best_config_imp else best_config_real
  }

  cat(sprintf("\n── %s ──\n", dataset_label))

  rbindlist(lapply(seq_len(nrow(best_cfg)), function(r) {
    bc <- best_cfg[r]
    mdl_list <- Filter(function(m) m$name == bc$learner_base, models_all_fs)

    if (length(mdl_list) == 0) {
      message(sprintf("  Classifier non trovato: %s", bc$learner_base))
      return(NULL)
    }

    mdl <- mdl_list[[1]]$clone()
    res <- tryCatch(
      run_cv_fs(mdl, task, ri, fs_method = bc$fs_method),
      error = function(e) {
        message(sprintf("  %s error: %s", bc$learner_base, e$message))
        NULL
      })

    if (is.null(res)) return(NULL)

    dt <- as.data.table(as.list(res$band_pct))
    dt_long <- melt(dt, variable.name = "banda", value.name = "pct")
    dt_long[, `:=`(
      learner = bc$learner_base,
      dataset = dataset_label,
      freq = as.integer(res$band_freq[as.character(banda)])
    )]

    dt_long[order(-pct)][seq_len(min(.N, 10))]
  }))
}

run_permutation <- function(classifier, best_hp, best_bands, best_threshold,
  task, ri, obs_bacc, label) {

  X_all <- as.matrix(task$data(cols = task$feature_names))
  storage.mode(X_all) <- "numeric"
  y_all <- task$data(cols = task$target_names)[[1]]
  n <- task$nrow

  band_idx <- which(colnames(X_all) %in% best_bands)

  cat(sprintf("\nPermutation test — %s | obs_bacc=%.3f\n", label, obs_bacc))
  perm_bacc <- numeric(N_PERM)

  for (p in seq_len(N_PERM)) {
    cat(sprintf(" perm %d/%d\r", p, N_PERM))
    flush.console()
    set.seed(42 + p)

    y_perm <- sample(y_all)
    probs_sum <- numeric(n)
    probs_cnt <- integer(n)
    truth_vec <- character(n)

    for (i in seq_len(ri$iters)) {
      train_idx <- ri$train_set(i)
      test_idx <- ri$test_set(i)

      X_tr_fs <- X_all[train_idx, band_idx, drop = FALSE]
      X_te_fs <- X_all[test_idx,  band_idx, drop = FALSE]
      y_train <- y_perm[train_idx]

      tab <- table(y_train)
      n_maj <- max(tab)
      n_min <- min(tab)
      min_cls <- names(which.min(tab))
      if (n_maj > n_min && n_min > 0) {
        min_rows <- which(y_train == min_cls)
        extra <- min_rows[sample(length(min_rows), n_maj - n_min, replace = TRUE)]
        X_tr_fs <- X_tr_fs[c(seq_len(nrow(X_tr_fs)), extra), , drop = FALSE]
        y_train <- factor(c(as.character(y_train), as.character(y_train[extra])), levels = CLASS_LEVELS)
      } else {
        y_train <- factor(as.character(y_train), levels = CLASS_LEVELS)
      }

      clf_p <- classifier$clone()
      clf_p$safe_fit(X_tr_fs, y_train, hp = best_hp)

      if (is.null(clf_p$fit_obj)) next

      prob_te <- clf_p$safe_predict(X_te_fs, length(test_idx))
      probs_sum[test_idx] <- probs_sum[test_idx] + prob_te
      probs_cnt[test_idx] <- probs_cnt[test_idx] + 1L
      truth_vec[test_idx] <- as.character(y_perm[test_idx])
    }

    probs_avg <- ifelse(probs_cnt > 0, probs_sum / probs_cnt, 0.5)
    truth_f <- factor(truth_vec, levels = CLASS_LEVELS)
    pred_f <- factor(ifelse(probs_avg >= best_threshold, POS_LABEL, NEG_LABEL), levels = CLASS_LEVELS)
    sens_p <- mean(pred_f[truth_f == POS_LABEL] == POS_LABEL)
    spec_p <- mean(pred_f[truth_f == NEG_LABEL] == NEG_LABEL)
    perm_bacc[p] <- (sens_p + spec_p) / 2
  }

  cat(sprintf("  perm %d/%d — done\n", N_PERM, N_PERM))
  p_val <- mean(perm_bacc >= obs_bacc)
  cat(sprintf("  P-value: %.3f | %s\n", p_val, ifelse(p_val < 0.05, "\u2705", "\u274c")))

  list(
    summary = data.table(
      model = label,
      obs_bacc = obs_bacc,
      perm_mean = round(mean(perm_bacc), 3),
      p_value = round(p_val, 3),
      result = ifelse(p_val < 0.05, "significant", "not significant")
    ),
    perm_bacc = perm_bacc
  )
}

prepare_lmm_data <- function(data) {

  #  ── 1. Assertions ─────────────────────────────────────────────────────────
  assertDataTable(data)

  #  ── 2. Variables ──────────────────────────────────────────────────────────
  nir_vars <- grep("^[[:digit:]]+$", colnames(data), value = TRUE)
  chem_vars <- c("IMP","ATP","ADP","ipoxantina","AMP","Inosina")
  complete_idx <- which(complete.cases(data[, ..chem_vars]))

  #  ── 3. Data pre-processing ────────────────────────────────────────────────
  data_spectra <- preprocess_nir(data[complete_idx, ..nir_vars])
  X_real <- as.matrix(data_spectra)
  kv <- data$Kvalue[complete_idx]
  day <- data$day[complete_idx]

  list(
    X_real = X_real,
    kv = kv,
    day = day,
    complete_idx = complete_idx
  )
}

run_partial_cors <- function(X_real, kv, day) {

  #  ── 1. Assertions ─────────────────────────────────────────────────────────
  assertMatrix(X_real, mode = "numeric", any.missing = FALSE)
  assertNumeric(kv, any.missing = FALSE)
  assertNumeric(day, any.missing = FALSE)

  partial_cors <- rbindlist(lapply(seq_len(ncol(X_real)), function(j) {
    df <- data.frame(nir = X_real[, j], kval = kv, day = day)
    pc <- tryCatch(
      pcor.test(df$nir, df$kval, df$day), error = function(e) NULL
    )

    if (is.null(pc)) return(NULL)

    data.table(
      banda = colnames(X_real)[j],
      r_simple = round(cor(df$nir, df$kval), 3),
      r_partial = round(pc$estimate, 3),
      p_partial = round(pc$p.value, 3))
  }))

  partial_cors[, abs_r := abs(r_partial)]
  setorder(partial_cors, -abs_r)
  partial_cors
}

get_model_config <- function(learner_name) {
  base <- gsub("_auc|_anova|_mrmr", "", learner_name)
  fsm <- regmatches(learner_name, regexpr("auc|anova|mrmr", learner_name))

  if (length(fsm) == 0) fsm <- "auc"
  mdl <- models_all_fs[sapply(models_all_fs, function(m) m$name == base)][[1]]
  list(mdl = mdl, fsm = fsm)
}

run_lmm_candidates <- function(X_real, kv, day) {
  df_base <- data.table(
    Kvalue = kv,
    day = factor(day),
    NIR_1520_1532 = rowMeans(X_real[, c("W1520","W1526","W1532")]),
    NIR_1453_1469 = rowMeans(X_real[, c("W1453","W1459","W1464","W1469")]),
    NIR_1646_1660 = rowMeans(X_real[, c("W1646","W1653","W1660")]),
    NIR_2143_2291 = rowMeans(X_real[, c("W2143","W2154","W2265","W2277","W2291")]),
    NIR_1770_1778 = rowMeans(X_real[, c("W1770","W1778")])
  )

  mod0 <- lmer(Kvalue ~ 1 + (1|day), data = df_base, REML = FALSE)
  r2_null <- r.squaredGLMM(mod0)

  test_pred <- function(pred_name, label) {
    fmla <- as.formula(paste("Kvalue ~", pred_name, "+ (1|day)"))
    mod <- tryCatch(lmer(fmla, data = df_base, REML = FALSE), error = function(e) NULL)

    if (is.null(mod)) return(NULL)

    lrt <- anova(mod0, mod)
    r2 <- r.squaredGLMM(mod)
    coef_s <- summary(mod)$coefficients
    beta <- coef_s[pred_name, "Estimate"]
    p_fix <- coef_s[pred_name, "Pr(>|t|)"]
    p_lrt <- lrt[2, "Pr(>Chisq)"]

    data.table(
      predictor = label,
      beta = round(as.numeric(beta), 4),
      p_fixed = round(p_fix, 4),
      p_LRT = round(p_lrt, 4),
      R2_marginal = round(r2[1], 4),
      delta_R2 = round(r2[2] - r2_null[2], 4),
      AIC = round(AIC(mod), 2))
  }

  results_lmm <- rbindlist(list(
    test_pred("NIR_1520_1532", "Mean 1520-1532 nm (significant partial correlation)"),
    test_pred("NIR_1453_1469", "O-H/N-H"),
    test_pred("NIR_1646_1660", "C=O lipids"),
    test_pred("NIR_2143_2291", "C-H/C-O esters"),
    test_pred("NIR_1770_1778", "C-H fatty acids")
  ))

  setorder(results_lmm, p_LRT)

  list(
    results = results_lmm,
    mod0 = mod0,
    r2_null = r2_null,
    df_base = df_base)
}

