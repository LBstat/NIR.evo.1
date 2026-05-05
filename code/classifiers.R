
Classifier <- R6Class("Classifier",
  public = list(
    name = NULL,
    nfeat_max = NULL,
    fit_obj = NULL,
    best_hp = NULL,
    initialize = function(name, nfeat_max = 5L) {
      self$name <- name
      self$nfeat_max <- as.integer(nfeat_max)
    },
    # Returns data.frame of hyperparameter grid (one row per combination)
    # Subclasses override this. Base returns single dummy row (no HP to tune).
    hp_grid = function() {
      data.frame(dummy = NA_real_, stringsAsFactors = FALSE)
    },
    # Full tuning grid: nfeat x hyperparameters
    full_grid = function() {
      hp <- self$hp_grid()
      do.call(rbind, lapply(seq_len(self$nfeat_max), function(nf) {
        hp$nfeat <- nf
        hp
      }))
    },
    # Fit model — subclasses must implement
    fit = function(X, y, hp = NULL) {
      stop(sprintf("fit() not implemented in %s", self$name))
    },
    # Predict class probabilities for POS_LABEL — subclasses must implement
    predict_prob = function(X) {
      stop(sprintf("predict_prob() not implemented in %s", self$name))
    },
    # Safe wrapper used in CV loops
    safe_fit = function(X, y, hp = NULL) {
      tryCatch(self$fit(X, y, hp), error = function(e) {
        message(sprintf("  [%s] fit error: %s", self$name, e$message))
        NULL
      })
    },
    safe_predict = function(X, n_fallback) {
      tryCatch(
        self$predict_prob(X), error = function(e) rep(0.5, n_fallback)
      )
    },
    print = function(...) {
      cat(sprintf("<Classifier: %s | nfeat_max=%d>\n", self$name, self$nfeat_max))
      invisible(self)
    }
  )
)

LDAClassifier <- R6Class("LDAClassifier",
  inherit = Classifier,
  public = list(
    initialize = function(nfeat_max = 5L) {
      super$initialize("LDA", nfeat_max)
    },
    fit = function(X, y, hp = NULL) {
      self$fit_obj <- MASS::lda(X, y)
      invisible(self)
    },
    predict_prob = function(X) {
      predict(self$fit_obj, X)$posterior[, POS_LABEL]
    }
  )
)

QDAClassifier <- R6Class("QDAClassifier",
  inherit = Classifier,
  public = list(
    initialize = function(nfeat_max = 2L) {
      super$initialize("QDA", nfeat_max)
    },
    fit = function(X, y, hp = NULL) {
      self$fit_obj <- tryCatch(
        MASS::qda(X, y),
        error = function(e) {
          message("  [QDA] rank deficient — falling back to LDA")
          MASS::lda(X, y)
        }
      )
      invisible(self)
    },
    predict_prob = function(X) {
      tryCatch(
        predict(self$fit_obj, X)$posterior[, POS_LABEL],
        error = function(e) rep(0.5, nrow(X))
      )
    }
  )
)

NBClassifier <- R6Class("NBClassifier",
  inherit = Classifier,
  public = list(
    initialize = function(nfeat_max = 5L) {
      super$initialize("NB", nfeat_max)
    },
    fit = function(X, y, hp = NULL) {
      df <- as.data.frame(X)
      df$class <- y
      self$fit_obj <- e1071::naiveBayes(class ~ ., data = df)
      invisible(self)
    },
    predict_prob = function(X) {
      predict(self$fit_obj, as.data.frame(X), type = "raw")[, POS_LABEL]
    }
  )
)

KNNClassifier <- R6Class("KNNClassifier",
  inherit = Classifier,
  public = list(
    initialize = function(nfeat_max = 5L) {
      super$initialize("kNN", nfeat_max)
    },
    hp_grid = function() {
      expand.grid(
        k = KNN_K_GRID,
        kernel = KNN_KERNEL_GRID,
        stringsAsFactors = FALSE
      )
    },
    fit = function(X, y, hp = NULL) {
      k <- if (!is.null(hp) && !is.na(hp$k)) hp$k else 5L
      kernel <- if (!is.null(hp) && !is.na(hp$kernel)) hp$kernel else "optimal"
      self$fit_obj <- list(X = X, y = y, k = as.integer(k), kernel = kernel)
      invisible(self)
    },
    predict_prob = function(X) {
      df_tr <- as.data.frame(self$fit_obj$X)
      df_tr$class <- self$fit_obj$y
      kknn::kknn(
        class ~ .,
        train = df_tr,
        test = as.data.frame(X),
        k = self$fit_obj$k,
        kernel = self$fit_obj$kernel)$prob[, POS_LABEL]
    }
  )
)

SVMClassifier <- R6Class("SVMClassifier",
  inherit = Classifier,
  public = list(
    initialize = function(nfeat_max = 5L) {
      super$initialize("SVM", nfeat_max)
    },
    hp_grid = function() {
      data.frame(C = SVM_C_GRID, stringsAsFactors = FALSE)
    },
    fit = function(X, y, hp = NULL) {
      C <- if (!is.null(hp) && !is.na(hp$C)) hp$C else 1
      self$fit_obj <- e1071::svm(
        X, y,
        kernel = "linear",
        cost = C,
        probability = TRUE,
        scale = TRUE
      )
      invisible(self)
    },
    predict_prob = function(X) {
      attr(predict(self$fit_obj, X, probability = TRUE), "probabilities")[, POS_LABEL]
    }
  )
)

RFClassifier <- R6Class("RFClassifier",
  inherit = Classifier,
  public = list(
    initialize = function(nfeat_max = 10L) {
      super$initialize("RF", nfeat_max)
    },
    hp_grid = function() {
      expand.grid(
        mtry = RF_MTRY_GRID,
        min.node.size = RF_NODESIZE_GRID,
        stringsAsFactors = FALSE
      )
    },
    fit = function(X, y, hp = NULL) {
      mtry <- if (!is.null(hp) && !is.na(hp$mtry)) min(as.integer(hp$mtry), ncol(X)) else min(5L, ncol(X))
      min.node.size <- if (!is.null(hp) && !is.na(hp$min.node.size)) as.integer(hp$min.node.size) else 1L
      self$fit_obj  <- ranger::ranger(
        x = as.data.frame(X),
        y = y,
        num.trees = 500,
        mtry = mtry,
        min.node.size = min.node.size,
        probability = TRUE,
        importance = "impurity",
        seed = 123
      )
      invisible(self)
    },
    predict_prob = function(X) {
      predict(self$fit_obj, data = as.data.frame(X))$predictions[, POS_LABEL]
    }
  )
)

XGBClassifier <- R6Class("XGBClassifier",
  inherit = Classifier,
  public = list(
    initialize = function(nfeat_max = 10L) {
      super$initialize("XGBoost", nfeat_max)
    },
    hp_grid = function() {
      expand.grid(
        eta = XGB_ETA_GRID,
        max_depth = XGB_DEPTH_GRID,
        subsample = XGB_SUB_GRID,
        stringsAsFactors = FALSE
      )
    },
    fit = function(X, y, hp = NULL) {
      eta <- if (!is.null(hp) && !is.na(hp$eta)) hp$eta else 0.1
      max_depth <- if (!is.null(hp) && !is.na(hp$max_depth)) as.integer(hp$max_depth) else 3L
      subsample <- if (!is.null(hp) && !is.na(hp$subsample)) hp$subsample else 0.8
      y_bin <- as.numeric(y == POS_LABEL)
      self$fit_obj <- xgboost::xgb.train(
        params = list(
          objective = "binary:logistic",
          eta = eta,
          max_depth = max_depth,
          subsample = subsample,
          nthread = 1
        ),
        data = xgboost::xgb.DMatrix(X, label = y_bin),
        nrounds = 50,
        verbose = 0
      )
      invisible(self)
    },
    predict_prob = function(X) {
      as.numeric(predict(self$fit_obj, xgboost::xgb.DMatrix(X)))
    }
  )
)

