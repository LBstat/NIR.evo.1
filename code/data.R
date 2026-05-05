
build_data_main <- function() {

  # ── 1. Import data ────────────────────────────────────────────────────────
  data <- read_excel("data/raw/Dati filetti.xlsx")
  data_lightness <- read_excel("data/raw/Dati luminosità.xlsx")
  spectral_measures <- read_excel("data/raw/Spettri.xlsx")

  setDT(data); setDT(data_lightness); setDT(spectral_measures)

  variables <- c("campione", grep("^[[:alpha:]]\\*?$", colnames(data_lightness), value = TRUE))

  # ── 2. Aggregation ────────────────────────────────────────────────────────
  data[, day := ((seq_len(.N) - 1) %/% 5) + 1]

  data_lightness <- data_lightness[, ..variables][, .(
    L.median = median(`L*`, na.rm = TRUE),
    a.median = median(`a*`, na.rm = TRUE),
    b.median = median(`b*`, na.rm = TRUE),
    C.median = median(`C*`, na.rm = TRUE),
    h.median = median(h,   na.rm = TRUE)
  ), by = campione][
    , sample := as.numeric(gsub("campione_", "", campione))
  ][, campione := NULL]

  data_analysis <- cbindlist(list(data, data_lightness))
  setnames(data_analysis, "K value", "Kvalue")
  setcolorder(data_analysis, "sample")

  data_analysis <- round(data_analysis, 2)

  variables <- grep("^[[:digit:]]*$", colnames(spectral_measures), value = TRUE)
  spectral_measures <- spectral_measures[spettro %between% c(1, 5)][, ..variables]
  spectral_measures <- spectral_measures[
    , sample := ((seq_len(.N) - 1) %/% 5) + 1
  ][, lapply(.SD, function(x) median(x, na.rm = TRUE)), by = sample]

  data_analysis <- merge(data_analysis, spectral_measures)

  saveRDS(data_analysis, file = "data/intermediate/data analysis.rds")
}
