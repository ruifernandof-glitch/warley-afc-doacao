# Executa o pipeline em sequência (alternativa a targets::tar_make()).
steps <- sprintf("analysis/%s", c("01_import_clean.R", "02_descriptives_missing.R", "03_sample_estimator.R",
  "04_specify_model.R", "05_estimate_cfa.R", "06_fit_indices.R", "07_loadings_ave_cr.R",
  "08_discriminant_validity.R", "09_modification_diagnostics.R", "10_item_decision_table.R", "11_figures.R"))
for (s in steps) { message("== ", s); source(s, encoding = "UTF-8", local = new.env()) }
