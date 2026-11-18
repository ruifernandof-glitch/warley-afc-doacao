# Pipeline targets: mudanças nos dados ou em qualquer função de R/ invalidam
# apenas as etapas dependentes. Executar com targets::tar_make() na raiz.
library(targets)
if (!isTRUE(l10n_info()$`UTF-8`)) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
tar_source("R")
tar_option_set(packages = c("lavaan", "semTools", "psych", "dplyr", "tibble", "ggplot2"),
               seed = 20261028)

raw_file <- Sys.getenv("WARLEY_RAW", "data/raw/respostas_original.xlsx")
if (!file.exists(raw_file)) raw_file <- sub("\\.xlsx$", ".csv", raw_file)

list(
  # Etapa 3 - importação, elegibilidade, exame preliminar
  tar_target(raw_path, raw_file, format = "file"),
  tar_target(prep, prepare_warley(read_raw_warley(raw_path))),
  tar_target(dados, prep$data),
  tar_target(processed_csv, {
    p <- out_path("data", "processed", "dados_analiticos.csv"); readr::write_csv(dados, p); p
  }, format = "file"),
  tar_target(desc, item_descriptives(dados)),
  tar_target(freq, item_frequencies(dados)),
  tar_target(miss, missing_summary(dados)),
  tar_target(qual, response_quality(dados)),
  tar_target(norm, normality_tests(dados)),
  tar_target(profile, profile_table(dados)),
  # Etapas 1-2 - especificação a priori
  tar_target(modelo, warley_model()),
  tar_target(modelo_lav, { p <- out_path("outputs", "models", "modelo_cinco_fatores.lav"); writeLines(modelo, p); p }, format = "file"),
  # Etapas 4-5 - estimador, tamanho amostral, estimação, admissibilidade
  tar_target(poly, polychoric_check(dados)),
  tar_target(fit, fit_cfa(dados, modelo, "delta")),
  tar_target(fit_theta, fit_cfa(dados, modelo, "theta")),
  tar_target(size, model_size(fit, nrow(dados))),
  tar_target(mc, monte_carlo_cfa(fit, nrow(dados), reps = 200)),
  tar_target(adm, admissibility(fit, "Principal (delta)")),
  tar_target(adm_theta, admissibility(fit_theta, "Especificação original (theta)")),
  tar_target(rivals, c(list(fator_unico = fit_cfa(dados, warley_one_factor())),
                       lapply(warley_rival_models(), function(m) fit_cfa(dados, m)))),
  # Etapa 6 - ajuste global
  tar_target(fits, rbind(fit_table(fit, "Cinco fatores (a priori)"),
                         do.call(rbind, lapply(names(rivals), function(nm) fit_table(rivals[[nm]], nm))))),
  tar_target(comp, compare_models(fit, rivals)),
  # Etapas 7-9 - cargas, confiabilidade, AVE, discriminância
  tar_target(loadings, loadings_table(fit)),
  tar_target(rel, reliability_table(fit, dados)),
  tar_target(disc, discriminant_table(fit, dados, rel)),
  # Etapa 10 - diagnóstico, sensibilidade e decisão por item
  tar_target(resid, residual_diagnostics(fit)),
  tar_target(mi_all, lavaan::modindices(fit, sort. = TRUE)),
  tar_target(mi_top, modification_table(fit)),
  tar_target(sens, sensitivity_models(dados, qual)),
  tar_target(sens_sum, sensitivity_summary(sens, fit)),
  tar_target(without, fit_without_each_item(dados)),
  tar_target(decision, item_decision_table(loadings, rel, disc, without, fits[1, ], resid, mi_all)),
  # Saídas
  tar_target(res, list(prep = prep, data = dados, desc = desc, freq = freq, miss = miss, qual = qual,
                       norm = norm, profile = profile, poly = poly, size = size, mc = mc, fit = fit,
                       fit_theta = fit_theta, rivals = rivals, adm = adm, adm_theta = adm_theta,
                       fits = fits, comp = comp, loadings = loadings, rel = rel, disc = disc,
                       resid = resid, mi_all = mi_all, mi_top = mi_top, sens = sens,
                       sens_sum = sens_sum, decision = decision, session = utils::sessionInfo())),
  tar_target(res_rds, { p <- out_path("outputs", "models", "resultados.rds"); saveRDS(res, p); p }, format = "file"),
  tar_target(figures, save_figures(res), format = "file"),
  tar_target(tables, export_tables(res), format = "file"),
  tar_target(report, {
    force(res_rds); force(figures); force(tables)
    system2("quarto", c("render", "manuscript/index.qmd", "--to", "docx"))
    "manuscript/index.docx"
  }, format = "file")
)
