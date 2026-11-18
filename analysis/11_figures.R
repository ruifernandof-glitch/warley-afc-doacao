# Saídas finais: figuras (PNG 300 dpi + PDF), tabelas (CSV + DOCX) e objeto consolidado.
source("analysis/_setup.R")
prep <- load_state("01_prep"); ds <- load_state("02_desc"); sm <- load_state("03_sample")
f <- load_state("05_fit"); fi <- load_state("06_fitidx"); cv <- load_state("07_conv")
res <- c(list(prep = prep, data = prep$data), ds, sm, list(fit = f$fit, fit_theta = f$fit_theta,
         rivals = f$rivals, adm = f$adm, adm_theta = f$adm_theta), fi, cv,
         list(disc = load_state("08_disc")), load_state("09_diag"), list(decision = load_state("10_decision")))
res$session <- utils::sessionInfo()
saveRDS(res, out_path("outputs", "models", "resultados.rds"))
print(save_figures(res)); print(export_tables(res))
