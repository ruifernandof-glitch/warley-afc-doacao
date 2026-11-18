# Etapa 4 - adequação amostral em função do modelo estimado e escolha do estimador.
source("analysis/_setup.R")
d <- load_state("01_prep")$data
poly <- polychoric_check(d)
fit0 <- fit_cfa(d)
mc <- monte_carlo_cfa(fit0, n = nrow(d), reps = as.integer(Sys.getenv("WARLEY_MC_REPS", "200")))
save_state(list(poly = poly, size = model_size(fit0, nrow(d)), mc = mc), "03_sample")
print(mc$resumo)
