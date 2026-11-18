# Etapa 10 (parte 1) - resíduos, índices de modificação e sensibilidade pós hoc.
source("analysis/_setup.R")
d <- load_state("01_prep")$data; f <- load_state("05_fit"); q <- load_state("02_desc")$qual
resid <- residual_diagnostics(f$fit)
mi_all <- lavaan::modindices(f$fit, sort. = TRUE)
sens <- sensitivity_models(d, q)
save_state(list(resid = resid, mi_all = mi_all, mi_top = modification_table(f$fit),
                sens = sens, sens_sum = sensitivity_summary(sens, f$fit)), "09_diag")
print(resid$acima)
