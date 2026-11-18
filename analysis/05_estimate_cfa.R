# Etapa 5 - estimação e admissibilidade (delta principal; theta pré-especificada como verificação).
source("analysis/_setup.R")
d <- load_state("01_prep")$data; spec <- load_state("04_spec")
fit <- fit_cfa(d, spec$principal, "delta")
fit_theta <- fit_cfa(d, spec$principal, "theta")
rivals <- c(list(fator_unico = fit_cfa(d, spec$fator_unico)), lapply(spec$rivais, function(m) fit_cfa(d, m)))
save_state(list(fit = fit, fit_theta = fit_theta, rivals = rivals,
                adm = admissibility(fit, "Principal (delta)"),
                adm_theta = admissibility(fit_theta, "Especificação original (theta)")), "05_fit")
print(admissibility(fit))
