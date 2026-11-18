# Etapas 7 e 8 - cargas, validade convergente, confiabilidade composta e AVE.
source("analysis/_setup.R")
d <- load_state("01_prep")$data; f <- load_state("05_fit")
out <- list(loadings = loadings_table(f$fit), rel = reliability_table(f$fit, d))
save_state(out, "07_conv")
print(out$rel)
