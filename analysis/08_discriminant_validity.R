# Etapa 9 - validade discriminante (correlações latentes + IC, Fornell-Larcker, HTMT).
source("analysis/_setup.R")
d <- load_state("01_prep")$data; f <- load_state("05_fit"); cv <- load_state("07_conv")
disc <- discriminant_table(f$fit, d, cv$rel)
save_state(disc, "08_disc")
print(disc$pares)
