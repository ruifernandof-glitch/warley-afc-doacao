# Etapa 6 - ajuste global combinado e comparação com rivais aninhados.
source("analysis/_setup.R")
f <- load_state("05_fit")
fits <- rbind(fit_table(f$fit, "Cinco fatores (a priori)"),
              do.call(rbind, lapply(names(f$rivals), function(nm) fit_table(f$rivals[[nm]], nm))))
comp <- compare_models(f$fit, f$rivals)
save_state(list(fits = fits, comp = comp), "06_fitidx")
print(fits); print(comp)
