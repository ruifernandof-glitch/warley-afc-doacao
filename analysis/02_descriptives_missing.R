# Etapa 3 (parte 2) - descritivas, ausências, frequências, qualidade de resposta e normalidade.
source("analysis/_setup.R")
prep <- load_state("01_prep"); d <- prep$data
out <- list(desc = item_descriptives(d), freq = item_frequencies(d), miss = missing_summary(d),
            qual = response_quality(d), norm = normality_tests(d), profile = profile_table(d))
save_state(out, "02_desc")
print(out$qual$resumo)
