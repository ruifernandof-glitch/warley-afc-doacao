# Etapa 10 (parte 2) - tabela de decisão item por item (estatística + teoria + conteúdo).
source("analysis/_setup.R")
d <- load_state("01_prep")$data; f6 <- load_state("06_fitidx"); cv <- load_state("07_conv")
disc <- load_state("08_disc"); dg <- load_state("09_diag")
without <- fit_without_each_item(d)
dec <- item_decision_table(cv$loadings, cv$rel, disc, without, f6$fits[1, ], dg$resid, dg$mi_all)
save_state(dec, "10_decision")
print(dec[, c("item", "carga", "decisao", "criterio")])
