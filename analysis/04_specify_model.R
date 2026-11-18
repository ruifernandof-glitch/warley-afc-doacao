# Etapas 1 e 2 - construtos e especificação a priori (sem cargas cruzadas nem resíduos correlacionados).
source("analysis/_setup.R")
spec <- list(principal = warley_model(), fator_unico = warley_one_factor(), rivais = warley_rival_models(),
             mapa = warley_constructs(), enunciados = item_wording())
writeLines(spec$principal, out_path("outputs", "models", "modelo_cinco_fatores.lav"))
save_state(spec, "04_spec")
cat(spec$principal, "\n")
