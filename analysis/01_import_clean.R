# Etapa 3 (parte 1) - importação e elegibilidade predefinida.
source("analysis/_setup.R")
raw <- read_raw_warley(RAW_PATH)
prep <- prepare_warley(raw)
stopifnot(nrow(prep$data) > 0, !anyDuplicated(prep$data$id_anonimo))
readr::write_csv(prep$data, out_path("data", "processed", "dados_analiticos.csv"))
save_state(prep, "01_prep")
print(prep$flow)
