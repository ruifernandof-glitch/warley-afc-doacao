# Carrega funções do compêndio e define caminhos estáveis.
# Executar a partir da raiz do projeto.
if (!isTRUE(l10n_info()$`UTF-8`)) {
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8") != "" || Sys.setlocale("LC_CTYPE", "en_US.UTF-8") != "")
  if (!ok) stop("Execute o R em uma localidade UTF-8 (ex.: LC_ALL=C.UTF-8); os scripts têm acentos.")
}
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source, encoding = "UTF-8"))
RAW_PATH <- Sys.getenv("WARLEY_RAW", "data/raw/respostas_original.xlsx")
if (!file.exists(RAW_PATH)) RAW_PATH <- sub("\\.xlsx$", ".csv", RAW_PATH)
state_path <- function(step) out_path("outputs", "models", paste0(step, ".rds"))
save_state <- function(x, step) saveRDS(x, state_path(step))
load_state <- function(step) readRDS(state_path(step))
