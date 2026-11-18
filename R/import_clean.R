#' Ler a exportação bruta do Google Forms (.xlsx ou .csv)
#'
#' @param path Caminho do arquivo bruto.
#' @return Tibble com as colunas originais (nomes preservados).
#' @export
read_raw_warley <- function(path) {
  stopifnot(file.exists(path))
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    raw <- readxl::read_excel(path, col_types = "text", .name_repair = "minimal")
  } else {
    raw <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                           name_repair = "minimal", show_col_types = FALSE)
  }
  names(raw) <- trimws(names(raw))
  raw
}

#' Converter rótulo Likert ("6 Concordo") em inteiro 1-7
#'
#' @param x Vetor de rótulos.
#' @return Inteiros 1-7 ou NA.
#' @export
parse_likert <- function(x) {
  v <- suppressWarnings(as.integer(sub("^\\s*([1-7]).*$", "\\1", x)))
  v[!is.na(v) & !(v %in% 1:7)] <- NA_integer_
  v
}

#' Localizar as colunas dos 20 indicadores pelo código entre colchetes
#'
#' @param nms Nomes das colunas brutas.
#' @return Vetor nomeado item -> nome da coluna.
#' @export
item_columns <- function(nms) {
  out <- vapply(warley_items(), function(it) {
    hit <- grep(paste0("\\[", it, "\\."), nms, value = TRUE)
    if (length(hit) != 1L) stop("Coluna não encontrada ou ambígua para ", it)
    hit
  }, character(1))
  out
}

#' Aplicar regras de elegibilidade predefinidas e montar o banco analítico
#'
#' Regras (definidas antes da análise): (1) concordância expressa com o TCLE;
#' (2) vínculo declarado com escola pública estadual; (3) os 20 indicadores
#' completos. Nenhum caso é excluído por critério estatístico.
#'
#' @param raw Saída de [read_raw_warley()].
#' @return Lista com `data` (banco analítico), `audit` (motivo por caso) e
#'   `flow` (fluxograma de elegibilidade).
#' @export
prepare_warley <- function(raw) {
  nms <- names(raw)
  col_of <- function(pattern) {
    hit <- grep(pattern, nms, ignore.case = TRUE)
    if (length(hit) != 1L) stop("Coluna não identificada: ", pattern)
    hit
  }
  codes <- item_columns(nms)
  base <- tibble::tibble(id_anonimo = sprintf("R%03d", seq_len(nrow(raw))))
  for (it in names(codes)) base[[it]] <- parse_likert(raw[[codes[[it]]]])

  consent <- grepl("^sim, li e concordo", tolower(raw[[col_of("concorda voluntariamente")]]))
  vinc <- raw[[col_of("seu v.nculo com a escola")]]
  eligible <- !is.na(vinc) & nzchar(trimws(vinc)) &
    !grepl("n[aã]o possuo", tolower(vinc))
  complete <- stats::complete.cases(base[warley_items()])
  reason <- dplyr::case_when(!consent ~ "sem_consentimento",
                             !eligible ~ "sem_vinculo",
                             !complete ~ "incompleto",
                             TRUE ~ "apto")
  keep <- reason == "apto"
  data <- base[keep, , drop = FALSE]
  data$faixa_etaria <- raw[[col_of("^faixa et")]][keep]
  data$escolaridade <- raw[[col_of("^escolaridade")]][keep]
  data$renda_familiar <- raw[[col_of("^renda familiar")]][keep]
  data$experiencia_escola_publica <- raw[[col_of("^experi.ncia como estudante")]][keep]
  data$vinculo <- vinc[keep]
  data$conhecimento_doacao <- raw[[col_of("sabia que escolas")]][keep]
  data$participacao_acao <- raw[[col_of("participou de alguma a")]][keep]

  ts <- raw[[1]]
  datas <- suppressWarnings(as.Date(sub(" .*$", "", ts), format = "%m/%d/%Y"))
  levels_flow <- c("sem_consentimento", "sem_vinculo", "incompleto")
  flow <- tibble::tibble(
    etapa = c("Registros recebidos", "Excluídos: ausência de consentimento",
              "Excluídos: sem vínculo declarado", "Excluídos: indicadores incompletos",
              "Casos analíticos"),
    n = c(nrow(raw), vapply(levels_flow, function(l) sum(reason == l), integer(1)),
          sum(keep))
  )
  list(data = data,
       audit = tibble::tibble(id_anonimo = base$id_anonimo, motivo = reason),
       flow = flow,
       periodo = range(datas, na.rm = TRUE))
}
