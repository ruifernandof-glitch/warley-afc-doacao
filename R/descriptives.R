#' Estatísticas descritivas por indicador
#'
#' @param data Banco analítico.
#' @return Tibble com média, DP, mediana, assimetria, curtose, piso e teto (%).
#' @export
item_descriptives <- function(data) {
  X <- as.data.frame(data[warley_items()])
  tibble::tibble(
    item = names(X),
    construto = factor_label(item_factor()[names(X)]),
    n = vapply(X, function(x) sum(!is.na(x)), integer(1)),
    media = vapply(X, mean, numeric(1), na.rm = TRUE),
    dp = vapply(X, stats::sd, numeric(1), na.rm = TRUE),
    mediana = vapply(X, stats::median, numeric(1), na.rm = TRUE),
    assimetria = vapply(X, psych::skew, numeric(1)),
    curtose = vapply(X, psych::kurtosi, numeric(1)),
    piso_pct = vapply(X, function(x) 100 * mean(x == 1, na.rm = TRUE), numeric(1)),
    teto_pct = vapply(X, function(x) 100 * mean(x == 7, na.rm = TRUE), numeric(1))
  )
}

#' Frequências por categoria (1-7) e células esparsas
#'
#' @param data Banco analítico.
#' @return Tibble largo item x categoria com contagens.
#' @export
item_frequencies <- function(data) {
  X <- as.data.frame(data[warley_items()])
  m <- t(vapply(X, function(x) as.integer(table(factor(x, levels = 1:7))), integer(7)))
  colnames(m) <- paste0("cat_", 1:7)
  tibble::as_tibble(m) |> dplyr::mutate(item = rownames(m), .before = 1) |>
    dplyr::mutate(min_celula = apply(m, 1, min))
}

#' Ausências por variável
#'
#' @param data Banco analítico.
#' @return Tibble com n e % ausente.
#' @export
missing_summary <- function(data) {
  tibble::tibble(variavel = names(data),
                 ausentes = vapply(data, function(x) sum(is.na(x) | (is.character(x) & !nzchar(trimws(x)))), integer(1)),
                 pct = 100 * ausentes / nrow(data))
}

#' Diagnóstico de qualidade de resposta (sem exclusão automática)
#'
#' Calcula resposta invariante nos 20 itens, invariância dentro de cada bloco,
#' maior sequência idêntica (longstring) e distância de Mahalanobis
#' (Curran, 2016; Meade & Craig, 2012).
#'
#' @param data Banco analítico.
#' @return Lista com `casos` (um registro por respondente) e `resumo`.
#' @export
response_quality <- function(data) {
  X <- as.data.frame(data[warley_items()])
  k <- warley_constructs()
  invar_bloco <- vapply(k$itens, function(b) apply(X[b], 1, stats::sd) == 0,
                        logical(nrow(X)))
  colnames(invar_bloco) <- k$fator
  md <- stats::mahalanobis(X, colMeans(X), stats::cov(X))
  casos <- tibble::tibble(
    id_anonimo = data$id_anonimo,
    invariante_20 = apply(X, 1, stats::sd) == 0,
    blocos_invariantes = rowSums(invar_bloco),
    longstring = apply(X, 1, function(r) max(rle(as.numeric(r))$lengths)),
    mahal = md,
    mahal_p = stats::pchisq(md, df = ncol(X), lower.tail = FALSE)
  )
  resumo <- tibble::tibble(
    indicador = c("Resposta idêntica nos 20 itens",
                  "Invariância dentro dos 5 blocos",
                  "Invariância em 1 ou mais blocos",
                  "Longstring >= 16 itens",
                  "Outlier multivariado (Mahalanobis p < 0,001)"),
    n = c(sum(casos$invariante_20), sum(casos$blocos_invariantes == 5),
          sum(casos$blocos_invariantes >= 1), sum(casos$longstring >= 16),
          sum(casos$mahal_p < 0.001))
  ) |> dplyr::mutate(pct = 100 * n / nrow(X))
  por_bloco <- tibble::tibble(fator = k$fator, construto = k$rotulo,
                              pct_invariante = 100 * colMeans(invar_bloco))
  list(casos = casos, resumo = resumo, por_bloco = por_bloco)
}

#' Normalidade univariada e multivariada
#'
#' Usa `MVN::mvn()` quando disponível e, na ausência, o teste de Mardia de
#' `psych::mardia()` (Mardia, 1970).
#'
#' @param data Banco analítico.
#' @return Tibble com assimetria e curtose multivariadas.
#' @export
normality_tests <- function(data) {
  X <- as.data.frame(data[warley_items()])
  if (requireNamespace("MVN", quietly = TRUE)) {
    r <- MVN::mvn(X, mvn_test = "mardia")
    return(tibble::as_tibble(r$multivariate_normality))
  }
  m <- psych::mardia(X, plot = FALSE)
  tibble::tibble(teste = c("Assimetria multivariada de Mardia", "Curtose multivariada de Mardia"),
                 estatistica = c(m$skew, m$kurtosis),
                 p = c(m$p.skew, m$p.kurt),
                 b = c(m$b1p, m$b2p),
                 implementacao = "psych::mardia")
}

#' Perfil dos respondentes (para relatório agregado)
#'
#' @param data Banco analítico.
#' @param vars Variáveis de perfil.
#' @return Tibble variável/categoria/n/%.
#' @export
profile_table <- function(data, vars = c("faixa_etaria", "escolaridade", "renda_familiar",
                                         "vinculo", "conhecimento_doacao", "participacao_acao",
                                         "experiencia_escola_publica")) {
  purrr_free <- lapply(vars, function(v) {
    t <- table(data[[v]])
    tibble::tibble(variavel = v, categoria = sub("\\.$", "", names(t)),
                   n = as.integer(t), pct = 100 * as.integer(t) / nrow(data))
  })
  do.call(rbind, purrr_free)
}
