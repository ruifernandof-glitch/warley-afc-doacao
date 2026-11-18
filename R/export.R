#' Gravar tabela em CSV (UTF-8) e DOCX (flextable)
#'
#' @param df Data frame.
#' @param name Nome-base estável do arquivo.
#' @param title Título da tabela (para o .docx).
#' @param dest Pasta de destino.
#' @param digits Casas decimais para colunas numéricas no .docx.
#' @return Caminhos dos dois arquivos.
#' @export
write_table <- function(df, name, title = name, dest = "outputs/tables", digits = 3) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(dest, paste0(name, ".csv"))
  readr::write_csv(df, csv, na = "")
  docx <- file.path(dest, paste0(name, ".docx"))
  ft <- flextable::flextable(as.data.frame(df))
  num <- names(df)[vapply(df, is.double, logical(1))]
  if (length(num)) ft <- flextable::colformat_double(ft, j = num, digits = digits, decimal.mark = ",", big.mark = ".")
  ft <- flextable::set_caption(ft, title)
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::autofit(ft)
  flextable::save_as_docx(ft, path = docx)
  c(csv, docx)
}

#' Matriz para data frame com nome da linha
#'
#' @param m Matriz.
#' @param first Nome da primeira coluna.
#' @return Tibble.
#' @export
matrix_df <- function(m, first = "fator") {
  m <- as.matrix(m); m[upper.tri(m)] <- NA
  tibble::as_tibble(as.data.frame(m)) |> dplyr::mutate(!!first := rownames(m), .before = 1)
}

#' Exportar todas as tabelas do relatório
#'
#' @param res Lista de resultados do pipeline.
#' @param dest Pasta de destino.
#' @return Vetor de caminhos.
#' @export
export_tables <- function(res, dest = "outputs/tables") {
  tabs <- list(
    tab01_fluxo_amostral = list(res$prep$flow, "Fluxo de elegibilidade"),
    tab02_descritivas_itens = list(res$desc, "Estatísticas descritivas dos indicadores"),
    tab03_frequencias_categorias = list(res$freq, "Frequências por categoria"),
    tab04_ausencias = list(res$miss, "Ausências por variável"),
    tab05_qualidade_resposta = list(res$qual$resumo, "Qualidade de resposta"),
    tab05b_invariancia_por_bloco = list(res$qual$por_bloco, "Invariância por bloco"),
    tab06_normalidade = list(res$norm, "Normalidade multivariada"),
    tab07_tamanho_modelo = list(res$size, "Informação amostral e parâmetros"),
    tab08_monte_carlo = list(res$mc$resumo, "Simulação de Monte Carlo com N observado"),
    tab09_admissibilidade = list(rbind(res$adm, res$adm_theta), "Convergência e admissibilidade"),
    tab10_ajuste_modelos = list(res$fits, "Ajuste global dos modelos"),
    tab11_comparacao_modelos = list(res$comp, "Comparação com modelos rivais"),
    tab12_cargas = list(res$loadings, "Cargas padronizadas"),
    tab13_confiabilidade_ave = list(res$rel, "Confiabilidade composta, AVE e alfa"),
    tab14_correlacoes_latentes = list(res$disc$pares, "Correlações latentes, HTMT e Fornell-Larcker"),
    tab14b_fornell_larcker = list(matrix_df(res$disc$fornell_larcker), "Matriz Fornell-Larcker"),
    tab14c_htmt = list(matrix_df(res$disc$htmt), "Matriz HTMT"),
    tab15_residuos = list(res$resid$acima, "Resíduos de correlação acima do corte"),
    tab16_indices_modificacao = list(res$mi_top, "Índices de modificação"),
    tab17_sensibilidade = list(res$sens_sum, "Análises de sensibilidade"),
    tab18_decisao_itens = list(res$decision, "Decisão item por item"),
    tab19_perfil = list(suppress_small(res$profile), "Perfil dos respondentes (agregado, células < 5 suprimidas)")
  )
  unlist(lapply(names(tabs), function(nm) write_table(tabs[[nm]][[1]], nm, tabs[[nm]][[2]], dest)))
}

#' Suprimir células pequenas em tabelas de perfil
#'
#' @param df Tibble com colunas `n` e `pct`.
#' @param min Menor contagem divulgada.
#' @return Tibble com `n` e `pct` como texto e "< min" nas células pequenas.
#' @export
suppress_small <- function(df, min = 5) {
  small <- df$n < min
  df$n <- ifelse(small, paste0("< ", min), as.character(df$n))
  df$pct <- ifelse(small, "", formatC(df$pct, format = "f", digits = 1, decimal.mark = ","))
  df
}
