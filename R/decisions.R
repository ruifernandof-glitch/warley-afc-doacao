#' Ajuste do modelo sem cada indicador (sensibilidade pós hoc)
#'
#' @param data Banco analítico.
#' @return Tibble item x índices de ajuste do modelo sem o item.
#' @export
fit_without_each_item <- function(data) {
  do.call(rbind, lapply(warley_items(), function(it) {
    f <- fit_cfa(data, warley_model_without(it))
    fm <- lavaan::fitMeasures(f, c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
    tibble::tibble(item = it, chi2_sem = fm[[1]], gl_sem = fm[[2]], cfi_sem = fm[[3]],
                   tli_sem = fm[[4]], rmsea_sem = fm[[5]], srmr_sem = fm[[6]],
                   heywood_sem = any(abs(lavaan::standardizedSolution(f)$est.std[lavaan::standardizedSolution(f)$op == "=~"]) > 1))
  }))
}

#' Justificativas de conteúdo por item (APA)
#'
#' Notas redigidas a partir do enunciado de cada item e da teoria do
#' comportamento planejado; ficam separadas do cálculo para que a decisão
#' combine estatística, teoria e validade de conteúdo.
#'
#' @return Vetor nomeado item -> texto.
#' @export
item_content_notes <- function() {
  c(
    A1 = "Avaliação geral positiva do comportamento, núcleo da definição de atitude (Ajzen, 1991).",
    A2 = "Componente instrumental da atitude; estimativa no limite superior sugere sobreposição de redação com A3 e A4, a examinar em versão futura (Brown, 2015; Kolenikov & Bollen, 2012).",
    A3 = "Crença sobre consequências do comportamento para os estudantes, conteúdo central da atitude na TCP (Ajzen, 1991).",
    A4 = "Avaliação da relevância social da contribuição; cobre faceta normativa da atitude prevista no instrumento (Ajzen, 1991; Smith & McSweeney, 2007).",
    NS1 = "Aprovação de referentes importantes, formulação clássica da norma subjetiva injuntiva (Ajzen, 1991).",
    NS2 = "Referente específico (família), relevante para doação em contexto escolar (Smith & McSweeney, 2007).",
    NS3 = "Julgamento de correção por pessoas próximas; amplia o domínio dos referentes (Ajzen, 1991).",
    NS4 = "Referentes que influenciam decisões; fecha o domínio de conteúdo da norma subjetiva (Ajzen, 1991).",
    CP1 = "Capacidade de realizar a doação, faceta de autoeficácia do controle percebido (Ajzen, 2002).",
    CP2 = "Facilidade percebida, indicador clássico de controle comportamental (Ajzen, 1991, 2002).",
    CP3 = "Conhecimento sobre como doar; faceta informacional do controle, conceitualmente mais distante de CP1-CP2, o que é coerente com carga menor (Ajzen, 2002).",
    CP4 = "Controlabilidade (decisão própria), faceta distinta da autoeficácia segundo Ajzen (2002); carga menor é teoricamente esperada.",
    C1 = "Confiança no uso adequado dos recursos, definição central de confiança do doador (Sargeant & Lee, 2004).",
    C2 = "Crença na aplicação em benefício dos estudantes; índice de modificação elevado com C3 indica possível redundância de formulação (Brown, 2015).",
    C3 = "Confiança na prestação de contas, dimensão de accountability da confiança (Sargeant & Lee, 2004).",
    C4 = "Responsabilidade da gestão escolar no uso dos recursos; completa o domínio de confiança institucional (Sargeant & Lee, 2004).",
    ID1 = "Intenção declarada de doar no futuro, formulação padrão de intenção (Ajzen, 1991).",
    ID2 = "Probabilidade subjetiva de doar quando houver oportunidade (Ajzen, 1991).",
    ID3 = "Consideração séria de contribuir; carga próxima de 1 sugere redundância com ID1-ID2 (Brown, 2015).",
    ID4 = "Intenção condicionada a campanha transparente; mistura intenção e confiança, o que justifica monitorar seu resíduo com o fator Confiança (Ajzen, 1991; Sargeant & Lee, 2004)."
  )
}

#' Tabela de decisão item por item
#'
#' Regras definidas antes da inspeção dos resultados: **excluir** apenas se
#' carga < 0,50 ou não significativa e houver fundamento de conteúdo;
#' **revisar** se 0,50 <= carga < 0,70, se a carga estiver no limite (IC95%
#' contendo 1) ou se o item participar de resíduo |r| > 0,10 ou de índice
#' de modificação > 10 sugerindo redundância; **manter** nos demais casos.
#' "Revisar" significa manter no modelo desta amostra e revisar a redação na
#' próxima versão do instrumento (Hair et al., 2010; Brown, 2015).
#'
#' @param loadings Saída de [loadings_table()].
#' @param rel Saída de [reliability_table()].
#' @param disc Saída de [discriminant_table()].
#' @param without Saída de [fit_without_each_item()].
#' @param full_fit Linha de [fit_table()] do modelo completo.
#' @param resid Saída de [residual_diagnostics()].
#' @param mi Índices de modificação completos (`lavaan::modindices`).
#' @return Tibble com 20 linhas.
#' @export
item_decision_table <- function(loadings, rel, disc, without, full_fit, resid, mi) {
  notes <- item_content_notes()
  pares <- disc$pares
  disc_by_factor <- vapply(rel$fator, function(f) {
    p <- pares[pares$fator_1 == f | pares$fator_2 == f, ]
    sprintf("HTMT máx. %.2f; F-L %s", max(p$htmt), ifelse(all(p$fornell_larcker), "atendido", "violado"))
  }, character(1))
  res_max <- vapply(warley_items(), function(it) {
    p <- resid$pares[resid$pares$item_1 == it | resid$pares$item_2 == it, ]
    max(abs(p$residuo))
  }, numeric(1))
  mi_max <- vapply(warley_items(), function(it) {
    m <- mi[mi$op == "~~" & (mi$lhs == it | mi$rhs == it), ]
    if (nrow(m)) max(m$mi) else 0
  }, numeric(1))
  k <- table(loadings$fator)
  out <- loadings |>
    dplyr::left_join(without, by = "item") |>
    dplyr::mutate(
      contribuicao_ave = r2 / as.numeric(k[fator]),
      ave_fator = rel$ave[match(fator, rel$fator)],
      discriminante = disc_by_factor[fator],
      residuo_max = res_max[item], mi_residual_max = mi_max[item],
      cfi_com = full_fit$cfi, rmsea_com = full_fit$rmsea, srmr_com = full_fit$srmr,
      decisao = dplyr::case_when(
        (carga < 0.50 | p > 0.05) ~ "Excluir",
        carga < 0.70 ~ "Revisar",
        ic_sup >= 1 ~ "Revisar",
        residuo_max > 0.10 | mi_residual_max > 10 ~ "Revisar",
        TRUE ~ "Manter"),
      criterio = dplyr::case_when(
        (carga < 0.50 | p > 0.05) ~ "Carga < 0,50 ou não significativa",
        carga < 0.70 ~ "Carga entre 0,50 e 0,70",
        ic_sup >= 1 ~ "Carga no limite (IC95% contém 1): possível redundância",
        residuo_max > 0.10 ~ sprintf("Resíduo de correlação %.2f com outro item", residuo_max),
        mi_residual_max > 10 ~ sprintf("MI residual %.1f: possível redundância de redação", mi_residual_max),
        TRUE ~ "Carga >= 0,70, sem problemas locais"),
      justificativa = unname(notes[item])
    )
  out
}
