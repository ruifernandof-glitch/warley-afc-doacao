#' Cargas padronizadas, erros-padrão, IC95% e R² dos indicadores
#'
#' @param fit Objeto lavaan.
#' @return Tibble com uma linha por indicador.
#' @export
loadings_table <- function(fit) {
  ss <- lavaan::standardizedSolution(fit)
  l <- ss[ss$op == "=~", ]
  tibble::tibble(construto = factor_label(l$lhs), fator = l$lhs, item = l$rhs,
                 carga = l$est.std, ep = l$se, z = l$z, p = l$pvalue,
                 ic_inf = l$ci.lower, ic_sup = l$ci.upper, r2 = l$est.std^2)
}

#' Confiabilidade composta (ômega categórico), AVE e alfa
#'
#' O ômega é calculado por `semTools::compRelSEM()`, que para itens ordinais
#' usa a abordagem de Green e Yang (2009); a AVE por `semTools::AVE()`
#' (Fornell & Larcker, 1981). Alfa de Cronbach e alfa ordinal são
#' apresentados apenas como evidência complementar (McNeish, 2018).
#'
#' @param fit Objeto lavaan.
#' @param data Banco analítico.
#' @return Tibble por construto.
#' @export
reliability_table <- function(fit, data) {
  k <- warley_constructs()
  av <- semTools::AVE(fit)
  om <- semTools::compRelSEM(fit)
  alfa <- vapply(k$itens, function(b) psych::alpha(as.data.frame(data[b]), warnings = FALSE)$total$raw_alpha, numeric(1))
  alfa_ord <- vapply(k$itens, function(b) {
    r <- suppressWarnings(psych::polychoric(as.data.frame(data[b]))$rho)
    psych::alpha(r, warnings = FALSE)$total$raw_alpha
  }, numeric(1))
  tibble::tibble(construto = k$rotulo, fator = k$fator,
                 omega = unname(om[k$fator]), ave = unname(av[k$fator]),
                 raiz_ave = sqrt(unname(av[k$fator])),
                 alfa = alfa, alfa_ordinal = alfa_ord)
}

#' Correlações latentes com erro-padrão e IC95%
#'
#' @param fit Objeto lavaan.
#' @return Tibble por par de fatores.
#' @export
latent_correlations <- function(fit) {
  ss <- lavaan::standardizedSolution(fit)
  facs <- lavaan::lavNames(fit, "lv")
  lc <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% facs & ss$rhs %in% facs, ]
  tibble::tibble(fator_1 = lc$lhs, fator_2 = lc$rhs, r = lc$est.std, ep = lc$se,
                 ic_inf = lc$ci.lower, ic_sup = lc$ci.upper)
}

#' Validade discriminante: Fornell-Larcker e HTMT
#'
#' Combina o critério de Fornell e Larcker (1981), a razão heterotraço-
#' monotraço (Henseler et al., 2015) calculada com correlações policóricas
#' e o limite superior do IC95% da correlação latente (Rönkkö & Cho, 2022).
#'
#' @param fit Objeto lavaan.
#' @param data Banco analítico.
#' @param rel Saída de [reliability_table()].
#' @return Lista com matriz Fornell-Larcker, matriz HTMT e tabela por par.
#' @export
discriminant_table <- function(fit, data, rel) {
  items <- warley_items()
  clv <- lavaan::lavInspect(fit, "cor.lv")
  fl <- clv; diag(fl) <- rel$raiz_ave[match(rownames(fl), rel$fator)]
  ht <- semTools::htmt(warley_model(), data = as_ordinal(data, items), ordered = items)
  lc <- latent_correlations(fit)
  pares <- lc |>
    dplyr::mutate(
      htmt = ht[cbind(match(fator_2, rownames(ht)), match(fator_1, colnames(ht)))],
      htmt = ifelse(is.na(htmt), ht[cbind(match(fator_1, rownames(ht)), match(fator_2, colnames(ht)))], htmt),
      raiz_ave_1 = rel$raiz_ave[match(fator_1, rel$fator)],
      raiz_ave_2 = rel$raiz_ave[match(fator_2, rel$fator)],
      fornell_larcker = pmin(raiz_ave_1, raiz_ave_2) > abs(r),
      htmt_085 = htmt < 0.85,
      ic_abaixo_090 = ic_sup < 0.90
    )
  list(fornell_larcker = fl, htmt = ht, pares = pares)
}
