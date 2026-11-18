#' Estimar AFC ordinal com WLSMV
#'
#' Itens declarados como ordinais; correlações policóricas, limiares e
#' estimador DWLS com erros-padrão robustos e qui-quadrado ajustado por média
#' e variância (Flora & Curran, 2004; Li, 2016; Rosseel, 2012).
#'
#' @param data Banco analítico (itens inteiros 1-7 ou já recategorizados).
#' @param model Sintaxe lavaan.
#' @param parameterization "delta" (principal) ou "theta" (pré-registrada).
#' @param items Itens ordinais.
#' @return Objeto lavaan.
#' @export
fit_cfa <- function(data, model = warley_model(), parameterization = "delta",
                    items = NULL) {
  if (is.null(items)) items <- intersect(warley_items(), lavaan::lavNames(lavaan::lavaanify(model), "ov"))
  stopifnot(all(stats::complete.cases(data[items])))
  suppressWarnings(
    lavaan::cfa(model, data = as_ordinal(data, items), ordered = items,
                estimator = "WLSMV", parameterization = parameterization,
                std.lv = TRUE)
  )
}

#' Verificação de convergência e admissibilidade da solução
#'
#' Verifica convergência, erros-padrão computáveis, definição positiva da
#' matriz de covariância das estimativas, variâncias residuais negativas,
#' cargas padronizadas fora de [-1, 1] e correlações latentes |r| >= 1
#' (Kolenikov & Bollen, 2012; Brown, 2015).
#'
#' @param fit Objeto lavaan.
#' @param nome Rótulo do modelo.
#' @return Tibble com um critério por linha.
#' @export
admissibility <- function(fit, nome = "cinco_fatores") {
  conv <- lavaan::lavInspect(fit, "converged")
  pt <- lavaan::parTable(fit)
  se_ok <- !anyNA(pt$se[pt$free > 0])
  raw_lam <- pt$est[pt$op == "=~"]; names(raw_lam) <- pt$rhs[pt$op == "=~"]
  extreme <- names(raw_lam)[abs(raw_lam) > 10]
  vc <- tryCatch(lavaan::lavInspect(fit, "vcov"), error = function(e) NULL)
  vc_min <- if (is.null(vc) || anyNA(vc)) NA_real_ else min(eigen(vc, symmetric = TRUE, only.values = TRUE)$values)
  th <- diag(lavaan::lavInspect(fit, "theta"))
  ss <- lavaan::standardizedSolution(fit)
  lam <- ss[ss$op == "=~", ]
  clv <- lavaan::lavInspect(fit, "cor.lv")
  max_r <- if (length(clv) > 1) max(abs(clv[lower.tri(clv)])) else NA_real_
  neg <- names(th)[th < 0]
  over <- lam$rhs[abs(lam$est.std) > 1]
  over_ci <- lam$rhs[abs(lam$est.std) > 1 & !is.na(lam$ci.lower) & lam$ci.lower > 1]
  tibble::tibble(
    modelo = nome,
    criterio = c("Convergência", "Erros-padrão computados",
                 "Menor autovalor da matriz de covariância das estimativas",
                 "Variâncias residuais negativas", "Cargas padronizadas > |1|",
                 "Carga > 1 com IC95% inteiramente acima de 1",
                 "Carga não padronizada extrema (|λ| > 10)",
                 "Maior correlação latente em módulo"),
    resultado = c(ifelse(conv, "Sim", "Não"), ifelse(se_ok, "Sim", "Não"),
                  formatC(vc_min, format = "e", digits = 2),
                  ifelse(length(neg), paste0(neg, " (", sprintf("%.4f", th[neg]), ")", collapse = "; "), "Nenhuma"),
                  ifelse(length(over), paste0(over, " (", sprintf("%.3f", lam$est.std[match(over, lam$rhs)]), ")", collapse = "; "), "Nenhuma"),
                  ifelse(length(over_ci), paste(over_ci, collapse = "; "), "Nenhuma"),
                  ifelse(length(extreme), paste0(extreme, " (", sprintf("%.1f", raw_lam[extreme]), ")", collapse = "; "), "Nenhuma"),
                  sprintf("%.3f", max_r)),
    adequado = c(conv, se_ok, !is.na(vc_min) && vc_min > -1e-8, length(neg) == 0,
                 length(over) == 0, length(over_ci) == 0, length(extreme) == 0,
                 is.na(max_r) || max_r < 1)
  )
}
