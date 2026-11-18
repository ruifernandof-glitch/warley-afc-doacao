#' Resíduos de correlação e resíduos padronizados
#'
#' @param fit Objeto lavaan.
#' @param corte Limite para resíduo de correlação relevante (Kline, 2016).
#' @return Lista com matrizes e tabela dos pares acima do corte.
#' @export
residual_diagnostics <- function(fit, corte = 0.10) {
  r <- lavaan::lavResiduals(fit, type = "cor.bentler")
  rc <- r$cov
  z <- r$cov.z
  lt <- which(lower.tri(rc), arr.ind = TRUE)
  tab <- tibble::tibble(item_1 = rownames(rc)[lt[, 1]], item_2 = colnames(rc)[lt[, 2]],
                        residuo = rc[lt], z = if (!is.null(z)) z[lt] else NA_real_)
  list(matriz = rc, z = z, pares = dplyr::arrange(tab, dplyr::desc(abs(residuo))),
       acima = dplyr::filter(tab, abs(residuo) > corte) |> dplyr::arrange(dplyr::desc(abs(residuo))),
       srmr = lavaan::fitMeasures(fit, "srmr"), corte = corte)
}

#' Índices de modificação (sem reespecificação automática)
#'
#' @param fit Objeto lavaan.
#' @param n Quantos índices listar.
#' @return Tibble ordenado por MI, com tipo de parâmetro.
#' @export
modification_table <- function(fit, n = 15) {
  m <- utils::head(lavaan::modindices(fit, sort. = TRUE), n)
  tibble::tibble(parametro = paste(m$lhs, m$op, m$rhs),
                 tipo = ifelse(m$op == "=~", "Carga cruzada", "Covariância residual"),
                 mi = m$mi, epc_padronizado = m$sepc.all)
}

#' Análises de sensibilidade (pós hoc, não substituem o modelo principal)
#'
#' @param data Banco analítico.
#' @param qual Saída de [response_quality()].
#' @return Lista de modelos estimados.
#' @export
sensitivity_models <- function(data, qual) {
  items <- warley_items()
  col5 <- data
  for (v in items) col5[[v]] <- as.integer(cut(data[[v]], c(0, 2, 3, 4, 5, 7)))
  keep <- !qual$casos$invariante_20
  theta <- tryCatch(fit_cfa(data, parameterization = "theta"), error = function(e) NULL)
  list(
    theta_7cat = theta,
    delta_5cat = fit_cfa(col5),
    sem_invariantes = fit_cfa(data[keep, ]),
    mlr_continuo = suppressWarnings(lavaan::cfa(warley_model(), data = as.data.frame(data[items]),
                                                 estimator = "MLR", std.lv = TRUE))
  )
}

#' Resumo comparativo das análises de sensibilidade
#'
#' @param sens Saída de [sensitivity_models()].
#' @param principal Modelo principal.
#' @return Tibble com ajuste, admissibilidade e faixas de cargas/AVE/correlações.
#' @export
sensitivity_summary <- function(sens, principal) {
  all <- c(list(principal_delta_7cat = principal), sens)
  rotulos <- c(principal_delta_7cat = "Principal: WLSMV delta, 7 categorias",
               theta_7cat = "WLSMV theta, 7 categorias (especificação original)",
               delta_5cat = "WLSMV delta, 5 categorias (1-2, 3, 4, 5, 6-7)",
               sem_invariantes = "WLSMV delta, sem respostas idênticas nos 20 itens",
               mlr_continuo = "MLR, itens tratados como contínuos")
  do.call(rbind, lapply(names(all), function(nm) {
    f <- all[[nm]]
    if (is.null(f)) return(NULL)
    fm <- lavaan::fitMeasures(f)
    g <- function(a, b) if (a %in% names(fm) && !is.na(fm[[a]])) unname(fm[[a]]) else NA_real_
    pt <- lavaan::parTable(f)
    se_ok <- !anyNA(pt$se[pt$free > 0])
    l <- if (se_ok) lavaan::standardizedSolution(f) else NULL
    lam <- if (!is.null(l)) l$est.std[l$op == "=~"] else lavaan::inspect(f, "std")$lambda[lavaan::inspect(f, "std")$lambda != 0]
    clv <- lavaan::lavInspect(f, "cor.lv")
    av <- tryCatch(semTools::AVE(f), error = function(e) NA)
    tibble::tibble(analise = rotulos[[nm]], n = lavaan::lavInspect(f, "nobs"),
                   erros_padrao = ifelse(se_ok, "Sim", "Não"),
                   chi2 = g("chisq.scaled", "chisq"), gl = fm[["df"]],
                   cfi = if (nm == "mlr_continuo") g("cfi.robust", "cfi") else g("cfi.scaled", "cfi"),
                   rmsea = if (nm == "mlr_continuo") g("rmsea.robust", "rmsea") else g("rmsea.scaled", "rmsea"),
                   srmr = fm[["srmr"]],
                   carga_min = min(lam), carga_max = max(lam),
                   ave_min = min(av), r_latente_max = max(abs(clv[lower.tri(clv)])))
  }))
}
