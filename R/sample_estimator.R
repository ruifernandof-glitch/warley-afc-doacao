#' Diagnóstico da matriz policórica
#'
#' @param data Banco analítico.
#' @return Lista com a matriz, menor autovalor e se foi necessário suavizar.
#' @export
polychoric_check <- function(data) {
  X <- as.data.frame(data[warley_items()])
  smoothed <- FALSE
  rho <- withCallingHandlers(psych::polychoric(X, correct = 0)$rho,
                             warning = function(w) {
                               if (grepl("positive definite", conditionMessage(w))) smoothed <<- TRUE
                               invokeRestart("muffleWarning")
                             })
  ev <- eigen(rho, symmetric = TRUE, only.values = TRUE)$values
  list(rho = rho, min_eigen = min(ev), smoothed = smoothed,
       max_offdiag = max(abs(rho[lower.tri(rho)])))
}

#' Contagem de informação amostral e de parâmetros livres
#'
#' @param fit Modelo lavaan estimado.
#' @param n Tamanho da amostra.
#' @return Tibble com estatísticas amostrais, parâmetros e razões.
#' @export
model_size <- function(fit, n) {
  p <- length(lavaan::lavNames(fit, "ov"))
  thr <- sum(lavaan::parameterEstimates(fit)$op == "|")
  npar <- lavaan::fitMeasures(fit, "npar")
  stats_n <- thr + p * (p - 1) / 2
  tibble::tibble(
    indicador = c("Respondentes analíticos", "Indicadores", "Limiares estimados",
                  "Estatísticas amostrais (limiares + correlações policóricas)",
                  "Parâmetros livres", "Graus de liberdade", "Respondentes por parâmetro livre"),
    valor = c(n, p, thr, stats_n, npar, lavaan::fitMeasures(fit, "df"), n / npar)
  )
}

#' Simulação de Monte Carlo do desempenho do modelo com o N observado
#'
#' Gera dados ordinais a partir da solução estimada (cargas, correlações e
#' limiares), reestima o modelo com WLSMV e registra convergência,
#' admissibilidade, viés relativo das cargas, cobertura e taxa de
#' rejeição do qui-quadrado (Muthén & Muthén, 2002; Wolf et al., 2013).
#'
#' @param fit Modelo populacional (estimado na amostra).
#' @param n Tamanho amostral a simular.
#' @param reps Número de réplicas.
#' @param seed Semente.
#' @return Lista com `replicas` e `resumo`.
#' @export
monte_carlo_cfa <- function(fit, n, reps = 200, seed = 20261028) {
  set.seed(seed)
  ss <- lavaan::standardizedSolution(fit)
  lam <- ss[ss$op == "=~", ]
  lam$est.std <- pmin(lam$est.std, 0.985)
  facs <- unique(lam$lhs); items <- lam$rhs
  Phi <- lavaan::lavInspect(fit, "cor.lv")[facs, facs]
  L <- matrix(0, length(items), length(facs), dimnames = list(items, facs))
  L[cbind(items, lam$lhs)] <- lam$est.std
  Sigma <- L %*% Phi %*% t(L); diag(Sigma) <- 1
  pe <- lavaan::parameterEstimates(fit)
  thr <- split(pe$est[pe$op == "|"], pe$lhs[pe$op == "|"])
  syntax <- paste(vapply(facs, function(f) paste(f, "=~", paste(lam$rhs[lam$lhs == f], collapse = " + ")), ""), collapse = "\n")
  one <- function(r) {
    Z <- MASS::mvrnorm(n, rep(0, length(items)), Sigma)
    D <- as.data.frame(lapply(seq_along(items), function(j) findInterval(Z[, j], thr[[items[j]]]) + 1L))
    names(D) <- items
    if (any(vapply(D, function(x) length(unique(x)) < 2, logical(1)))) return(NULL)
    f <- tryCatch(suppressWarnings(lavaan::cfa(syntax, data = as_ordinal(D, items), ordered = items,
                                                estimator = "WLSMV", parameterization = "delta",
                                                std.lv = TRUE)), error = function(e) NULL)
    if (is.null(f) || !lavaan::lavInspect(f, "converged")) return(tibble::tibble(rep = r, convergiu = FALSE))
    s <- lavaan::standardizedSolution(f); l <- s[s$op == "=~", ]
    fm <- lavaan::fitMeasures(f, c("pvalue.scaled", "rmsea.scaled", "cfi.scaled", "srmr"))
    tibble::tibble(rep = r, convergiu = TRUE,
                   heywood = any(abs(l$est.std) > 1),
                   vies_rel_medio = mean((l$est.std - lam$est.std) / lam$est.std),
                   cobertura = mean(l$ci.lower <= lam$est.std & l$ci.upper >= lam$est.std, na.rm = TRUE),
                   se_ok = !anyNA(l$se),
                   rejeita_chi2 = fm[["pvalue.scaled"]] < 0.05,
                   rmsea = fm[["rmsea.scaled"]], cfi = fm[["cfi.scaled"]], srmr = fm[["srmr"]])
  }
  rp <- do.call(rbind, lapply(seq_len(reps), one))
  ok <- rp[rp$convergiu, ]
  resumo <- tibble::tibble(
    indicador = c("Réplicas", "Convergência (%)", "Soluções com carga > |1| (%)",
                  "Erros-padrão computados (%)", "Viés relativo médio das cargas (%)",
                  "Cobertura média do IC95% das cargas (%)", "Rejeição do χ² a 5% (%)",
                  "RMSEA médio", "CFI médio", "SRMR médio"),
    valor = c(reps, 100 * mean(rp$convergiu), 100 * mean(ok$heywood), 100 * mean(ok$se_ok),
              100 * mean(ok$vies_rel_medio), 100 * mean(ok$cobertura),
              100 * mean(ok$rejeita_chi2), mean(ok$rmsea), mean(ok$cfi), mean(ok$srmr))
  )
  list(replicas = rp, resumo = resumo, n = n)
}
