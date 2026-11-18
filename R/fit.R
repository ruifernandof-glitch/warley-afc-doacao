#' Índices de ajuste global de um modelo
#'
#' Reporta χ² escalonado (média e variância), gl, p, χ²/gl, CFI, TLI,
#' RMSEA com IC90% e SRMR, além do χ² DWLS não corrigido. Índices
#' "robustos" são incluídos quando a versão do lavaan os calcula para dados
#' ordinais (Savalei, 2021); caso contrário, ficam como NA.
#'
#' @param fit Objeto lavaan.
#' @param nome Rótulo do modelo.
#' @return Tibble de uma linha.
#' @export
fit_table <- function(fit, nome) {
  want <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
            "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr",
            "chisq", "df", "cfi.robust", "tli.robust", "rmsea.robust")
  fm <- lavaan::fitMeasures(fit)
  v <- vapply(want, function(z) if (z %in% names(fm)) unname(fm[[z]]) else NA_real_, numeric(1))
  tibble::tibble(modelo = nome, chi2 = v[["chisq.scaled"]], gl = v[["df.scaled"]],
                 p = v[["pvalue.scaled"]], chi2_gl = v[["chisq.scaled"]] / v[["df.scaled"]],
                 cfi = v[["cfi.scaled"]], tli = v[["tli.scaled"]], rmsea = v[["rmsea.scaled"]],
                 rmsea_li = v[["rmsea.ci.lower.scaled"]], rmsea_ls = v[["rmsea.ci.upper.scaled"]],
                 srmr = v[["srmr"]], chi2_dwls = v[["chisq"]],
                 cfi_robusto = v[["cfi.robust"]], rmsea_robusto = v[["rmsea.robust"]])
}

#' Comparar o modelo principal com rivais aninhados
#'
#' Usa o teste de diferença de qui-quadrado escalonado (Satorra, 2000)
#' implementado em `lavaan::lavTestLRT()`.
#'
#' @param principal Modelo de cinco fatores.
#' @param rivais Lista nomeada de modelos rivais.
#' @return Tibble com Δχ², Δgl e p.
#' @export
compare_models <- function(principal, rivais) {
  do.call(rbind, lapply(names(rivais), function(nm) {
    t <- tryCatch(lavaan::lavTestLRT(rivais[[nm]], principal, method = "satorra.2000"),
                  error = function(e) NULL)
    if (is.null(t)) return(tibble::tibble(rival = nm, delta_chi2 = NA_real_, delta_gl = NA_real_, p = NA_real_))
    tibble::tibble(rival = nm, delta_chi2 = t[2, "Chisq diff"], delta_gl = t[2, "Df diff"],
                   p = t[2, "Pr(>Chisq)"])
  }))
}
