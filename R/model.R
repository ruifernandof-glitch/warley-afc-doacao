#' Modelo de mensuração a priori: cinco fatores correlacionados
#'
#' Cada indicador carrega apenas no seu fator teórico; não há cargas
#' cruzadas nem covariâncias residuais. A identificação é feita com
#' `std.lv = TRUE` (variância de cada fator fixada em 1), deixando livres as
#' 20 cargas e as 10 correlações entre fatores (Brown, 2015; Kline, 2016).
#'
#' @return String com a sintaxe lavaan.
#' @export
warley_model <- function() {
  k <- warley_constructs()
  paste(sprintf("%s =~ %s", k$fator, vapply(k$itens, paste, character(1), collapse = " + ")),
        collapse = "\n")
}

#' Modelo rival de fator único
#'
#' @return String lavaan.
#' @export
warley_one_factor <- function() {
  paste("G =~", paste(warley_items(), collapse = " + "))
}

#' Modelos rivais teoricamente plausíveis com quatro fatores
#'
#' Fundem pares de construtos adjacentes na TCP: controle percebido com
#' intenção, e norma subjetiva com intenção (Ajzen, 1991, 2002).
#'
#' @return Lista nomeada de strings lavaan.
#' @export
warley_rival_models <- function() {
  list(
    quatro_cp_int = paste("ATT =~ A1 + A2 + A3 + A4",
                          "NS =~ NS1 + NS2 + NS3 + NS4",
                          "CPINT =~ CP1 + CP2 + CP3 + CP4 + ID1 + ID2 + ID3 + ID4",
                          "CONF =~ C1 + C2 + C3 + C4", sep = "\n"),
    quatro_ns_int = paste("ATT =~ A1 + A2 + A3 + A4",
                          "NSINT =~ NS1 + NS2 + NS3 + NS4 + ID1 + ID2 + ID3 + ID4",
                          "CP =~ CP1 + CP2 + CP3 + CP4",
                          "CONF =~ C1 + C2 + C3 + C4", sep = "\n"),
    quatro_att_conf = paste("ATTCONF =~ A1 + A2 + A3 + A4 + C1 + C2 + C3 + C4",
                            "NS =~ NS1 + NS2 + NS3 + NS4",
                            "CP =~ CP1 + CP2 + CP3 + CP4",
                            "INT =~ ID1 + ID2 + ID3 + ID4", sep = "\n")
  )
}

#' Sintaxe do modelo sem um indicador (sensibilidade pós hoc)
#'
#' @param item Item a remover.
#' @return String lavaan.
#' @export
warley_model_without <- function(item) {
  k <- warley_constructs()
  its <- lapply(k$itens, setdiff, item)
  paste(sprintf("%s =~ %s", k$fator, vapply(its, paste, character(1), collapse = " + ")),
        collapse = "\n")
}
