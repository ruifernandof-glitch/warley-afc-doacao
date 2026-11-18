#' Itens da escala, na ordem do instrumento
#'
#' @return Vetor de caracteres com os 20 códigos de indicadores.
#' @export
warley_items <- function() {
  c(paste0("A", 1:4), paste0("NS", 1:4), paste0("CP", 1:4),
    paste0("C", 1:4), paste0("ID", 1:4))
}

#' Mapeamento a priori item -> construto (Ajzen, 1991; Sargeant & Lee, 2004)
#'
#' @return Tibble com código do fator, rótulo em português e itens.
#' @export
warley_constructs <- function() {
  tibble::tibble(
    fator  = c("ATT", "NS", "CP", "CONF", "INT"),
    rotulo = c("Atitude", "Norma subjetiva", "Controle percebido",
               "Confiança", "Intenção de doar"),
    itens  = list(paste0("A", 1:4), paste0("NS", 1:4), paste0("CP", 1:4),
                  paste0("C", 1:4), paste0("ID", 1:4))
  )
}

#' Rótulo legível de um fator
#'
#' @param x Códigos de fator (ATT, NS, CP, CONF, INT).
#' @return Vetor de rótulos.
#' @export
factor_label <- function(x) {
  k <- warley_constructs()
  unname(stats::setNames(k$rotulo, k$fator)[x])
}

#' Construto teórico de cada item
#'
#' @return Vetor nomeado item -> código do fator.
#' @export
item_factor <- function() {
  k <- warley_constructs()
  stats::setNames(rep(k$fator, lengths(k$itens)), unlist(k$itens))
}

#' Enunciados dos itens, extraídos do cabeçalho do questionário exportado
#'
#' @return Vetor nomeado item -> enunciado.
#' @export
item_wording <- function() {
  c(
    A1 = "Doar dinheiro para escolas públicas estaduais é algo positivo.",
    A2 = "Doar dinheiro para escolas públicas estaduais é uma forma importante de apoiar a educação.",
    A3 = "Doar dinheiro para escolas públicas estaduais pode trazer benefícios para os estudantes.",
    A4 = "Contribuir financeiramente com escolas públicas estaduais é uma atitude socialmente relevante.",
    NS1 = "Pessoas importantes para mim aprovariam que eu fizesse uma doação financeira para uma escola pública estadual.",
    NS2 = "Minha família apoiaria a decisão de doar dinheiro para uma escola pública estadual.",
    NS3 = "Pessoas próximas a mim considerariam correta a decisão de contribuir financeiramente com uma escola pública estadual.",
    NS4 = "Pessoas que influenciam minhas decisões apoiariam uma doação financeira para escolas públicas estaduais.",
    CP1 = "Eu teria condições de realizar uma pequena doação financeira para uma escola pública estadual, se desejasse.",
    CP2 = "Para mim, seria fácil realizar uma doação financeira para uma escola pública estadual.",
    CP3 = "Eu saberia como buscar informações para realizar uma doação financeira para uma escola pública estadual.",
    CP4 = "A realização de uma doação financeira para uma escola pública estadual dependeria principalmente da minha decisão.",
    C1 = "Eu confio que uma escola pública estadual utilizaria adequadamente os recursos recebidos por meio de doações.",
    C2 = "Eu acredito que os recursos doados a uma escola pública estadual seriam aplicados em benefício dos estudantes.",
    C3 = "Eu confio que a escola pública estadual prestaria contas sobre o uso dos recursos recebidos por doação.",
    C4 = "Eu acredito que a gestão da escola pública estadual teria responsabilidade no uso dos recursos doados.",
    ID1 = "Eu pretendo realizar uma doação financeira para uma escola pública estadual no futuro.",
    ID2 = "É provável que eu faça uma doação financeira para uma escola pública estadual quando tiver oportunidade.",
    ID3 = "Eu consideraria seriamente contribuir financeiramente com uma escola pública estadual.",
    ID4 = "Caso houvesse uma campanha transparente e voluntária, eu teria intenção de doar para uma escola pública estadual."
  )
}

#' Converter itens para fatores ordenados 1-7
#'
#' @param data Data frame com os itens.
#' @param items Itens a converter.
#' @return Data frame com itens como `ordered`.
#' @export
as_ordinal <- function(data, items = warley_items()) {
  out <- as.data.frame(data)
  for (v in items) out[[v]] <- ordered(out[[v]])
  out
}

#' Garantir diretório e devolver caminho
#'
#' @param ... Partes do caminho.
#' @return Caminho criado.
#' @export
out_path <- function(...) {
  p <- file.path(...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}
