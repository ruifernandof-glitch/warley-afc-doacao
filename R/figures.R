#' Paleta e tema gráfico do compêndio
#'
#' @return Lista de cores.
#' @export
viz_palette <- function() {
  list(ink = "#0b0b0b", ink2 = "#52514e", grid = "#e4e3df", surface = "#fcfcfb",
       blue = "#2a78d6", neutral = "#f0efec",
       likert = c("#b83232", "#e34948", "#f2a3a2", "#d9d8d3", "#86b6ef", "#3987e5", "#1c5cab"),
       seq = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
       div = c("#b83232", "#f2a3a2", "#f0efec", "#86b6ef", "#1c5cab"))
}

#' Tema ggplot discreto para figuras de relatório
#'
#' @param base Tamanho base da fonte.
#' @return Tema ggplot2.
#' @export
theme_report <- function(base = 11) {
  pal <- viz_palette()
  ggplot2::theme_minimal(base_size = base, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = pal$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = pal$ink2),
      axis.title = ggplot2::element_text(colour = pal$ink2),
      strip.text = ggplot2::element_text(colour = pal$ink, face = "bold", hjust = 0),
      plot.title = ggplot2::element_text(colour = pal$ink, face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = pal$ink2),
      legend.position = "top", legend.title = ggplot2::element_text(colour = pal$ink2)
    )
}

#' Figura: distribuição das respostas por item (barras divergentes)
#'
#' @param freq Saída de [item_frequencies()].
#' @param n Tamanho amostral.
#' @return Objeto ggplot.
#' @export
plot_likert <- function(freq, n) {
  pal <- viz_palette()
  long <- tidyr::pivot_longer(freq, dplyr::starts_with("cat_"), names_to = "cat", values_to = "n") |>
    dplyr::mutate(cat = as.integer(sub("cat_", "", cat)), pct = 100 * n / !!n,
                  item = factor(item, levels = rev(warley_items())),
                  construto = factor(factor_label(item_factor()[as.character(item)]),
                                     levels = warley_constructs()$rotulo))
  neg <- long |> dplyr::filter(cat <= 4) |>
    dplyr::mutate(pct = ifelse(cat == 4, pct / 2, pct), pct = -pct,
                  cat = factor(cat, levels = c(1, 2, 3, 4)))
  pos <- long |> dplyr::filter(cat >= 4) |>
    dplyr::mutate(pct = ifelse(cat == 4, pct / 2, pct), cat = factor(cat, levels = c(7, 6, 5, 4)))
  lab <- as.character(1:7)
  ggplot2::ggplot() +
    ggplot2::geom_col(data = neg, ggplot2::aes(x = pct, y = item, fill = cat), width = 0.7,
                      colour = "white", linewidth = 0.3) +
    ggplot2::geom_col(data = pos, ggplot2::aes(x = pct, y = item, fill = cat), width = 0.7,
                      colour = "white", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.4) +
    ggplot2::facet_grid(construto ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_fill_manual(values = stats::setNames(pal$likert, 1:7), breaks = as.character(1:7),
                               labels = lab, name = "Categoria (1 = discordo totalmente; 7 = concordo totalmente)") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-90, 90), breaks = seq(-80, 80, 20)) +
    ggplot2::labs(x = "Percentual de respondentes (categoria 4 dividida ao meio)", y = NULL) +
    theme_report(9) + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top")) +
    ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 90, hjust = 0.5), strip.placement = "outside")
}

#' Figura: cargas padronizadas com IC95%
#'
#' @param loadings Saída de [loadings_table()].
#' @return Objeto ggplot.
#' @export
plot_loadings <- function(loadings) {
  pal <- viz_palette()
  d <- loadings |> dplyr::mutate(item = factor(item, levels = rev(warley_items())),
                                 construto = factor(construto, levels = warley_constructs()$rotulo))
  ggplot2::ggplot(d, ggplot2::aes(x = carga, y = item)) +
    
    ggplot2::geom_vline(xintercept = c(0.5, 0.7), linetype = "dashed", colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 1, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ic_inf, xmax = pmin(ic_sup, 1.05)), height = 0,
                            colour = pal$blue, linewidth = 0.6) +
    ggplot2::geom_point(colour = pal$blue, size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", carga)), nudge_y = 0.38, size = 2.8,
                       colour = pal$ink2) +
    ggplot2::facet_grid(construto ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_x_continuous(limits = c(0.4, 1.09), breaks = seq(0.4, 1, 0.1)) +
    ggplot2::labs(x = "Carga fatorial padronizada (IC95%)", y = NULL) +
    theme_report(10) + ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 90, hjust = 0.5), strip.placement = "outside")
}

#' Figura: matriz de correlações latentes (triângulo inferior) e HTMT (superior)
#'
#' @param fit Modelo principal.
#' @param disc Saída de [discriminant_table()].
#' @return Objeto ggplot.
#' @export
plot_discriminant <- function(fit, disc) {
  pal <- viz_palette()
  f <- warley_constructs()$fator
  clv <- lavaan::lavInspect(fit, "cor.lv")[f, f]
  ht <- disc$htmt[f, f]
  g <- expand.grid(l = f, c = f, stringsAsFactors = FALSE)
  g$i <- match(g$l, f); g$j <- match(g$c, f)
  g$valor <- ifelse(g$i > g$j, clv[cbind(g$i, g$j)], ifelse(g$i < g$j, ht[cbind(g$j, g$i)], NA))
  g$tipo <- ifelse(g$i > g$j, "Correlação latente", ifelse(g$i < g$j, "HTMT", "Diagonal"))
  g$lab <- ifelse(is.na(g$valor), "", sprintf("%.2f", g$valor))
  g$l <- factor(factor_label(g$l), levels = rev(warley_constructs()$rotulo))
  g$c <- factor(factor_label(g$c), levels = warley_constructs()$rotulo)
  ggplot2::ggplot(g, ggplot2::aes(c, l, fill = valor)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = lab, colour = valor > 0.45), size = 3.4, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = pal$ink)) +
    ggplot2::scale_fill_gradientn(colours = pal$seq, limits = c(0, 1), na.value = pal$neutral,
                                  name = "Valor") +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_report(10) + ggplot2::theme(panel.grid = ggplot2::element_blank(),
                                      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Figura: resíduos de correlação
#'
#' @param resid Saída de [residual_diagnostics()].
#' @return Objeto ggplot.
#' @export
plot_residuals <- function(resid) {
  pal <- viz_palette()
  m <- resid$matriz[warley_items(), warley_items()]
  g <- expand.grid(i = warley_items(), j = warley_items(), stringsAsFactors = FALSE)
  g$v <- m[cbind(g$i, g$j)]
  g$v[match(g$i, warley_items()) <= match(g$j, warley_items())] <- NA
  g$i <- factor(g$i, levels = rev(warley_items())); g$j <- factor(g$j, levels = warley_items())
  g$lab <- ifelse(!is.na(g$v) & abs(g$v) > resid$corte, sprintf("%.2f", g$v), "")
  ggplot2::ggplot(g, ggplot2::aes(j, i, fill = v)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = lab), size = 2.6, colour = pal$ink) +
    ggplot2::scale_fill_gradientn(colours = pal$div, limits = c(-0.2, 0.2), na.value = "white",
                                  oob = scales::squish, name = "Resíduo") +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = sprintf("Valores rotulados quando |resíduo| > %.2f", resid$corte)) +
    theme_report(9) + ggplot2::theme(panel.grid = ggplot2::element_blank(),
                                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
}

#' Figura: qualidade de resposta (blocos invariantes por respondente)
#'
#' @param qual Saída de [response_quality()].
#' @return Objeto ggplot.
#' @export
plot_response_quality <- function(qual) {
  pal <- viz_palette()
  d <- as.data.frame(table(factor(qual$casos$blocos_invariantes, levels = 0:5)))
  names(d) <- c("blocos", "n")
  ggplot2::ggplot(d, ggplot2::aes(blocos, n)) +
    ggplot2::geom_col(fill = pal$blue, width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4, size = 3.2, colour = pal$ink2) +
    ggplot2::labs(x = "Número de blocos de quatro itens respondidos com a mesma categoria",
                  y = "Respondentes") +
    theme_report(10)
}

#' Figura: diagrama do modelo de mensuração com cargas padronizadas
#'
#' Desenhado em ggplot2 para manter os rótulos legíveis; as correlações
#' entre fatores são apresentadas na Figura de validade discriminante.
#'
#' @param loadings Saída de [loadings_table()].
#' @return Objeto ggplot.
#' @export
plot_path_diagram <- function(loadings) {
  pal <- viz_palette()
  k <- warley_constructs()
  it <- tibble::tibble(item = warley_items(), fator = unname(item_factor()[warley_items()])) |>
    dplyr::mutate(bloco = match(fator, k$fator), pos = seq_along(item) + (bloco - 1) * 0.8)
  fx <- it |> dplyr::group_by(fator, bloco) |> dplyr::summarise(x = mean(pos), .groups = "drop") |>
    dplyr::mutate(rotulo = sub(" ", "\n", factor_label(fator)))
  e <- it |> dplyr::left_join(fx[, c("fator", "x")], by = "fator") |>
    dplyr::left_join(loadings[, c("item", "carga")], by = "item") |>
    dplyr::mutate(lx = x + 0.72 * (pos - x), ly = 2.6 - 0.72 * 2.05)
  circle <- do.call(rbind, lapply(seq_len(nrow(fx)), function(i) {
    t <- seq(0, 2 * pi, length.out = 100)
    data.frame(fator = fx$fator[i], x = fx$x[i] + 1.05 * cos(t), y = 2.75 + 0.42 * sin(t))
  }))
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = e, ggplot2::aes(x = x, y = 2.33, xend = pos, yend = 0.32),
                          colour = pal$ink2, linewidth = 0.4,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.08, "in"), type = "closed")) +
    ggplot2::geom_label(data = e, ggplot2::aes(x = lx, y = ly, label = sub("^0", "", sprintf("%.2f", carga))),
                        size = 2.6, label.size = 0, fill = "white", colour = pal$ink, label.padding = ggplot2::unit(0.06, "lines")) +
    ggplot2::geom_polygon(data = circle, ggplot2::aes(x, y, group = fator), fill = "#cde2fb", colour = pal$ink, linewidth = 0.4) +
    ggplot2::geom_text(data = fx, ggplot2::aes(x, 2.75, label = rotulo), size = 3, fontface = "bold", colour = pal$ink, lineheight = 0.85) +
    ggplot2::geom_tile(data = it, ggplot2::aes(pos, 0.12), width = 0.86, height = 0.4, fill = pal$neutral, colour = pal$ink, linewidth = 0.3) +
    ggplot2::geom_text(data = it, ggplot2::aes(pos, 0.12, label = item), size = 2.9, colour = pal$ink) +
    ggplot2::coord_cartesian(xlim = c(0.3, max(it$pos) + 0.7), ylim = c(-0.2, 3.3), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

#' Salvar todas as figuras em PNG (300 dpi) e PDF
#'
#' @param res Lista de resultados do pipeline.
#' @param dest Pasta de destino.
#' @return Vetor de caminhos gerados.
#' @export
save_figures <- function(res, dest = "outputs/figures") {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  figs <- list(
    fig1_distribuicao_itens = list(plot_likert(res$freq, nrow(res$data)), 8, 7.5),
    fig2_qualidade_resposta = list(plot_response_quality(res$qual), 7, 4),
    fig3_cargas_ic95 = list(plot_loadings(res$loadings), 7, 7.5),
    fig4_discriminante = list(plot_discriminant(res$fit, res$disc), 6.5, 5.5),
    fig5_residuos = list(plot_residuals(res$resid), 7.5, 7),
    fig6_diagrama_modelo = list(plot_path_diagram(res$loadings), 11, 4.2)
  )
  out <- character()
  for (nm in names(figs)) {
    f <- figs[[nm]]
    for (ext in c("png", "pdf")) {
      p <- file.path(dest, paste0(nm, ".", ext))
      ggplot2::ggsave(p, f[[1]], width = f[[2]], height = f[[3]], dpi = 300, bg = "white")
      out <- c(out, p)
    }
  }
  for (ext in c("png", "pdf")) {
    p <- file.path(dest, paste0("fig6b_diagrama_semplot.", ext))
    if (ext == "png") grDevices::png(p, width = 12, height = 6, units = "in", res = 300) else grDevices::pdf(p, width = 12, height = 6)
    semPlot::semPaths(res$fit, what = "std", whatLabels = "std", layout = "tree", rotation = 1,
                      residuals = FALSE, intercepts = FALSE, thresholds = FALSE, exoCov = FALSE,
                      nCharNodes = 0, sizeLat = 10, sizeMan = 6.2, edge.label.cex = 0.95,
                      edge.label.position = 0.62, edge.color = "#52514e", fade = FALSE, esize = 1.2,
                      asize = 1.6, color = list(lat = "#cde2fb", man = "#f0efec"),
                      mar = c(4, 2, 6, 2), label.cex = 1.15)
    grDevices::dev.off(); out <- c(out, p)
  }
  out
}
