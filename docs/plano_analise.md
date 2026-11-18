# Plano de análise e estrutura do projeto

**Objetivo.** Verificar, de forma confirmatória, a estrutura de mensuração de 20 itens em cinco
fatores correlacionados (atitude A1–A4, norma subjetiva NS1–NS4, controle percebido CP1–CP4,
confiança C1–C4, intenção ID1–ID4), definida a priori pela teoria do comportamento planejado
(Ajzen, 1991, 2002) e pela literatura de confiança do doador (Sargeant & Lee, 2004).

**Hipóteses de mensuração.** H1: cinco fatores ajustam-se melhor que fator único e que três
modelos de quatro fatores. H2: cargas ≥ 0,70, ômega ≥ 0,70, AVE ≥ 0,50. H3: fatores
distinguíveis (Fornell-Larcker, HTMT < 0,85, IC95% de r abaixo de 1).

| Etapa | Tipo | Script | Saídas | Situação |
|---|---|---|---|---|
| 1. Construtos e indicadores | Obrigatória | 04_specify_model.R | Tabela 1 | Executada |
| 2. Especificação do modelo | Obrigatória | 04_specify_model.R | modelo_cinco_fatores.lav | Executada |
| 3. Exame preliminar | Obrigatória | 01, 02 | tab01–tab06, fig1–fig2 | Executada |
| 4. Amostra e estimador | Obrigatória | 03_sample_estimator.R | tab07–tab08 (Monte Carlo) | Executada |
| 5. Estimação e admissibilidade | Obrigatória | 05_estimate_cfa.R | tab09 | Executada (Heywood em A2 relatado) |
| 6. Ajuste global | Obrigatória | 06_fit_indices.R | tab10–tab11 | Executada |
| 7. Cargas e convergência | Obrigatória | 07_loadings_ave_cr.R | tab12, fig3 | Executada |
| 8. Confiabilidade e AVE | Obrigatória | 07_loadings_ave_cr.R | tab13 | Executada |
| 9. Discriminância | Obrigatória | 08_discriminant_validity.R | tab14, fig4 | Executada |
| 10. Diagnóstico e decisão | Boa prática essencial | 09, 10 | tab15–tab18, fig5–fig6 | Executada, sem reespecificação |

**Estimador.** WLSMV (DWLS + EP robustos + χ² ajustado por média e variância), itens ordinais,
parametrização *delta*. A *theta*, prevista no plano original, não produziu erros-padrão
(matriz de informação não invertível) e é mantida como verificação documentada.

**Regras de decisão por item (fixadas antes dos resultados).** Excluir: carga < 0,50 ou não
significativa, com fundamento de conteúdo. Revisar: 0,50 ≤ carga < 0,70; IC95% da carga
contendo 1; resíduo de correlação > 0,10; MI residual > 10. Manter: demais casos. "Revisar"
mantém o item nesta amostra e recomenda reformulação na próxima versão.
