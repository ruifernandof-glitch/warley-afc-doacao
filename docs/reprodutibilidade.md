# Reprodutibilidade

- Ambiente de execução desta versão: R 4.3.3, lavaan 0.6-17, semTools 0.5-6, psych 2.4.1,
  semPlot 1.1.6, targets 1.4.1, Quarto 1.5.57. Versões completas em `renv.lock`.
- MVN não estava disponível no ambiente; `normality_tests()` usa `MVN::mvn()` quando instalado
  e, caso contrário, `psych::mardia()` (o relatório informa qual foi usado).
- Sementes fixas: Monte Carlo `set.seed(20261028)`; `tar_option_set(seed = 20261028)`.
- `targets::tar_make()` reconstrói só os alvos afetados. `targets::tar_visnetwork()` mostra o grafo.
- Localidade UTF-8 obrigatória (os arquivos têm acentos). Em Linux: `LC_ALL=C.UTF-8`.
- O arquivo bruto usado nesta execução foi reconstruído da planilha do Google Forms
  (`respostas_original.xlsx`); o banco analítico resultante é idêntico, item a item e caso a
  caso, ao `dados_analiticos.csv` da versão anterior do compêndio.
- Checagem de formatação: `manuscript/check_paragraphs.py` conta as linhas de cada parágrafo no
  PDF (A4, Times 12, 1,5) e falha se algum ficar fora de 7–11 linhas.
