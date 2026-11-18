# AFC ordinal — intenção de doação financeira a escolas públicas estaduais (MG)

Compêndio de pesquisa da etapa confirmatória da dissertação de **Warley Borges Silva**
(orientação: Prof. Dr. Rui Fernando Correia Ferreira). Executa as 10 etapas do protocolo de
análise fatorial confirmatória (AFC) para 20 itens em cinco fatores — atitude, norma subjetiva,
controle percebido, confiança e intenção de doar — com estimador WLSMV para dados ordinais, e
gera automaticamente todas as tabelas, figuras e o relatório final em Word.

> **Dados individuais não fazem parte do repositório público.** A combinação de faixa etária,
> escolaridade, renda e vínculo escolar pode permitir reidentificação. As pastas `data/raw/` e
> `data/processed/` e os objetos `outputs/models/*.rds` estão no `.gitignore`.

## Estrutura

```
data/raw/            respostas_original.xlsx (privado; não versionado)
data/processed/      dados_analiticos.csv (gerado; não versionado)
R/                   funções documentadas (roxygen2): importação, descritivas, modelo,
                     estimação, ajuste, validade, diagnóstico, decisão por item, figuras, exportação
analysis/            01_import_clean.R ... 11_figures.R, run_all.R, bootstrap_renv.R
_targets.R           pipeline targets (recalcula só o que mudou)
outputs/tables/      tab01...tab19 em .csv e .docx
outputs/figures/     fig1...fig6 em .png (300 dpi) e .pdf
outputs/models/      modelo_cinco_fatores.lav (sintaxe a priori); .rds locais
manuscript/          index.qmd (relatório), references.bib, apa.csl, reference.docx,
                     check_paragraphs.py (verifica parágrafos de 7 a 11 linhas)
docs/                plano de análise, decisões analíticas, codebook, reprodutibilidade, Zenodo
```

## Como executar

Requisitos: R ≥ 4.3, Quarto ≥ 1.4, localidade UTF-8.

```r
# 1. Dependências nas versões registradas
source("analysis/bootstrap_renv.R")      # instala renv e restaura renv.lock

# 2. Colocar o arquivo bruto em data/raw/respostas_original.xlsx (ou .csv)

# 3. Pipeline completo (inclui o relatório Word)
targets::tar_make()

#    alternativa sequencial, sem targets:
source("analysis/run_all.R")
system("quarto render manuscript/index.qmd --to docx")
```

Variáveis de ambiente opcionais: `WARLEY_RAW` (caminho do bruto) e `WARLEY_MC_REPS` (réplicas
de Monte Carlo nos scripts numerados; padrão 200). No Linux sem localidade UTF-8, execute com
`LC_ALL=C.UTF-8`.

Verificação dos parágrafos (após converter o .docx em PDF com LibreOffice):

```bash
soffice --headless --convert-to pdf manuscript/index.docx --outdir manuscript
python3 manuscript/check_paragraphs.py manuscript/index.docx manuscript/index.pdf
```

## Principais resultados (N = 122)

| Índice | Cinco fatores | Fator único |
|---|---|---|
| χ² escalonado (gl) | 250,06 (160) | 1443,24 (170) |
| CFI / TLI | 0,998 / 0,997 | 0,966 / 0,962 |
| RMSEA [IC90%] | 0,068 [0,051–0,084] | 0,249 [0,237–0,261] |
| SRMR | 0,039 | 0,220 |

Cargas 0,73–1,00; ômega 0,90–0,97; AVE 0,73–0,93; HTMT máximo 0,73. Caso de Heywood em A2
(fronteira; IC95% contém 1). Detalhes e decisões em `manuscript/index.docx`.

## Publicar no GitHub e gerar DOI no Zenodo

Passo a passo em [`docs/zenodo.md`](docs/zenodo.md). Resumo: (1) criar repositório público
sem dados; (2) em zenodo.org → *GitHub*, ativar o repositório; (3) criar uma *release* `v1.0.0`
no GitHub; (4) o Zenodo arquiva a release e emite o DOI; (5) inserir o DOI em `CITATION.cff`
e neste README.

## Dissertação associada

Silva, W. B. (2026). *Modelo de mensuração da intenção de doação financeira para escolas
públicas: validação no contexto das Escolas Estaduais de Minas Gerais* [Dissertação de
Mestrado, Centro Universitário Unihorizontes]. Programa de Pós-Graduação em Administração.
Orientador: Prof. Dr. Rui Fernando Correia Ferreira.

Coleta por questionário eletrônico (3 ago. a 16 out. 2026), escolas da SRE Metropolitana B;
134 registros, 122 casos analíticos (58 ex-alunos, 64 pais/responsáveis). Instrumento no
Apêndice C da dissertação e em `docs/codebook.md`.

## Como citar

Silva, W. B., & Ferreira, R. F. C. (2026). *Compêndio de pesquisa: análise fatorial
confirmatória ordinal da intenção de doação financeira a escolas públicas estaduais de Minas
Gerais* (Versão 1.0.0) [Software]. Zenodo. https://doi.org/10.5281/zenodo.XXXXXXX

## Licença

Código sob licença MIT (`LICENSE`). Textos e figuras: CC BY 4.0, salvo indicação em contrário.
