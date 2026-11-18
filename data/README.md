# Dados

Os dados individuais da pesquisa **não são publicados** neste repositório.

A combinação de faixa etária, escolaridade, renda e vínculo escolar dos 122 respondentes
poderia permitir reidentificação, e o TCLE (Apêndice B da dissertação) garantiu
confidencialidade, anonimato e análise apenas agregada. Por isso, `data/raw/` e `data/processed/` estão no `.gitignore`.

Para reproduzir a análise, coloque o arquivo bruto em `data/raw/respostas_original.xlsx`
(ou `.csv`, ou indique o caminho na variável de ambiente `WARLEY_RAW`) e execute
`targets::tar_make()`. A estrutura das variáveis está em [`docs/codebook.md`](../docs/codebook.md).

Todos os resultados agregados (tabelas e figuras) estão em [`outputs/`](../outputs/).
