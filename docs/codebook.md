# Codebook — `data/processed/dados_analiticos.csv`

| Variável | Tipo | Valores | Origem no formulário |
|---|---|---|---|
| id_anonimo | texto | R001–R134 (ordem da exportação) | gerado |
| A1–A4 | ordinal | 1–7 | Seção 4 — Atitude |
| NS1–NS4 | ordinal | 1–7 | Seção 5 — Apoio de pessoas importantes |
| CP1–CP4 | ordinal | 1–7 | Seção 6 — Condições para realizar uma doação |
| C1–C4 | ordinal | 1–7 | Seção 7 — Confiança na escola pública estadual |
| ID1–ID4 | ordinal | 1–7 | Seção 8 — Intenção de realizar uma doação |
| faixa_etaria | categórica | 18 a 24 ... 65 ou mais | Faixa etária |
| escolaridade | categórica | 8 níveis | Escolaridade |
| renda_familiar | categórica | 5 níveis | Renda familiar |
| experiencia_escola_publica | categórica | Sim / Não | Experiência como estudante de escola pública |
| vinculo | categórica | ex-aluno / responsável | Vínculo com a escola pública estadual |
| conhecimento_doacao | categórica | Sim / Não / Não tenho certeza | Conhecimento prévio sobre doações via caixa escolar |
| participacao_acao | categórica | Sim / Não / Não me lembro | Participação anterior em arrecadação |

Escala: 1 Discordo totalmente, 2 Discordo, 3 Discordo parcialmente, 4 Nem concordo nem
discordo, 5 Concordo parcialmente, 6 Concordo, 7 Concordo totalmente. Enunciados completos em
`R/utils.R::item_wording()` e na Tabela 1 do relatório.
