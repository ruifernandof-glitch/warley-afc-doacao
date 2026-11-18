# Publicação no GitHub e DOI no Zenodo

1. **Conferir o que vai ao público.** `git status` não deve listar nada em `data/` nem `.rds`.
   O `.gitignore` já exclui esses caminhos.
2. **Criar o repositório** público no GitHub (ex.: `warley-afc-doacao`) e enviar:
   `git remote add origin git@github.com:<usuario>/warley-afc-doacao.git && git push -u origin main`.
3. **Ligar ao Zenodo.** Entrar em https://zenodo.org com a conta GitHub → menu do usuário →
   *GitHub* → *Sync now* → ativar o interruptor do repositório.
4. **Revisar metadados** em `.zenodo.json` (autores, ORCID, palavras-chave) antes da release.
5. **Criar a release** no GitHub: *Releases* → *Draft a new release* → tag `v1.0.0` → título
   "Versão 1.0.0 — AFC ordinal" → *Publish release*.
6. **Obter o DOI.** Em poucos minutos o Zenodo arquiva a release e exibe o DOI da versão e o
   *concept DOI* (que aponta sempre para a última versão). Use o concept DOI na dissertação.
7. **Atualizar** `CITATION.cff` (campo `doi`) e o `README.md` com o DOI e criar nova release
   (`v1.0.1`) se quiser que o arquivo citado já contenha o DOI.
8. Cada nova release gera automaticamente um novo DOI de versão.
