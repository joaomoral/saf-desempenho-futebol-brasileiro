# Material de replicação: efeito da adoção da SAF no desempenho esportivo de clubes brasileiros

Código em R e base de dados do Trabalho de Conclusão de Curso sobre a adoção do modelo de Sociedade Anônima do Futebol (SAF, Lei nº 14.193/2021) e o desempenho esportivo dos clubes brasileiros (painel de 75 clubes, 2021–2025, 375 observações clube-ano).

## Estrutura

```
dados/       Dados_TCC_Painel_Modelagem_Rev5.xlsx
scripts/     scripts em R (todos usam caminhos relativos à raiz do repositório)
resultados/  planilhas, relatórios HTML e registros (logs) gerados pelos scripts
```

### Base de dados

`dados/Dados_TCC_Painel_Modelagem_Rev5.xlsx`, com as abas: `Painel`, `Painel_Ampliado`, `Serie_D_Fase_Grupos`, `Datas_SAF`, `Fontes_Financeiras`, `Dicionario` e `Metodologia`. A aba `Dicionario` descreve cada variável e a aba `Datas_SAF` traz as datas de constituição das SAFs e as fontes.

### Scripts

| Script | O que faz |
|---|---|
| `Script_Unificado_TCC_SAF_Robustez_Rev5.R` | Modelos de efeitos fixos com e sem variáveis financeiras, diagnósticos, testes econométricos, robustez (Poisson, dummies de divisão, desempenho relativo) e estudo de evento de Sun e Abraham |
| `Replica_Sousa_et_al_2025.R` | Réplica de Sousa et al. (Série A, 2022–2024), incluindo Hausman e Mundlak |
| `Robustez_Rev6.R` | Especificações alternativas de tratamento, amostra e controles |
| `HonestDiD_Rev7.R`, `HonestDiD_Rev7_grade_fina.R` | Sensibilidade de Rambachan e Roth (pacote HonestDiD), desempenho ponderado e ranking nacional |
| `HonestDiD_Rev8.R` | Sensibilidade de Rambachan e Roth para o índice hierárquico |
| `Complementar_Rev9c.R` | Índice hierárquico, ranking, amostras alternativas, Mundlak, wild cluster bootstrap, permutação, heterogeneidade por tipo de controle, sensibilidade do Athletic, ordenação alternativa da Série D, descritivas e poder mínimo detectável |

## Como executar

Requer R (versão 4.3 ou superior) e os pacotes `readxl`, `dplyr`, `fixest`, `openxlsx`, `plm` e `lmtest` (réplica) e `HonestDiD` (instalado automaticamente pelos scripts de sensibilidade, se ausente). O script unificado carrega seus pacotes adicionais no início do arquivo.

Execute cada script **a partir da raiz do repositório** (a pasta que contém `dados/`):

```bash
Rscript scripts/Script_Unificado_TCC_SAF_Robustez_Rev5.R
Rscript scripts/Replica_Sousa_et_al_2025.R
Rscript scripts/Robustez_Rev6.R
Rscript scripts/HonestDiD_Rev7.R
Rscript scripts/HonestDiD_Rev8.R
Rscript scripts/Complementar_Rev9c.R
```

Os resultados são gravados em `resultados/`, sobrescrevendo os arquivos existentes. As sementes aleatórias estão fixadas nos scripts que usam bootstrap e permutação. As versões dos pacotes usadas no script unificado estão em `resultados/00_Sessao_R.txt`.

O wild cluster bootstrap é usado como procedimento complementar de inferência com poucos clusters. O teste de permutação redistribui as trajetórias de tratamento entre clubes; como a adoção da SAF não foi aleatória, sua interpretação como teste de randomização exige uma hipótese de permutabilidade entre clubes que não é garantida. Por isso, os p-valores da permutação devem ser lidos apenas como uma checagem de sensibilidade, e não como evidência causal independente.

## Observações

- A classificação das SAFs por tipo de controle (investidor, associação, não classificada) vem de reportagens e é exploratória.
- A posição final geral da Série D, usada no ranking nacional, não foi confirmada em fonte oficial única; a alternativa por pontos da fase de grupos está na aba `Serie_D_Fase_Grupos`.
- A data de constituição da SAF do Athletic Club vem de notícia de anúncio, e a sensibilidade a essa escolha é avaliada nos scripts.
