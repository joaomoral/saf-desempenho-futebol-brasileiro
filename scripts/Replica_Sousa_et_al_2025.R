# ======================================================================
# TCC - REPLICA DE SOUSA ET AL. (2025) E PONTE ATE O MODELO ESTENDIDO
#
# Base: Dados_TCC_Painel_Modelagem_Rev5.xlsx (aba Painel)
#
# Replica: 26 clubes que passaram pela Serie A em 2022-2024,
# Y = pontos x peso (A = 2, B = 1, C = 0), como no artigo.
# O artigo usa ainda ln(valor do elenco) e sete indicadores contabeis.
# A base nao tem valor do elenco para 2022 e 2023 nem os indicadores
# contabeis; por isso a replica estima o efeito da SAF sem controles e,
# como alternativa, com ln(receita total).
#
# Ponte (todos com efeitos fixos de clube e ano, erros por clube):
#  M1 Replica com a classificacao SAF da Tabela 1 do artigo
#  M2 Replica com a classificacao da base (regra de junho, datas
#     documentadas, Red Bull Bragantino como nao SAF)
#  M3 Mesmos 26 clubes, 2021-2025
#  M4 75 clubes, 2021-2025, pesos do artigo
#  M5 75 clubes, 2021-2025, pesos A=4, B=3, C=2, D=1, sem divisao=0
# ======================================================================

options(scipen = 999)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(fixest)
  library(plm); library(lmtest); library(openxlsx)
})

pasta_tcc <- if (dir.exists("dados")) normalizePath(".") else if (dir.exists("../dados")) normalizePath("..") else stop("Execute a partir da raiz do repositorio (pasta que contem 'dados').")
arquivo_padrao <- file.path(pasta_tcc, "dados", "Dados_TCC_Painel_Modelagem_Rev5.xlsx")
pasta_resultados <- file.path(pasta_tcc, "resultados")
arquivo <- if (file.exists(arquivo_padrao)) arquivo_padrao else file.choose()
setwd(if (dir.exists(pasta_resultados)) pasta_resultados else dirname(normalizePath(arquivo)))

base <- read_excel(arquivo, sheet = "Painel") %>%
  mutate(
    Ano = as.integer(Ano),
    SAF = as.integer(SAF),
    Pontos = as.numeric(Pontos_Brasileirao),
    Peso_Base = as.numeric(Peso_Divisao),
    Peso_Artigo = case_when(
      Serie_Brasileirao == "A" ~ 2,
      Serie_Brasileirao == "B" ~ 1,
      TRUE ~ 0
    ),
    Y_Artigo = ifelse(Peso_Artigo == 0, 0, Pontos * Peso_Artigo),
    Y_Base = ifelse(Peso_Base == 0, 0, Pontos * Peso_Base),
    Receita = as.numeric(Receita_Total),
    log_Receita = ifelse(!is.na(Receita) & Receita >= 0, log1p(Receita), NA_real_)
  )

# Clubes que disputaram a Serie A em ao menos um ano de 2022 a 2024.
clubes_A <- base %>%
  filter(Ano %in% 2022:2024, Serie_Brasileirao == "A") %>%
  distinct(Clube) %>% pull(Clube)
cat("Clubes da replica:", length(clubes_A), "\n")
if (length(clubes_A) != 26) stop("Esperados 26 clubes da Serie A em 2022-2024.")

# Classificacao SAF da Tabela 1 de Sousa et al. (2025): ano a partir do
# qual o clube aparece como SAF no artigo.
saf_artigo <- c(
  "Am\u00e9rica-MG" = 2022, "Atl\u00e9tico-GO" = 2024, "Atl\u00e9tico-MG" = 2023,
  "Bahia" = 2023, "Botafogo-RJ" = 2022, "Coritiba" = 2023, "Cruzeiro" = 2022,
  "Cuiab\u00e1" = 2022, "Fortaleza" = 2024, "Red Bull Bragantino" = 2022,
  "Vasco" = 2022
)
if (!all(names(saf_artigo) %in% base$Clube)) {
  stop("Clube da Tabela 1 nao encontrado: ",
       paste(setdiff(names(saf_artigo), base$Clube), collapse = ", "))
}
base <- base %>%
  mutate(
    Ano_SAF_Artigo = unname(saf_artigo[as.character(Clube)]),
    SAF_Artigo = as.integer(!is.na(Ano_SAF_Artigo) & Ano >= Ano_SAF_Artigo)
  )

amostra <- function(clubes, anos) {
  base %>% filter(Clube %in% clubes, Ano %in% anos)
}
d_rep <- amostra(clubes_A, 2022:2024)
d_26 <- amostra(clubes_A, 2021:2025)
d_75 <- base

clubes_com_variacao <- function(d, saf) {
  d %>% group_by(Clube) %>%
    summarise(v = n_distinct(.data[[saf]]) > 1, .groups = "drop") %>%
    summarise(n = sum(v)) %>% pull(n)
}

roda <- function(nome, d, y, saf, controle = NULL) {
  rhs <- paste(c(saf, controle), collapse = " + ")
  f <- as.formula(paste0(y, " ~ ", rhs, " | Clube + Ano"))
  m <- feols(f, data = d, vcov = ~Clube, notes = FALSE)
  ct <- coeftable(m)
  usados <- d[obs(m), ]
  data.frame(
    Modelo = nome, Variavel_dependente = y, Classificacao_SAF = saf,
    Controle = ifelse(is.null(controle), "nenhum", controle),
    Termo = rownames(ct), Estimativa = ct[, 1], Erro_padrao = ct[, 2],
    t = ct[, 3], p_valor = ct[, 4], N = nobs(m),
    Clubes = n_distinct(usados$Clube),
    Clubes_que_mudam_SAF = clubes_com_variacao(usados, saf),
    R2_within = unname(r2(m, "wr2")), row.names = NULL
  )
}

modelos <- bind_rows(
  roda("M1 Replica - classificacao do artigo", d_rep, "Y_Artigo", "SAF_Artigo"),
  roda("M1b Replica - artigo + ln(receita)", d_rep, "Y_Artigo", "SAF_Artigo", "log_Receita"),
  roda("M2 Replica - classificacao da base", d_rep, "Y_Artigo", "SAF"),
  roda("M2b Replica - base + ln(receita)", d_rep, "Y_Artigo", "SAF", "log_Receita"),
  roda("M3 26 clubes, 2021-2025", d_26, "Y_Artigo", "SAF"),
  roda("M4 75 clubes, 2021-2025, pesos do artigo", d_75, "Y_Artigo", "SAF"),
  roda("M5 75 clubes, 2021-2025, pesos da base", d_75, "Y_Base", "SAF")
)
cat("\n========== EFEITO DA SAF NA REPLICA E NA PONTE ==========\n")
print(modelos[, c("Modelo", "Termo", "Estimativa", "Erro_padrao", "p_valor",
                  "N", "Clubes", "Clubes_que_mudam_SAF")], digits = 4)

# Sequencia de testes do artigo (pooled, FE, RE, F, Hausman) na replica.
testes_replica <- function(saf) {
  pd <- pdata.frame(as.data.frame(d_rep), index = c("Clube", "Ano"))
  f <- as.formula(paste0("Y_Artigo ~ ", saf))
  pooled <- plm(f, data = pd, model = "pooling")
  fe <- plm(f, data = pd, model = "within", effect = "twoways")
  re <- tryCatch(plm(f, data = pd, model = "random", effect = "twoways"),
                 error = function(e) plm(f, data = pd, model = "random", effect = "individual"))
  tF <- pFtest(fe, pooled)
  tH <- phtest(fe, re)
  cp <- coeftest(pooled)
  cf <- coeftest(fe)
  data.frame(
    Classificacao_SAF = saf,
    SAF_pooled = cp[saf, 1], p_pooled = cp[saf, 4],
    SAF_FE_sem_cluster = cf[saf, 1], EP_FE_sem_cluster = cf[saf, 2], p_FE_sem_cluster = cf[saf, 4],
    F_pooled_vs_FE = unname(tF$statistic), p_F = tF$p.value,
    Hausman = unname(tH$statistic), gl_Hausman = unname(tH$parameter), p_Hausman = tH$p.value
  )
}
testes <- bind_rows(testes_replica("SAF_Artigo"), testes_replica("SAF"))
cat("\n========== TESTES DA REPLICA ==========\n")
print(testes, digits = 4)

classificacao <- d_rep %>%
  select(Clube, Ano, Serie_Brasileirao, Pontos, Peso_Artigo, Y_Artigo, SAF_Artigo, SAF) %>%
  arrange(Clube, Ano)
divergencias <- classificacao %>% filter(SAF_Artigo != SAF)
cat("\nClube-anos com classificacao SAF diferente do artigo:", nrow(divergencias), "\n")
print(as.data.frame(divergencias))

descritivas <- d_rep %>%
  summarise(N = n(), Clubes = n_distinct(Clube),
            Media_Y = mean(Y_Artigo), DP_Y = sd(Y_Artigo),
            Min_Y = min(Y_Artigo), Max_Y = max(Y_Artigo),
            Obs_com_receita = sum(!is.na(log_Receita)))
print(descritivas)

write.xlsx(
  list(Modelos = modelos, Testes_replica = testes,
       Classificacao_SAF = classificacao, Divergencias = divergencias,
       Descritivas_replica = descritivas),
  "13_Replica_Sousa_et_al.xlsx", overwrite = TRUE
)
cat("\nArquivo salvo: 13_Replica_Sousa_et_al.xlsx\n")
