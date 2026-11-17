# ======================================================================
# TCC - ROBUSTEZ DO MODELO PRINCIPAL (REV6)
# Base: Dados_TCC_Painel_Modelagem_Rev5.xlsx
#
# Modelo principal: desempenho ponderado (pontos x peso 4/3/2/1/0),
# efeitos fixos de clube e ano, erros agrupados por clube, e estudo de
# evento de Sun e Abraham (2021).
#
# Robustezes:
#  R1 Aproveitamento ponderado: (pontos / pontos possiveis) x peso
#  R2 Ranking nacional: A = 1-20, B = 21-40, C = 41-60, D = 61-124,
#     sem divisao nacional = 125 (valores menores = melhor desempenho)
#  R3 Sem o Red Bull Bragantino
#  R4 Regra anterior: tratamento no primeiro ano-calendario completo
#     apos a constituicao da SAF
#  R5 Amostra fixada pela divisao de 2021 (clubes nas Series A, B ou C
#     em 2021), que nao depende de acessos posteriores
#  R6 Controle de divida liquida, com e sem as observacoes de divida zero
#  R7 Todos os clubes que disputaram alguma divisao nacional (A a D) em
#     2021-2025: 218 clubes (aba Painel_Ampliado)
#  R8 Populacao de 2021: clubes nas Series A, B, C ou D em 2021 (124),
#     definida antes de quase todas as adocoes
#  R9 Sensibilidade aos pesos das divisoes (A/B/C/D): 2/1/0/0 (artigo),
#     6/4/2/1, 8/4/2/1 e 1/1/1/1 (somente pontos)
# ======================================================================

options(scipen = 999)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(fixest); library(openxlsx)
})

pasta_tcc <- if (dir.exists("dados")) normalizePath(".") else if (dir.exists("../dados")) normalizePath("..") else stop("Execute a partir da raiz do repositorio (pasta que contem 'dados').")
arquivo_padrao <- file.path(pasta_tcc, "dados", "Dados_TCC_Painel_Modelagem_Rev5.xlsx")
pasta_resultados <- file.path(pasta_tcc, "resultados")
arquivo <- if (file.exists(arquivo_padrao)) arquivo_padrao else file.choose()
setwd(if (dir.exists(pasta_resultados)) pasta_resultados else dirname(normalizePath(arquivo)))

# Jogos por clube na fase usada para os pontos de cada divisao.
jogos_divisao <- function(serie, ano) {
  dplyr::case_when(
    serie %in% c("A", "B") ~ 38,
    serie == "C" & ano == 2021 ~ 18,   # dois grupos de 10, ida e volta
    serie == "C" ~ 19,                 # turno unico com 20 clubes
    serie == "D" ~ 14,                 # fase de grupos
    TRUE ~ NA_real_
  )
}

base <- read_excel(arquivo, sheet = "Painel") %>%
  mutate(
    Ano = as.integer(Ano),
    SAF = as.integer(SAF),
    Ano_SAF = as.numeric(Ano_SAF),
    Pontos = as.numeric(Pontos_Brasileirao),
    Posicao = as.numeric(Posicao_Brasileirao),
    Peso = as.numeric(Peso_Divisao),
    Divida = as.numeric(Divida_Total),
    Y = ifelse(Peso == 0, 0, Pontos * Peso),
    Jogos = jogos_divisao(Serie_Brasileirao, Ano),
    Aproveitamento = 100 * Pontos / (3 * Jogos),
    Y_Aprov = ifelse(Peso == 0, 0, Aproveitamento * Peso),
    Ranking = case_when(
      Serie_Brasileirao == "A" ~ Posicao,
      Serie_Brasileirao == "B" ~ 20 + Posicao,
      Serie_Brasileirao == "C" ~ 40 + Posicao,
      Serie_Brasileirao == "D" ~ 60 + Posicao,
      TRUE ~ 125
    ),
    log_Divida = ifelse(!is.na(Divida) & Divida >= 0, log1p(Divida), NA_real_)
  )

if (any(base$Aproveitamento > 100, na.rm = TRUE)) stop("Aproveitamento acima de 100%.")
if (any(base$Ranking[base$Serie_Brasileirao == "D"] > 124, na.rm = TRUE)) stop("Posicao da Serie D acima de 64.")

# Regra anterior: primeiro ano-calendario completo apos a constituicao.
ajuste <- read_excel(arquivo, sheet = "Datas_SAF")
datas <- ajuste[["Data de constitui\u00e7\u00e3o formal"]]
datas <- if (inherits(datas, "POSIXct") || inherits(datas, "Date")) as.Date(datas) else as.Date(as.character(datas), "%d/%m/%Y")
regra_antiga <- data.frame(Clube = ajuste$Clube, Ano_SAF_antigo = as.numeric(format(datas, "%Y")) + 1)
if (anyNA(regra_antiga$Ano_SAF_antigo)) stop("Data de constituicao nao lida na aba Datas_SAF.")
base <- base %>%
  left_join(regra_antiga, by = "Clube") %>%
  mutate(SAF_antigo = as.integer(!is.na(Ano_SAF_antigo) & Ano >= Ano_SAF_antigo))

clubes_2021 <- base %>% filter(Ano == 2021, Serie_Brasileirao %in% c("A", "B", "C")) %>% pull(Clube)
cat("Clubes nas Series A, B ou C em 2021:", length(clubes_2021), "\n")

muda <- function(d, saf) {
  d %>% group_by(Clube) %>% summarise(v = n_distinct(.data[[saf]]) > 1, .groups = "drop") %>% pull(v) %>% sum()
}

coefs_evento <- list()
roda <- function(nome, d, y, saf = "SAF", ano_saf = "Ano_SAF", controle = NULL, evento = TRUE) {
  rhs <- paste(c(saf, controle), collapse = " + ")
  m <- feols(as.formula(paste0(y, " ~ ", rhs, " | Clube + Ano")), data = d, vcov = ~Clube, notes = FALSE)
  ct <- coeftable(m)
  usados <- d[obs(m), ]
  out <- data.frame(
    Especificacao = nome, Desfecho = y, N = nobs(m), Clubes = n_distinct(usados$Clube),
    Clubes_que_mudam = muda(usados, saf),
    SAF = ct[saf, 1], EP = ct[saf, 2], p = ct[saf, 4],
    ATT = NA_real_, EP_ATT = NA_real_, p_ATT = NA_real_,
    F_pre = NA_real_, gl1 = NA_real_, gl2 = NA_real_, p_pre = NA_real_
  )
  if (!is.null(controle)) {
    out$Controle <- controle; out$Coef_controle <- ct[controle, 1]
    out$EP_controle <- ct[controle, 2]; out$p_controle <- ct[controle, 4]
  }
  if (evento) {
    d$Coorte <- ifelse(is.na(d[[ano_saf]]), 10000, d[[ano_saf]])
    me <- feols(as.formula(paste0(y, " ~ sunab(Coorte, Ano, ref.p = -1) | Clube + Ano")),
                data = d, vcov = ~Clube, notes = FALSE)
    att <- coeftable(summary(me, agg = "att"))
    w <- wald(me, keep = "^Ano::-", print = FALSE)
    out$ATT <- att[1, 1]; out$EP_ATT <- att[1, 2]; out$p_ATT <- att[1, 4]
    out$F_pre <- w[["stat"]]; out$gl1 <- w[["df1"]]; out$gl2 <- w[["df2"]]; out$p_pre <- w[["p"]]
    ce <- as.data.frame(coeftable(me))
    coefs_evento[[nome]] <<- cbind(Especificacao = nome, Termo = rownames(ce), ce, row.names = NULL)
  }
  out
}

pesos_alt <- function(serie, pts, pa, pb, pc, pd) {
  w <- case_when(serie == "A" ~ pa, serie == "B" ~ pb, serie == "C" ~ pc, serie == "D" ~ pd, TRUE ~ 0)
  ifelse(w == 0, 0, pts * w)
}
base <- base %>% mutate(
  Y_w2100 = pesos_alt(Serie_Brasileirao, Pontos, 2, 1, 0, 0),
  Y_w6421 = pesos_alt(Serie_Brasileirao, Pontos, 6, 4, 2, 1),
  Y_w8421 = pesos_alt(Serie_Brasileirao, Pontos, 8, 4, 2, 1),
  Y_w1111 = pesos_alt(Serie_Brasileirao, Pontos, 1, 1, 1, 1)
)

amp <- read_excel(arquivo, sheet = "Painel_Ampliado") %>%
  mutate(
    Ano = as.integer(Ano), SAF = as.integer(SAF), Ano_SAF = as.numeric(Ano_SAF),
    Pontos = as.numeric(Pontos_Brasileirao), Peso = as.numeric(Peso_Divisao),
    Y = ifelse(Peso == 0, 0, Pontos * Peso)
  )
if (n_distinct(amp$Clube) != 218 || nrow(amp) != 1090) stop("Painel_Ampliado com dimensao inesperada.")
chk <- amp %>% filter(Amostra_principal == 1) %>% select(Clube, Ano, Y) %>%
  inner_join(base %>% select(Clube, Ano, Y0 = Y), by = c("Clube", "Ano"))
if (nrow(chk) != nrow(base) || any(abs(chk$Y - chk$Y0) > 1e-9)) stop("Painel_Ampliado difere do Painel.")
amp_2021 <- amp %>% filter(Populacao_2021 == 1)
cat("Painel ampliado:", n_distinct(amp$Clube), "clubes | Populacao 2021:", n_distinct(amp_2021$Clube), "clubes\n")

base_sem_rb <- base %>% filter(Clube != "Red Bull Bragantino")
base_2021 <- base %>% filter(Clube %in% clubes_2021)
base_div <- base %>% filter(!is.na(log_Divida))
base_div_sem0 <- base_div %>% filter(Divida > 0)

resumo <- bind_rows(
  roda("Principal", base, "Y"),
  roda("R1 Aproveitamento ponderado", base, "Y_Aprov"),
  roda("R2 Ranking nacional", base, "Ranking"),
  roda("R3 Sem Red Bull Bragantino", base_sem_rb, "Y"),
  roda("R4 Regra anterior (ano completo)", base, "Y", saf = "SAF_antigo", ano_saf = "Ano_SAF_antigo"),
  roda("R5 Amostra fixada em 2021", base_2021, "Y"),
  roda("R6a Divida liquida - todas", base_div, "Y", controle = "log_Divida", evento = FALSE),
  roda("R6b Divida liquida - sem divida zero", base_div_sem0, "Y", controle = "log_Divida", evento = FALSE),
  roda("R7 Todos os clubes nacionais (A-D)", amp, "Y"),
  roda("R8 Populacao de 2021 (A-D em 2021)", amp_2021, "Y"),
  roda("R9a Pesos 2/1/0/0 (artigo)", base, "Y_w2100"),
  roda("R9b Pesos 6/4/2/1", base, "Y_w6421"),
  roda("R9c Pesos 8/4/2/1", base, "Y_w8421"),
  roda("R9d Pesos 1/1/1/1 (so pontos)", base, "Y_w1111")
)

cat("\n========== ROBUSTEZ ==========\n")
print(resumo[, c("Especificacao", "N", "Clubes", "Clubes_que_mudam", "SAF", "EP", "p",
                 "ATT", "EP_ATT", "p_ATT", "F_pre", "gl2", "p_pre")], digits = 4)
cat("\nDivida zero excluida:\n")
print(as.data.frame(base_div %>% filter(Divida == 0) %>% select(Clube, Ano, Serie_Brasileirao, SAF)))
cat("\nControle de divida:\n")
print(resumo[!is.na(resumo$Coef_controle), c("Especificacao", "N", "Coef_controle", "EP_controle", "p_controle")], digits = 4)

descr_y <- bind_rows(
  base %>% summarise(Amostra = "Principal", across(c(Y, Y_w2100, Y_w6421, Y_w8421, Y_w1111),
                     list(media = ~mean(.x), dp = ~sd(.x)))),
  amp %>% summarise(Amostra = "R7 Todos (A-D)", Y_media = mean(Y), Y_dp = sd(Y)),
  amp_2021 %>% summarise(Amostra = "R8 Populacao 2021", Y_media = mean(Y), Y_dp = sd(Y))
)
print(as.data.frame(descr_y), digits = 4)

descr <- base %>% summarise(
  Media_Y_Aprov = mean(Y_Aprov, na.rm = TRUE), DP_Y_Aprov = sd(Y_Aprov, na.rm = TRUE),
  Media_Ranking = mean(Ranking, na.rm = TRUE), DP_Ranking = sd(Ranking, na.rm = TRUE),
  N_Ranking = sum(!is.na(Ranking)), N_Y_Aprov = sum(!is.na(Y_Aprov)))
print(descr)

write.xlsx(
  list(Resumo = resumo, Coeficientes_evento = bind_rows(coefs_evento), Descritivas = descr, Descritivas_Y = descr_y,
       Regra_anterior = regra_antiga, Clubes_2021 = data.frame(Clube = clubes_2021)),
  "14_Robustez_Rev6.xlsx", overwrite = TRUE
)
cat("\nArquivo salvo: 14_Robustez_Rev6.xlsx\n")
