# ======================================================================
# TCC - ANALISES COMPLEMENTARES EM R (REV9)
# Indice hierarquico (IH), ranking nacional, amostras alternativas,
# teste de Mundlak, wild cluster bootstrap, permutacao, heterogeneidade,
# sensibilidade do Athletic, ordenacao alternativa da Serie D e
# estatisticas descritivas.
# Base: Dados_TCC_Painel_Modelagem_Rev5.xlsx
# ======================================================================
options(scipen = 999)
suppressPackageStartupMessages({ library(readxl); library(dplyr); library(fixest); library(openxlsx) })
pasta_tcc <- if (dir.exists("dados")) normalizePath(".") else if (dir.exists("../dados")) normalizePath("..") else stop("Execute a partir da raiz do repositorio (pasta que contem 'dados').")
arquivo_padrao <- file.path(pasta_tcc, "dados", "Dados_TCC_Painel_Modelagem_Rev5.xlsx")
arquivo <- if (file.exists(arquivo_padrao)) arquivo_padrao else file.choose()
pasta_resultados <- file.path(pasta_tcc, "resultados")
setwd(if (dir.exists(pasta_resultados)) pasta_resultados else dirname(normalizePath(arquivo)))
set.seed(20261117)

jogos_divisao <- function(serie, ano) case_when(
  serie %in% c("A", "B") ~ 38, serie == "C" & ano == 2021 ~ 18, serie == "C" ~ 19, serie == "D" ~ 14, TRUE ~ NA_real_)
constroi <- function(d) {
  if (!"Posicao_Brasileirao" %in% names(d)) d$Posicao_Brasileirao <- NA_real_
  d %>% mutate(
    Ano = as.integer(Ano), SAF = as.integer(SAF), Ano_SAF = as.numeric(Ano_SAF),
    Pontos = as.numeric(Pontos_Brasileirao), Posicao = as.numeric(Posicao_Brasileirao),
    Nivel = case_when(Serie_Brasileirao == "A" ~ 4, Serie_Brasileirao == "B" ~ 3,
                      Serie_Brasileirao == "C" ~ 2, Serie_Brasileirao == "D" ~ 1, TRUE ~ 0),
    Y = ifelse(Nivel == 0, 0, Pontos * Nivel),
    Y_art = ifelse(Serie_Brasileirao %in% c("A", "B"), Pontos * ifelse(Serie_Brasileirao == "A", 2, 1), 0),
    Jogos = jogos_divisao(Serie_Brasileirao, Ano),
    IH = ifelse(Nivel == 0, 0, Nivel + Pontos / (3 * Jogos)),
    off = case_when(Serie_Brasileirao == "A" ~ 0, Serie_Brasileirao == "B" ~ 20, Serie_Brasileirao == "C" ~ 40,
                    Serie_Brasileirao == "D" ~ 60, TRUE ~ NA_real_),
    Rk125 = ifelse(is.na(off), 125, off + Posicao), Rk150 = ifelse(is.na(off), 150, off + Posicao),
    Rk200 = ifelse(is.na(off), 200, off + Posicao))
}
base <- constroi(read_excel(arquivo, sheet = "Painel"))
amp  <- constroi(read_excel(arquivo, sheet = "Painel_Ampliado"))
stopifnot(nrow(base) == 375, n_distinct(amp$Clube) == 218, nrow(amp) == 1090)
cat("Painel:", n_distinct(base$Clube), "clubes,", nrow(base), "obs\n")

ajuste <- read_excel(arquivo, sheet = "Datas_SAF")
datas <- ajuste[["Data de constituição formal"]]
datas <- if (inherits(datas, "POSIXct") || inherits(datas, "Date")) as.Date(datas) else as.Date(as.character(datas), "%d/%m/%Y")
regra_antiga <- data.frame(Clube = ajuste$Clube, Ano_SAF_antigo = as.numeric(format(datas, "%Y")) + 1)
base <- base %>% left_join(regra_antiga, by = "Clube") %>%
  mutate(SAF_antigo = as.integer(!is.na(Ano_SAF_antigo) & Ano >= Ano_SAF_antigo))

muda <- function(d, saf) d %>% group_by(Clube) %>% summarise(v = n_distinct(.data[[saf]]) > 1, .groups = "drop") %>% pull(v) %>% sum()

# ---- ajuste padrao: TWFE + Sun-Abraham (mesmas chamadas do script Rev6) ----
roda <- function(nome, d, y, saf = "SAF", ano_saf = "Ano_SAF", evento = TRUE) {
  m <- feols(as.formula(paste0(y, " ~ ", saf, " | Clube + Ano")), data = d, vcov = ~Clube, notes = FALSE)
  ct <- coeftable(m)
  out <- data.frame(Especificacao = nome, Desfecho = y, N = nobs(m), Clubes = n_distinct(d$Clube),
    Clubes_que_mudam = muda(d, saf), SAF = ct[saf, 1], EP = ct[saf, 2], p = ct[saf, 4],
    ATT = NA_real_, EP_ATT = NA_real_, p_ATT = NA_real_, F_pre = NA_real_, p_pre = NA_real_)
  es <- NULL
  if (evento) {
    d$Coorte <- ifelse(is.na(d[[ano_saf]]), 10000, d[[ano_saf]])
    me <- feols(as.formula(paste0(y, " ~ sunab(Coorte, Ano, ref.p = -1) | Clube + Ano")), data = d, vcov = ~Clube, notes = FALSE)
    att <- coeftable(summary(me, agg = "att"))
    w <- wald(me, keep = "^Ano::-", print = FALSE)
    out$ATT <- att[1, 1]; out$EP_ATT <- att[1, 2]; out$p_ATT <- att[1, 4]
    out$F_pre <- w[["stat"]]; out$p_pre <- w[["p"]]
    ce <- as.data.frame(coeftable(summary(me, agg = TRUE)))
    es <- cbind(Especificacao = nome, Desfecho = y, Termo = rownames(ce), ce, row.names = NULL)
  }
  list(res = out, es = es)
}
RES <- list(); ES <- list()
add <- function(nome, d, y, ...) { r <- roda(nome, d, y, ...); RES[[length(RES) + 1]] <<- r$res; if (!is.null(r$es)) ES[[length(ES) + 1]] <<- r$es; r$res }

# ---- 1. Replica, extensao e modelo principal (Y_art e IH) ----
clubes_rep <- base %>% filter(Serie_Brasileirao == "A", Ano %in% 2022:2024) %>% pull(Clube) %>% unique()
cat("Clubes da replica:", length(clubes_rep), "\n")
rep_d <- base %>% filter(Clube %in% clubes_rep, Ano %in% 2022:2024)
for (y in c("Y_art", "IH")) {
  add("Replica (26 clubes, 2022-2024)", rep_d, y, evento = FALSE)
  add("Extensao temporal (26 clubes, 2021-2025)", base %>% filter(Clube %in% clubes_rep), y, evento = FALSE)
  add("Extensao de clubes (75 clubes)", base, y, evento = FALSE)
}
for (y in c("Y", "IH", "Rk125", "Rk150", "Rk200")) add("Principal", base, y)

# ---- 2. Robustez do IH e do ranking ----
c21 <- base %>% filter(Ano == 2021, Serie_Brasileirao %in% c("A", "B", "C")) %>% pull(Clube)
for (y in c("IH", "Rk125")) {
  add("Sem Red Bull Bragantino", base %>% filter(Clube != "Red Bull Bragantino"), y)
  add("Regra anterior de tratamento", base, y, saf = "SAF_antigo", ano_saf = "Ano_SAF_antigo")
  add("Amostra fixada em 2021", base %>% filter(Clube %in% c21), y)
}
add("Todos os clubes nacionais (A-D)", amp, "IH")
add("Populacao de 2021 (A-D)", amp %>% filter(Populacao_2021 == 1), "IH")
add("Verificacao: 218 clubes, Y", amp, "Y", evento = FALSE)

# ---- 3. Athletic (data de tratamento vem de noticia) ----
ath23 <- base %>% mutate(SAF = ifelse(Clube == "Athletic", as.integer(Ano >= 2023), SAF),
                         Ano_SAF = ifelse(Clube == "Athletic", 2023, Ano_SAF))
for (y in c("Y", "IH")) {
  add("Sem o Athletic", base %>% filter(Clube != "Athletic"), y)
  add("Athletic tratado em 2023", ath23, y)
  add("Sem o Athletic e sem o Red Bull Bragantino", base %>% filter(!Clube %in% c("Athletic", "Red Bull Bragantino")), y)
}

# ---- 4. Ordenacao alternativa da Serie D ----
sd <- read_excel(arquivo, sheet = "Serie_D_Fase_Grupos") %>%
  mutate(Ano = as.integer(Ano), Pontos_D = as.numeric(Pontos)) %>%
  group_by(Ano) %>% mutate(PosD_alt = rank(-Pontos_D, ties.method = "min")) %>% ungroup() %>%
  select(Ano, Clube, Pontos_D, PosD_alt)
cat("Participantes da Serie D por ano:\n"); print(table(sd$Ano))
bd <- base %>% left_join(sd, by = c("Clube", "Ano"))
nd <- sum(bd$Serie_Brasileirao == "D"); nm <- sum(bd$Serie_Brasileirao == "D" & is.na(bd$PosD_alt))
cat("Obs. da Serie D na amostra:", nd, "| sem correspondencia na aba Serie_D_Fase_Grupos:", nm, "\n")
bd <- bd %>% mutate(Rk125_Dalt = ifelse(Serie_Brasileirao == "D" & !is.na(PosD_alt), 60 + PosD_alt, Rk125))
cat("Correlacao entre a posicao final geral da base e a ordem por pontos da fase de grupos (Serie D):",
    round(cor(bd$Posicao[bd$Serie_Brasileirao == "D"], bd$PosD_alt[bd$Serie_Brasileirao == "D"], use = "complete.obs", method = "spearman"), 3), "\n")
add("Ranking com Serie D ordenada por pontos da fase de grupos", bd, "Rk125_Dalt")

# ---- 5. Mundlak com erros agrupados ----
mundlak <- function(d, y) {
  d <- d %>% group_by(Clube) %>% mutate(mS = mean(SAF)) %>% ungroup()
  m <- feols(as.formula(paste0(y, " ~ SAF + mS | Ano")), data = d, vcov = ~Clube, notes = FALSE)
  ct <- coeftable(m); data.frame(Desfecho = y, coef_mS = ct["mS", 1], EP = ct["mS", 2], p = ct["mS", 4])
}
mund <- bind_rows(cbind(Amostra = "Replica", mundlak(rep_d, "Y_art")), cbind(Amostra = "Principal", mundlak(base, "Y")),
                  cbind(Amostra = "Principal", mundlak(base, "IH")))

# ---- 6. Wild cluster bootstrap-t e permutacao complementar ----
# A permutacao redistribui trajetorias de tratamento entre clubes. Como a adocao
# da SAF nao e aleatoria, sua leitura como teste de randomizacao pressuporia
# permutabilidade sob a hipotese nula, condicao nao garantida neste contexto.
# Interpretar p_perm apenas como verificacao complementar de sensibilidade.
dm <- function(A) A - rowMeans(A) %o% rep(1, ncol(A)) - rep(1, nrow(A)) %o% colMeans(A) + mean(A)
inf_boot <- function(d, y, B = 9999) {
  d <- d %>% arrange(Clube, Ano); G <- n_distinct(d$Clube); T <- n_distinct(d$Ano); N <- G * T; K <- 1 + T
  Y <- matrix(d[[y]], G, T, byrow = TRUE); X <- matrix(as.numeric(d$SAF), G, T, byrow = TRUE)
  Xd <- dm(X); sxx <- sum(Xd^2)
  tstat <- function(Yd) { b <- sum(Xd * Yd) / sxx; e <- Yd - b * Xd; s <- rowSums(Xd * e)
    V <- sum(s^2) / sxx^2 * G / (G - 1) * (N - 1) / (N - K); c(b, b / sqrt(V)) }
  r0 <- tstat(dm(Y)); u <- dm(Y)
  webb <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
  ts <- numeric(B)
  for (i in 1:B) { v <- sample(webb, G, replace = TRUE); ts[i] <- tstat(dm(v * u))[2] }
  Yd <- dm(Y); bs <- numeric(B)
  for (i in 1:B) { Xp <- dm(X[sample(G), , drop = FALSE]); bs[i] <- sum(Xp * Yd) / sum(Xp^2) }
  data.frame(Desfecho = y, b = r0[1], t = r0[2], p_wcb = mean(abs(ts) >= abs(r0[2])), p_perm = mean(abs(bs) >= abs(r0[1]) - 1e-12))
}
cat("\nRodando bootstrap e permutacao (pode levar alguns minutos)...\n")
boot <- bind_rows(cbind(Amostra = "Replica", inf_boot(rep_d, "Y_art")), cbind(Amostra = "Replica", inf_boot(rep_d, "IH")),
                  cbind(Amostra = "Principal", inf_boot(base, "Y")), cbind(Amostra = "Principal", inf_boot(base, "IH")),
                  cbind(Amostra = "Principal", inf_boot(base, "Rk125")))

# ---- 7. Heterogeneidade por tipo de controle ----
A_ <- c("Botafogo-RJ", "Cruzeiro", "Vasco", "Bahia", "Coritiba", "Atlético-MG"); B_ <- c("Atlético-GO", "Fortaleza", "Cuiabá")
base <- base %>% mutate(SAF_A = SAF * Clube %in% A_, SAF_B = SAF * Clube %in% B_, SAF_C = SAF * !(Clube %in% c(A_, B_)))
het <- bind_rows(lapply(c("Y", "IH", "Rk125"), function(y) {
  m <- feols(as.formula(paste0(y, " ~ SAF_A + SAF_B + SAF_C | Clube + Ano")), data = base, vcov = ~Clube, notes = FALSE)
  ct <- coeftable(m); V <- vcov(m); b <- coef(m); G <- n_distinct(base$Clube)
  dif <- function(c) { e <- sum(c * b); s <- sqrt(as.numeric(t(c) %*% V %*% c)); c(e, s, 2 * pt(-abs(e / s), G - 1)) }
  rbind(data.frame(Desfecho = y, Termo = c("SAF_A investidor", "SAF_B associacao", "SAF_C nao classificada"), Coef = ct[, 1], EP = ct[, 2], p = ct[, 4]),
        data.frame(Desfecho = y, Termo = c("A - B", "A - C"), Coef = c(dif(c(1, -1, 0))[1], dif(c(1, 0, -1))[1]),
                   EP = c(dif(c(1, -1, 0))[2], dif(c(1, 0, -1))[2]), p = c(dif(c(1, -1, 0))[3], dif(c(1, 0, -1))[3])))
}))
cat("\nObs. tratadas por grupo: A =", sum(base$SAF_A), "B =", sum(base$SAF_B), "C =", sum(base$SAF_C),
    "| clubes: A =", n_distinct(base$Clube[base$SAF_A == 1]), "B =", n_distinct(base$Clube[base$SAF_B == 1]),
    "C =", n_distinct(base$Clube[base$SAF_C == 1]), "\n")

# ---- 8. Descritivas e efeito minimo detectavel ----
lin <- function(lab, d) data.frame(Grupo = lab, N = nrow(d), Pontos = mean(d$Pontos, na.rm = TRUE),
  Aproveit = 100 * mean(d$Pontos / (3 * d$Jogos), na.rm = TRUE), Y = mean(d$Y), IH = mean(d$IH))
desc <- bind_rows(lin("Amostra total", base), lin("Sem SAF", filter(base, SAF == 0)), lin("Com SAF", filter(base, SAF == 1)),
  bind_rows(lapply(c("A", "B", "C", "D"), function(s) bind_rows(lin(paste0("Serie ", s, " - sem SAF"), filter(base, Serie_Brasileirao == s, SAF == 0)),
                                                                lin(paste0("Serie ", s, " - com SAF"), filter(base, Serie_Brasileirao == s, SAF == 1))))),
  lin("Sem divisao nacional", filter(base, Nivel == 0)))
res <- bind_rows(RES)
pr <- res %>% filter(Especificacao == "Principal")
mde <- data.frame(Desfecho = c("Y", "IH"), media = c(mean(base$Y), mean(base$IH)), dp = c(sd(base$Y), sd(base$IH)),
  MDE_TWFE = 2.8 * pr$EP[match(c("Y", "IH"), pr$Desfecho)], MDE_ATT = 2.8 * pr$EP_ATT[match(c("Y", "IH"), pr$Desfecho)])

cat("\n========== RESUMO ==========\n")
print(res[, c("Especificacao", "Desfecho", "N", "Clubes", "Clubes_que_mudam", "SAF", "EP", "p", "ATT", "EP_ATT", "p_ATT", "F_pre", "p_pre")], digits = 4, row.names = FALSE)
cat("\n========== EVENTO (Principal) ==========\n")
print(bind_rows(ES) %>% filter(Especificacao == "Principal") %>% select(Desfecho, Termo, Estimate, `Std. Error`, `Pr(>|t|)`), digits = 4, row.names = FALSE)
cat("\n========== MUNDLAK ==========\n"); print(mund, digits = 4)
cat("\n========== BOOTSTRAP / PERMUTACAO ==========\n"); print(boot, digits = 4)
cat("\n========== HETEROGENEIDADE ==========\n"); print(het, digits = 4)
cat("\n========== DESCRITIVAS ==========\n"); print(desc, digits = 4)
cat("\n========== MDE ==========\n"); print(mde, digits = 4)
write.xlsx(list(Resumo = res, Evento = bind_rows(ES), Mundlak = mund, Bootstrap_Permutacao = boot, Heterogeneidade = het,
                Descritivas = desc, MDE = mde, Serie_D_alt = sd), "17_Complementar_Rev9.xlsx", overwrite = TRUE)
cat("\nArquivo salvo: 17_Complementar_Rev9.xlsx\n")
