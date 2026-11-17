# ======================================================================
# TCC - SENSIBILIDADE A VIOLACOES DE TENDENCIAS PARALELAS (REV7) - GRADE FINA
# Recalcula Mbar = 0 e 0,5 com grade de 0,25 ponto para verificar se os
# intervalos que ficam proximos de zero na grade padrao (passo de ~4 e ~2
# pontos) de fato excluem o zero.
# Rambachan e Roth (2023), pacote HonestDiD, aplicado ao estudo de evento
# de Sun e Abraham (2021) do modelo principal e do ranking nacional.
#
# Metodos:
#  - Magnitudes relativas (Mbar): a violacao pos-tratamento pode ser ate
#    Mbar vezes a maior variacao observada entre periodos pre-tratamento.
#  - Suavidade (M): a tendencia diferencial pode mudar de inclinacao em
#    no maximo M pontos por periodo.
# Janela pre-adocao: e = -4 a -2 (periodos mais distantes tem poucos
# clubes e tornam o metodo de magnitudes relativas inviavel).
# Parametros-alvo: efeito medio dos periodos pos-adocao (e = 0 a 3),
# efeito no segundo ano (e = 2) e efeito no ano da adocao (e = 0).
# ======================================================================

options(scipen = 999)
if (!requireNamespace("HonestDiD", quietly = TRUE)) {
  install.packages("HonestDiD", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("HonestDiD", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", repos = "https://cloud.r-project.org")
  remotes::install_github("asheshrambachan/HonestDiD", upgrade = "never")
}
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(fixest); library(openxlsx); library(HonestDiD)
})
cat("HonestDiD versao:", as.character(packageVersion("HonestDiD")), "\n")

pasta_tcc <- if (dir.exists("dados")) normalizePath(".") else if (dir.exists("../dados")) normalizePath("..") else stop("Execute a partir da raiz do repositorio (pasta que contem 'dados').")
arquivo_padrao <- file.path(pasta_tcc, "dados", "Dados_TCC_Painel_Modelagem_Rev5.xlsx")
pasta_resultados <- file.path(pasta_tcc, "resultados")
arquivo <- if (file.exists(arquivo_padrao)) arquivo_padrao else file.choose()
setwd(if (dir.exists(pasta_resultados)) pasta_resultados else dirname(normalizePath(arquivo)))

base <- read_excel(arquivo, sheet = "Painel") %>%
  mutate(
    Ano = as.integer(Ano),
    Ano_SAF = as.numeric(Ano_SAF),
    Pontos = as.numeric(Pontos_Brasileirao),
    Posicao = as.numeric(Posicao_Brasileirao),
    Peso = as.numeric(Peso_Divisao),
    Y = ifelse(Peso == 0, 0, Pontos * Peso),
    Ranking = case_when(
      Serie_Brasileirao == "A" ~ Posicao,
      Serie_Brasileirao == "B" ~ 20 + Posicao,
      Serie_Brasileirao == "C" ~ 40 + Posicao,
      Serie_Brasileirao == "D" ~ 60 + Posicao,
      TRUE ~ 125
    ),
    Coorte = ifelse(is.na(Ano_SAF), 10000, Ano_SAF)
  )

Mbar_grid <- c(0, 0.5)


honest <- function(nome, y, M_grid, janela_pre, grade, pontos) {
  me <- feols(as.formula(paste0(y, " ~ sunab(Coorte, Ano, ref.p = -1) | Clube + Ano")),
              data = base, vcov = ~Clube, notes = FALSE)
  # Agregacao por periodo relativo (vinheta do HonestDiD para fixest::sunab)
  agg <- me$model_matrix_info$sunab$agg_period
  nms <- names(me$coefficients)
  sel <- grepl(agg, nms, perl = TRUE)
  nms <- nms[sel]
  wgt <- colSums(sign(model.matrix(me)[, nms, drop = FALSE]))
  per <- as.numeric(gsub(paste0(".*", agg, ".*"), "\\2", nms, perl = TRUE))
  mat <- model.matrix(~ 0 + factor(per))
  trans <- solve(t(mat) %*% (wgt * mat)) %*% t(wgt * mat)
  betahat <- as.numeric(trans %*% cbind(me$coefficients[nms]))
  sigma <- trans %*% me$cov.scaled[nms, nms] %*% t(trans)
  e <- sort(unique(per))
  conf <- coef(me)[paste0("Ano::", e)]
  if (max(abs(conf - betahat)) > 1e-6) stop("Agregacao difere dos coeficientes do fixest.")
  ep_conf <- se(me)[paste0("Ano::", e)]
  cat("Maior diferenca de EP vs fixest:", max(abs(ep_conf - sqrt(diag(sigma)))), "\n")
  keep <- e >= janela_pre
  betahat <- betahat[keep]; sigma <- sigma[keep, keep, drop = FALSE]; e <- e[keep]
  pre <- sum(e < 0); post <- sum(e >= 0)
  cat("\n==========", nome, "==========\n")
  cat("Periodos de evento:", paste(e, collapse = ", "), "| pre =", pre, "| pos =", post, "\n")
  print(round(cbind(e = e, coef = betahat, ep = sqrt(diag(sigma))), 3))

  alvos <- list(
    "e=0" = as.numeric(seq_len(post) == which(e[e >= 0] == 0)),
    "e=2" = as.numeric(seq_len(post) == which(e[e >= 0] == 2))
  )
  res <- list()
  for (a in names(alvos)) {
    l <- alvos[[a]]
    for (alpha in c(0.05)) {
      orig <- constructOriginalCS(betahat = betahat, sigma = sigma, numPrePeriods = pre,
                                  numPostPeriods = post, l_vec = l, alpha = alpha)
      rm <- createSensitivityResults_relativeMagnitudes(
        betahat = betahat, sigma = sigma, numPrePeriods = pre, numPostPeriods = post,
        Mbarvec = Mbar_grid, l_vec = l, alpha = alpha,
        gridPoints = pontos, grid.lb = -grade, grid.ub = grade)
      est <- sum(l * betahat[e >= 0])
      res[[length(res) + 1]] <- bind_rows(
        data.frame(Desfecho = nome, Janela_pre = janela_pre, Alvo = a, Nivel = 1 - alpha, Estimativa = est, Metodo = "Original (tendencias paralelas)",
                   Parametro = NA_real_, lb = orig$lb, ub = orig$ub),
        data.frame(Desfecho = nome, Janela_pre = janela_pre, Alvo = a, Nivel = 1 - alpha, Estimativa = est, Metodo = "Magnitudes relativas (Mbar)",
                   Parametro = rm$Mbar, lb = rm$lb, ub = rm$ub)
      )
    }
  }
  out <- bind_rows(res)
  out$Inclui_zero <- out$lb <= 0 & out$ub >= 0
  print(out %>% filter(Nivel == 0.95) %>% mutate(across(c(Estimativa, lb, ub), ~ round(.x, 2))), row.names = FALSE)
  list(tabela = out, coef = data.frame(Desfecho = nome, e = e, coef = betahat, ep = sqrt(diag(sigma))))
}

r1 <- honest("Desempenho ponderado", "Y", NULL, janela_pre = -3, grade = 100, pontos = 801)
r2 <- honest("Ranking nacional", "Ranking", NULL, janela_pre = -3, grade = 60, pontos = 481)
write.xlsx(list(Grade_fina = bind_rows(r1$tabela, r2$tabela)), "15b_HonestDiD_grade_fina.xlsx", overwrite = TRUE)
cat("\nArquivo salvo: 15b_HonestDiD_grade_fina.xlsx\n")
