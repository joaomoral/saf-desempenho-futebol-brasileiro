# ======================================================================
# TCC - IMPACTO DA SAF SOBRE O DESEMPENHO ESPORTIVO DOS CLUBES
# SCRIPT UNIFICADO: MODELOS PRINCIPAIS + DIAGNOSTICOS + ROBUSTEZ
#
# REV4 - alteracoes em relacao a Rev3:
#  a) resultados salvos em pasta fixa (nao mais em getwd());
#  b) rotulo "Sem participacao nacional" corrigido (estava com acentos
#     corrompidos e virava NA na variavel Divisao);
#  c) clubes nunca tratados codificados com coorte 10000, como na
#     documentacao do fixest; coortes inconsistentes interrompem o script;
#  d) conversoes numericas avisam quando um valor vira NA;
#  e) aba financeira validada contra a aba sem financeiro;
#  f) estudo de evento repetido para pontos e percentil na divisao, com
#     teste de pre-tendencias e auditoria das observacoes usadas;
#  g) versoes do R e dos pacotes gravadas em 00_Sessao_R.txt.
#
# REV5 - alteracoes em relacao a Rev4:
#  h) usa a base Dados_TCC_Painel_Modelagem_Rev5.xlsx, com a regra de
#     junho para a SAF (constituida ate 30/06 = tratada no proprio ano)
#     e com os pontos da fase de grupos da Serie D;
#  i) anos sem participacao nacional (peso 0) recebem desempenho
#     ponderado igual a zero, em vez de ficarem sem valor.
#
# Base esperada: Dados_TCC_Painel_Modelagem_Rev5.xlsx
# Aba: Painel (a versao sem financeiro e derivada da mesma aba)
# Painel esperado: 75 clubes, 375 observacoes, 2021-2025.
#
# IMPORTANTE:
# 1) Execute este arquivo inteiro em uma sessao nova do R/RStudio.
# 2) Se a planilha nao estiver na pasta de trabalho, selecione-a quando
#    solicitado.
# 3) Os arquivos de resultado serao salvos na pasta da planilha.
# 4) SAF NAO deve ser removida: e a variavel central de tratamento.
# ======================================================================


# 0. CONFIGURACAO -------------------------------------------------------

options(scipen = 999)
cat("\n========== INICIO DO SCRIPT DO TCC ==========\n")


# 1. PACOTES ------------------------------------------------------------

pacotes <- c(
  "readxl", "dplyr", "fixest", "modelsummary", "openxlsx",
  "plm", "lmtest", "sandwich"
)

faltantes <- pacotes[!pacotes %in% rownames(installed.packages())]

if (length(faltantes) > 0) {
  install.packages(faltantes, dependencies = TRUE)
}

invisible(lapply(pacotes, function(p) {
  suppressPackageStartupMessages(
    library(p, character.only = TRUE)
  )
}))


# 2. IMPORTACAO DA PLANILHA ---------------------------------------------

# A base fica em dados e os resultados em
# resultados, qualquer que seja a pasta de trabalho do R.
pasta_tcc <- if (dir.exists("dados")) normalizePath(".") else if (dir.exists("../dados")) normalizePath("..") else stop("Execute a partir da raiz do repositorio (pasta que contem 'dados').")
arquivo_padrao <- file.path(pasta_tcc, "dados", "Dados_TCC_Painel_Modelagem_Rev5.xlsx")
pasta_resultados <- file.path(pasta_tcc, "resultados")

if (file.exists(arquivo_padrao)) {
  arquivo <- normalizePath(arquivo_padrao, winslash = "/")
} else {
  cat("\nSelecione Dados_TCC_Painel_Modelagem_Rev5.xlsx\n")
  arquivo <- file.choose()
}

if (!file.exists(arquivo)) {
  stop("O arquivo selecionado nao foi encontrado.")
}

abas_arquivo <- readxl::excel_sheets(arquivo)

abas_necessarias <- c("Painel")
abas_ausentes <- setdiff(abas_necessarias, abas_arquivo)

if (length(abas_ausentes) > 0) {
  stop(
    "A planilha selecionada nao possui a aba Painel.",
    " Selecione a versao Dados_TCC_Painel_Modelagem_Rev5.xlsx."
  )
}

painel_fin_bruto <- readxl::read_excel(arquivo, sheet = "Painel")

# Versao sem variaveis financeiras: mesma aba, sem as colunas financeiras.
painel_sem_bruto <- painel_fin_bruto[, setdiff(
  names(painel_fin_bruto), c("Receita_Total", "Divida_Total", "Folha_Salarial",
                              "Valor_Elenco", "Gasto_Contratacoes")
)]

# Os resultados sao salvos em resultados (ou, se essa pasta
# nao existir, na pasta da planilha).
setwd(if (dir.exists(pasta_resultados)) pasta_resultados else dirname(arquivo))
cat("Planilha:", arquivo, "\n")
cat("Pasta dos resultados:", getwd(), "\n")

writeLines(capture.output(utils::sessionInfo()), "00_Sessao_R.txt")


# 3. VALIDACAO DAS COLUNAS E PREPARACAO -------------------------------

colunas_obrigatorias <- c(
  "Clube", "UF", "Ano", "SAF", "Ano_SAF",
  "Titulos", "Vices", "Terceiros", "Podios",
  "Serie_Brasileirao", "Posicao_Brasileirao",
  "Pontos_Brasileirao", "Acesso", "Rebaixamento",
  "Peso_Divisao"
)

validar_colunas <- function(dados, nome_base) {
  ausentes <- setdiff(colunas_obrigatorias, names(dados))
  if (length(ausentes) > 0) {
    stop(
      "Colunas obrigatorias ausentes em ", nome_base, ": ",
      paste(ausentes, collapse = ", ")
    )
  }
  if (!"SAF" %in% names(dados)) {
    stop("A coluna SAF nao foi encontrada em ", nome_base, ".")
  }
  invisible(TRUE)
}

validar_colunas(painel_sem_bruto, "Modelo_sem_Financeiro")
validar_colunas(painel_fin_bruto, "Modelo_com_Financeiro")

# SAF precisa ser 0/1 antes de qualquer conversao para inteiro.
for (nome_base in c("Modelo_sem_Financeiro", "Modelo_com_Financeiro")) {
  bruto <- if (nome_base == "Modelo_sem_Financeiro") painel_sem_bruto else painel_fin_bruto
  saf_bruto <- suppressWarnings(as.numeric(bruto$SAF))
  if (anyNA(saf_bruto) || any(!saf_bruto %in% c(0, 1))) {
    stop("A coluna SAF deve conter apenas 0 ou 1 em ", nome_base, ".")
  }
}
rm(bruto, saf_bruto, nome_base)

# Converte colunas numericas e avisa quando algum valor preenchido
# nao pode ser lido como numero (por exemplo, texto ou virgula decimal).
converter_numerica <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  perdidos <- sum(is.na(y) & !is.na(x))
  if (perdidos > 0) {
    warning(
      perdidos, " valor(es) preenchido(s) nao numerico(s) viraram NA.",
      call. = FALSE
    )
  }
  y
}

prepara_painel <- function(dados) {

  numericas_base <- c(
    "Ano", "SAF", "Ano_SAF", "Titulos", "Vices",
    "Terceiros", "Podios", "Posicao_Brasileirao",
    "Pontos_Brasileirao", "Acesso", "Rebaixamento",
    "Peso_Divisao"
  )

  numericas_existentes <- intersect(numericas_base, names(dados))

  dados <- dados %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numericas_existentes),
        converter_numerica
      ),
      Clube = as.factor(Clube),
      Ano = as.integer(Ano),
      SAF = as.integer(SAF)
    )

  # Variavel de desempenho ponderado: pontos x peso da divisao.
  # Sem participacao nacional o peso e 0, logo o desempenho e 0 mesmo
  # sem pontos. Nos demais casos, NA permanece NA.
  dados <- dados %>%
    dplyr::mutate(
      Desempenho_Ponderado = ifelse(
        !is.na(Peso_Divisao) & Peso_Divisao == 0,
        0,
        Pontos_Brasileirao * Peso_Divisao
      )
    )

  # Logaritmos das variaveis financeiras, quando as colunas existem.
  if ("Receita_Total" %in% names(dados)) {
    dados$Receita_Total <- converter_numerica(dados$Receita_Total)
    dados$log_Receita <- ifelse(
      !is.na(dados$Receita_Total) & dados$Receita_Total >= 0,
      log1p(dados$Receita_Total), NA_real_
    )
  }

  if ("Divida_Total" %in% names(dados)) {
    dados$Divida_Total <- converter_numerica(dados$Divida_Total)
    dados$log_Divida <- ifelse(
      !is.na(dados$Divida_Total) & dados$Divida_Total >= 0,
      log1p(dados$Divida_Total), NA_real_
    )
  }

  if ("Valor_Elenco" %in% names(dados)) {
    dados$Valor_Elenco <- converter_numerica(dados$Valor_Elenco)
    dados$log_Valor_Elenco <- ifelse(
      !is.na(dados$Valor_Elenco) & dados$Valor_Elenco >= 0,
      log1p(dados$Valor_Elenco), NA_real_
    )
  }

  if ("Folha_Salarial" %in% names(dados)) {
    dados$Folha_Salarial <- converter_numerica(dados$Folha_Salarial)
    dados$log_Folha <- ifelse(
      !is.na(dados$Folha_Salarial) & dados$Folha_Salarial >= 0,
      log1p(dados$Folha_Salarial), NA_real_
    )
  }

  if ("Gasto_Contratacoes" %in% names(dados)) {
    dados$Gasto_Contratacoes <- converter_numerica(dados$Gasto_Contratacoes)
    dados$log_Gasto_Contratacoes <- ifelse(
      !is.na(dados$Gasto_Contratacoes) & dados$Gasto_Contratacoes >= 0,
      log1p(dados$Gasto_Contratacoes), NA_real_
    )
  }

  dados
}

painel_sem <- prepara_painel(painel_sem_bruto)
painel_fin <- prepara_painel(painel_fin_bruto)

rm(painel_sem_bruto, painel_fin_bruto)


# 4. CHECAGENS DA BASE --------------------------------------------------

cat("\n========== CHECAGEM DA BASE ==========\n")
cat("Base sem financeiro:", nrow(painel_sem), "observacoes;",
    dplyr::n_distinct(painel_sem$Clube), "clubes.\n")
cat("Base com financeiro:", nrow(painel_fin), "observacoes;",
    dplyr::n_distinct(painel_fin$Clube), "clubes.\n")
cat("Anos:", paste(sort(unique(painel_sem$Ano)), collapse = ", "), "\n")
cat("SAF presente:", "SAF" %in% names(painel_sem), "\n")
cat("Valores de SAF:", paste(sort(unique(stats::na.omit(painel_sem$SAF))),
                              collapse = ", "), "\n")
cat("Chaves Clube-Ano duplicadas:",
    sum(duplicated(painel_sem[c("Clube", "Ano")])), "\n")

if (anyNA(painel_sem$SAF)) {
  stop("A variavel SAF possui valores ausentes. Verifique a planilha.")
}
if (any(!painel_sem$SAF %in% c(0, 1))) {
  stop("A variavel SAF deve estar codificada como 0 ou 1.")
}
if (anyDuplicated(painel_sem[c("Clube", "Ano")]) > 0) {
  stop("Existem chaves Clube-Ano duplicadas na base sem financeiro.")
}

# A aba financeira deve ter as mesmas chaves e o mesmo tratamento.
if (anyDuplicated(painel_fin[c("Clube", "Ano")]) > 0) {
  stop("Existem chaves Clube-Ano duplicadas na base com financeiro.")
}
conferencia_abas <- merge(
  painel_sem[c("Clube", "Ano", "SAF")],
  painel_fin[c("Clube", "Ano", "SAF")],
  by = c("Clube", "Ano"), suffixes = c("_sem", "_fin")
)
if (nrow(conferencia_abas) != nrow(painel_sem) ||
    nrow(painel_fin) != nrow(painel_sem)) {
  stop("As abas com e sem financeiro nao possuem as mesmas chaves Clube-Ano.")
}
if (any(conferencia_abas$SAF_sem != conferencia_abas$SAF_fin)) {
  stop("A variavel SAF difere entre as abas com e sem financeiro.")
}
rm(conferencia_abas)

# A base informada para esta etapa deve conter 75 clubes x 5 anos.
if (nrow(painel_sem) != 375 ||
    dplyr::n_distinct(painel_sem$Clube) != 75 ||
    !all(sort(unique(painel_sem$Ano)) == 2021:2025)) {
  warning(
    "A dimensao da base difere de 375 observacoes, 75 clubes e 2021-2025. ",
    "O script continuara, mas confira se selecionou a planilha correta."
  )
}


# 5. ESTATISTICAS DESCRITIVAS GERAIS -----------------------------------

cat("\n========== ESTATISTICAS DESCRITIVAS ==========\n")

variaveis_desc <- c(
  "SAF", "Titulos", "Vices", "Terceiros", "Podios",
  "Posicao_Brasileirao", "Pontos_Brasileirao",
  "Desempenho_Ponderado", "Acesso", "Rebaixamento",
  "Peso_Divisao"
)

# Validacao explicita evita erro de all_of() por coluna inexistente.
variaveis_desc <- intersect(variaveis_desc, names(painel_sem))

descritivas <- painel_sem %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(variaveis_desc),
      list(
        N = ~sum(!is.na(.x)),
        Media = ~if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE),
        DP = ~if (sum(!is.na(.x)) < 2) NA_real_ else sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

print(descritivas)

# Cobertura das variaveis financeiras: inclui somente colunas existentes.
vars_fin_cobertura <- intersect(
  c("Receita_Total", "Divida_Total", "Folha_Salarial",
    "Valor_Elenco", "Gasto_Contratacoes", "Investimento_SAF"),
  names(painel_fin)
)

cobertura_financeira <- data.frame(
  Variavel = vars_fin_cobertura,
  N_validos = vapply(
    vars_fin_cobertura,
    function(v) sum(!is.na(painel_fin[[v]])),
    numeric(1)
  ),
  N_total = nrow(painel_fin)
)

if (nrow(cobertura_financeira) > 0) {
  cobertura_financeira$Percentual <- with(
    cobertura_financeira, 100 * N_validos / N_total
  )
  print(cobertura_financeira)
}


# 6. MODELOS PRINCIPAIS - SEM CONTROLES FINANCEIROS --------------------

cat("\n========== MODELOS PRINCIPAIS: SEM FINANCEIRO ==========\n")

outcomes_sem <- c(
  "Titulos", "Vices", "Terceiros", "Podios",
  "Acesso", "Rebaixamento", "Pontos_Brasileirao",
  "Desempenho_Ponderado", "Posicao_Brasileirao"
)

modelos_sem <- list()

for (y in outcomes_sem) {

  if (!y %in% names(painel_sem)) next

  d <- painel_sem %>%
    dplyr::filter(!is.na(.data[[y]]), !is.na(SAF))

  if (nrow(d) <= 10 || dplyr::n_distinct(d$Clube) <= 2) {
    message("Modelo ignorado por amostra insuficiente: ", y)
    next
  }

  formula_modelo <- stats::as.formula(
    paste0(y, " ~ SAF | Clube + Ano")
  )

  modelos_sem[[y]] <- tryCatch(
    fixest::feols(
      formula_modelo,
      data = d,
      vcov = ~Clube,
      notes = FALSE
    ),
    error = function(e) {
      message("Falha no modelo ", y, ": ", conditionMessage(e))
      NULL
    }
  )
}

modelos_sem <- modelos_sem[
  !vapply(modelos_sem, is.null, logical(1))
]

print(modelos_sem)


# 7. MODELOS COM CONTROLES FINANCEIROS -------------------------------

cat("\n========== MODELOS COM CONTROLES FINANCEIROS ==========\n")

# A divida disponivel nas fontes Sports Value e divida liquida,
# nao necessariamente divida bruta.
financeiras <- c("log_Receita", "log_Divida", "log_Folha", "log_Valor_Elenco",
                 "log_Gasto_Contratacoes")

outcomes_fin <- c(
  "Titulos", "Podios", "Desempenho_Ponderado",
  "Pontos_Brasileirao", "Posicao_Brasileirao"
)

modelos_com <- list()

for (y in outcomes_fin) {
  for (x in financeiras) {

    if (!all(c(y, x, "SAF") %in% names(painel_fin))) next

    d <- painel_fin %>%
      dplyr::filter(
        !is.na(.data[[y]]),
        !is.na(.data[[x]]),
        !is.na(SAF)
      )

    if (nrow(d) <= 20 || dplyr::n_distinct(d$Clube) <= 5) {
      message("Amostra insuficiente para ", y, " com ", x)
      next
    }

    nome <- paste(y, x, sep = "_")
    formula_modelo <- stats::as.formula(
      paste0(y, " ~ SAF + ", x, " | Clube + Ano")
    )

    modelos_com[[nome]] <- tryCatch(
      fixest::feols(
        formula_modelo,
        data = d,
        vcov = ~Clube,
        notes = FALSE
      ),
      error = function(e) {
        message("Falha no modelo ", nome, ": ", conditionMessage(e))
        NULL
      }
    )
  }
}

modelos_com <- modelos_com[
  !vapply(modelos_com, is.null, logical(1))
]

print(modelos_com)


# 8. TABELAS DOS MODELOS PRINCIPAIS -----------------------------------

if (length(modelos_sem) > 0) {
  modelsummary::modelsummary(
    modelos_sem,
    output = "01_Resultados_Modelos_Sem_Financeiro.html",
    stars = TRUE,
    title = "Desempenho esportivo - modelos sem controles financeiros"
  )
}

if (length(modelos_com) > 0) {
  modelsummary::modelsummary(
    modelos_com,
    output = "02_Resultados_Modelos_Com_Financeiro.html",
    stars = TRUE,
    title = "Desempenho esportivo - modelos com controles financeiros"
  )
}


# 9. DIAGNOSTICO DA VARIACAO TEMPORAL DA SAF ---------------------------

cat("\n========== VARIACAO TEMPORAL DA SAF ==========\n")

variacao_saf <- painel_sem %>%
  dplyr::group_by(Clube) %>%
  dplyr::summarise(
    SAF_min = if (all(is.na(SAF))) NA_real_ else min(SAF, na.rm = TRUE),
    SAF_max = if (all(is.na(SAF))) NA_real_ else max(SAF, na.rm = TRUE),
    varia_SAF = !is.na(SAF_min) & !is.na(SAF_max) & SAF_min != SAF_max,
    .groups = "drop"
  )

cat("Clubes com variacao temporal de SAF:",
    sum(variacao_saf$varia_SAF, na.rm = TRUE), "\n")
cat("Clubes sem variacao temporal de SAF:",
    sum(!variacao_saf$varia_SAF, na.rm = TRUE), "\n")

openxlsx::write.xlsx(
  variacao_saf,
  "03_Diagnostico_Variacao_SAF.xlsx",
  overwrite = TRUE
)


# 10. DESCRITIVAS POR ANO E POR SAF + CORRELACAO -----------------------

cat("\n========== DESCRITIVAS POR ANO E SAF ==========\n")

vars_desc_grupo <- intersect(
  c("SAF", "Titulos", "Vices", "Terceiros", "Podios",
    "Posicao_Brasileirao", "Pontos_Brasileirao",
    "Desempenho_Ponderado", "Acesso", "Rebaixamento",
    "Peso_Divisao"),
  names(painel_sem)
)

resumo_across <- list(
  N = ~sum(!is.na(.x)),
  Media = ~if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE),
  DP = ~if (sum(!is.na(.x)) < 2) NA_real_ else sd(.x, na.rm = TRUE)
)

desc_ano <- painel_sem %>%
  dplyr::group_by(Ano) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars_desc_grupo),
      resumo_across,
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

vars_desc_saf <- setdiff(vars_desc_grupo, "SAF")

desc_saf <- painel_sem %>%
  dplyr::group_by(SAF) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vars_desc_saf),
      resumo_across,
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

vars_cor <- intersect(
  c("SAF", "Peso_Divisao", "Titulos", "Vices", "Terceiros",
    "Podios", "Posicao_Brasileirao", "Pontos_Brasileirao",
    "Desempenho_Ponderado", "Acesso", "Rebaixamento"),
  names(painel_sem)
)

matriz_cor <- stats::cor(
  painel_sem[, vars_cor, drop = FALSE],
  use = "pairwise.complete.obs",
  method = "spearman"
)

openxlsx::write.xlsx(
  list(
    Descritivas_por_ano = desc_ano,
    Descritivas_por_SAF = desc_saf,
    Correlacao_Spearman = as.data.frame(matriz_cor),
    Variacao_SAF_por_clube = variacao_saf
  ),
  "04_Diagnosticos_Descritivos_TCC.xlsx",
  overwrite = TRUE
)


# 11. TESTES DE ESPECIFICACAO E DIAGNOSTICO DO PAINEL ------------------

cat("\n========== TESTES DE ESPECIFICACAO E DIAGNOSTICO ==========\n")

captura_teste <- function(expr) {
  tryCatch(
    {
      z <- eval.parent(substitute(expr))
      data.frame(
        estatistica = if (length(z$statistic) > 0)
          unname(z$statistic[1]) else NA_real_,
        p_valor = if (!is.null(z$p.value)) z$p.value else NA_real_,
        metodo = if (!is.null(z$method)) as.character(z$method) else NA_character_,
        erro = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      data.frame(
        estatistica = NA_real_,
        p_valor = NA_real_,
        metodo = NA_character_,
        erro = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    }
  )
}

executa_diagnosticos <- function(y, dados = painel_sem) {

  if (!all(c("Clube", "Ano", "SAF", y) %in% names(dados))) {
    return(NULL)
  }

  d <- dados %>%
    dplyr::select(Clube, Ano, SAF, dplyr::all_of(y)) %>%
    dplyr::filter(
      !is.na(Clube), !is.na(Ano),
      !is.na(SAF), !is.na(.data[[y]])
    ) %>%
    droplevels()

  if (nrow(d) < 30 ||
      dplyr::n_distinct(d$Clube) < 10 ||
      dplyr::n_distinct(d$Ano) < 3) {
    message("Amostra insuficiente para diagnosticos de ", y)
    return(NULL)
  }

  f <- stats::as.formula(paste0(y, " ~ SAF"))

  pd <- tryCatch(
    plm::pdata.frame(d, index = c("Clube", "Ano"), drop.index = FALSE),
    error = function(e) {
      message("Falha ao criar painel para ", y, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(pd)) return(NULL)

  pooled <- tryCatch(
    plm::plm(f, data = pd, model = "pooling"),
    error = function(e) NULL
  )
  within <- tryCatch(
    plm::plm(f, data = pd, model = "within", effect = "twoways"),
    error = function(e) NULL
  )
  random <- tryCatch(
    plm::plm(f, data = pd, model = "random", effect = "twoways"),
    error = function(e) NULL
  )

  if (is.null(pooled) || is.null(within)) {
    message("Nao foi possivel estimar pooling/FE para ", y)
    return(NULL)
  }

  teste_FE <- captura_teste(plm::pFtest(within, pooled))
  teste_LM <- captura_teste(
    plm::plmtest(pooled, effect = "individual", type = "bp")
  )

  teste_Hausman <- if (!is.null(random)) {
    captura_teste(plm::phtest(within, random))
  } else {
    data.frame(
      estatistica = NA_real_, p_valor = NA_real_,
      metodo = NA_character_,
      erro = "Modelo de efeitos aleatorios nao estimado."
    )
  }

  teste_serial <- captura_teste(plm::pbgtest(within, order = 1))
  teste_CD <- captura_teste(plm::pcdtest(within, test = "cd"))
  teste_hetero <- captura_teste(lmtest::bptest(within))

  testes <- dplyr::bind_rows(
    cbind(teste = "F: pooling vs FE", teste_FE),
    cbind(teste = "LM: pooling vs RE", teste_LM),
    cbind(teste = "Hausman: FE vs RE", teste_Hausman),
    cbind(teste = "Autocorrelacao serial", teste_serial),
    cbind(teste = "Dependencia transversal (CD)", teste_CD),
    cbind(teste = "Heterocedasticidade (BP)", teste_hetero)
  )

  robusto <- tryCatch(
    lmtest::coeftest(
      within,
      vcov. = plm::vcovHC(
        within, method = "arellano",
        type = "HC1", cluster = "group"
      )
    ),
    error = function(e) {
      message("Falha no erro-padrao robusto de ", y, ": ",
              conditionMessage(e))
      NULL
    }
  )

  list(
    testes = cbind(outcome = y, testes),
    modelo_FE = within,
    modelo_RE = random,
    modelo_pooling = pooled,
    coeficiente_FE_robusto = robusto,
    n = nrow(d),
    clubes = dplyr::n_distinct(d$Clube)
  )
}

outcomes_diag <- intersect(
  c("Desempenho_Ponderado", "Podios", "Titulos",
    "Acesso", "Rebaixamento"),
  names(painel_sem)
)

resultados_diag <- lapply(outcomes_diag, executa_diagnosticos)
names(resultados_diag) <- outcomes_diag
resultados_diag <- resultados_diag[
  !vapply(resultados_diag, is.null, logical(1))
]

if (length(resultados_diag) > 0) {

  tabela_testes <- dplyr::bind_rows(
    lapply(resultados_diag, function(x) x$testes)
  )

  print(tabela_testes)

  abas_diag <- list(Testes_Painel = tabela_testes)

  for (nm in names(resultados_diag)) {
    z <- resultados_diag[[nm]]$coeficiente_FE_robusto

    if (!is.null(z)) {
      abas_diag[[paste0("Coef_", nm)]] <- data.frame(
        termo = rownames(z),
        estimativa = z[, 1],
        erro_padrao = z[, 2],
        estatistica = z[, 3],
        p_valor = z[, 4],
        row.names = NULL
      )
    }
  }

  openxlsx::write.xlsx(
    abas_diag,
    "05_Testes_Econometricos_TCC.xlsx",
    overwrite = TRUE
  )

} else {
  warning("Nenhum diagnostico foi concluido. Verifique os avisos acima.")
}


# 12. ROBUSTEZ: POISSON COM EFEITOS FIXOS PARA CONTAGENS --------------

cat("\n========== ROBUSTEZ: POISSON FE ==========\n")

contagens <- intersect(
  c("Titulos", "Vices", "Terceiros", "Podios"),
  names(painel_sem)
)

modelos_poisson <- list()

for (y in contagens) {

  d <- painel_sem %>%
    dplyr::filter(
      !is.na(.data[[y]]),
      !is.na(SAF),
      .data[[y]] >= 0
    )

  if (nrow(d) <= 30 || dplyr::n_distinct(d$Clube) <= 5) next

  f <- stats::as.formula(paste0(y, " ~ SAF | Clube + Ano"))

  modelos_poisson[[y]] <- tryCatch(
    fixest::fepois(
      f, data = d, vcov = ~Clube, notes = FALSE
    ),
    error = function(e) {
      message("Poisson nao estimado para ", y, ": ",
              conditionMessage(e))
      NULL
    }
  )
}

modelos_poisson <- modelos_poisson[
  !vapply(modelos_poisson, is.null, logical(1))
]

if (length(modelos_poisson) > 0) {
  modelsummary::modelsummary(
    modelos_poisson,
    output = "06_Robustez_Poisson_Contagens.html",
    stars = TRUE,
    title = "Robustez: Poisson com efeitos fixos"
  )
}


# 13. ROBUSTEZ: DIVISAO COMO CATEGORIA -------------------------------

cat("\n========== ROBUSTEZ: DIVISAO CATEGORICA ==========\n")

# Primeiro, audita-se a disponibilidade real das variaveis por divisao.
# Isso evita interpretar uma categoria como referencia quando ela nao
# possui observacoes validas na amostra efetivamente estimada.

painel_sem <- painel_sem %>%
  dplyr::mutate(
    Divisao = factor(
      as.character(Serie_Brasileirao),
      levels = c("Sem participa\u00e7\u00e3o nacional", "D", "C", "B", "A")
    )
  )

auditoria_divisao <- painel_sem %>%
  dplyr::group_by(Divisao, .drop = FALSE) %>%
  dplyr::summarise(
    N_total = dplyr::n(),
    N_pontos = sum(!is.na(Pontos_Brasileirao)),
    N_desempenho_ponderado = sum(!is.na(Desempenho_Ponderado)),
    N_clubes = dplyr::n_distinct(Clube),
    .groups = "drop"
  )
print(auditoria_divisao)

# Modelo original, mantido para comparacao, mas acompanhado da auditoria.
d_div_ponderado <- painel_sem %>%
  dplyr::filter(
    !is.na(Desempenho_Ponderado), !is.na(SAF),
    !is.na(Divisao), !is.na(Clube), !is.na(Ano)
  ) %>%
  droplevels()

modelo_divisoes_ponderado <- NULL
if (nrow(d_div_ponderado) > 20 && dplyr::n_distinct(d_div_ponderado$Clube) > 5) {
  modelo_divisoes_ponderado <- tryCatch(
    fixest::feols(
      Desempenho_Ponderado ~ SAF + Divisao | Clube + Ano,
      data = d_div_ponderado,
      vcov = ~Clube,
      notes = FALSE
    ),
    error = function(e) {
      message("Modelo ponderado por divisao nao estimado: ", conditionMessage(e))
      NULL
    }
  )
}

# Especificacao complementar: pontos com controles categoricos de divisao.
# A interpretacao deve considerar que pontos de campeonatos distintos nao
# sao perfeitamente comparaveis; por isso este modelo e apenas complementar.
d_div_pontos <- painel_sem %>%
  dplyr::filter(
    !is.na(Pontos_Brasileirao), !is.na(SAF),
    !is.na(Divisao), !is.na(Clube), !is.na(Ano)
  ) %>%
  droplevels()

modelo_divisoes_pontos <- NULL
if (nrow(d_div_pontos) > 20 && dplyr::n_distinct(d_div_pontos$Clube) > 5 &&
    dplyr::n_distinct(d_div_pontos$Divisao) > 1) {
  modelo_divisoes_pontos <- tryCatch(
    fixest::feols(
      Pontos_Brasileirao ~ SAF + Divisao | Clube + Ano,
      data = d_div_pontos,
      vcov = ~Clube,
      notes = FALSE
    ),
    error = function(e) {
      message("Modelo de pontos com divisao categorica nao estimado: ", conditionMessage(e))
      NULL
    }
  )
}

modelos_divisao <- list()
if (!is.null(modelo_divisoes_ponderado)) {
  modelos_divisao[["Desempenho ponderado"]] <- modelo_divisoes_ponderado
}
if (!is.null(modelo_divisoes_pontos)) {
  modelos_divisao[["Pontos com controle de divisao"]] <- modelo_divisoes_pontos
}

if (length(modelos_divisao) > 0) {
  modelsummary::modelsummary(
    modelos_divisao,
    output = "07_Robustez_Dummies_Divisao.html",
    stars = TRUE,
    title = "Robustez: divisao categorica - amostras efetivamente utilizadas"
  )
}

# Medida adicional: desempenho relativo dentro da divisao e do ano.
# O percentil varia de 0 a 1 e compara o clube apenas com clubes da mesma
# divisao no mesmo ano, reduzindo a comparabilidade direta entre series.
painel_sem <- painel_sem %>%
  dplyr::group_by(Ano, Divisao) %>%
  dplyr::mutate(
    Desempenho_Relativo_Divisao = ifelse(
      sum(!is.na(Pontos_Brasileirao)) >= 2,
      dplyr::percent_rank(Pontos_Brasileirao),
      NA_real_
    )
  ) %>%
  dplyr::ungroup()

d_relativo <- painel_sem %>%
  dplyr::filter(
    !is.na(Desempenho_Relativo_Divisao), !is.na(SAF),
    !is.na(Clube), !is.na(Ano)
  )

modelo_relativo_divisao <- NULL
if (nrow(d_relativo) > 20 && dplyr::n_distinct(d_relativo$Clube) > 5) {
  modelo_relativo_divisao <- tryCatch(
    fixest::feols(
      Desempenho_Relativo_Divisao ~ SAF | Clube + Ano,
      data = d_relativo,
      vcov = ~Clube,
      notes = FALSE
    ),
    error = function(e) {
      message("Modelo de desempenho relativo nao estimado: ", conditionMessage(e))
      NULL
    }
  )
}

if (!is.null(modelo_relativo_divisao)) {
  modelsummary::modelsummary(
    list("Percentil dentro da divisao/ano" = modelo_relativo_divisao),
    output = "10_Robustez_Desempenho_Relativo.html",
    stars = TRUE,
    title = "Robustez: desempenho relativo dentro da divisao e do ano"
  )
}

# Exporta a auditoria da amostra e das categorias utilizadas.
info_modelos_divisao <- dplyr::bind_rows(
  if (!is.null(modelo_divisoes_ponderado)) data.frame(
    Modelo = "Desempenho ponderado + dummies",
    N = stats::nobs(modelo_divisoes_ponderado),
    Clubes = dplyr::n_distinct(d_div_ponderado$Clube),
    Divisoes_na_amostra = paste(sort(unique(as.character(d_div_ponderado$Divisao))), collapse = ", ")
  ) else NULL,
  if (!is.null(modelo_divisoes_pontos)) data.frame(
    Modelo = "Pontos + dummies de divisao",
    N = stats::nobs(modelo_divisoes_pontos),
    Clubes = dplyr::n_distinct(d_div_pontos$Clube),
    Divisoes_na_amostra = paste(sort(unique(as.character(d_div_pontos$Divisao))), collapse = ", ")
  ) else NULL,
  if (!is.null(modelo_relativo_divisao)) data.frame(
    Modelo = "Percentil dentro da divisao/ano",
    N = stats::nobs(modelo_relativo_divisao),
    Clubes = dplyr::n_distinct(d_relativo$Clube),
    Divisoes_na_amostra = paste(sort(unique(as.character(d_relativo$Divisao))), collapse = ", ")
  ) else NULL
)

openxlsx::write.xlsx(
  list(Auditoria_divisoes = auditoria_divisao,
       Amostras_modelos = info_modelos_divisao),
  "10_Auditoria_Robustez_Divisoes.xlsx",
  overwrite = TRUE
)


# 13B. DIAGNOSTICO DA ADOCAO ESCALONADA DA SAF -------------------------

cat("\n========== DIAGNOSTICO: ADOCAO ESCALONADA DA SAF ==========\n")

# Confere se Ano_SAF e constante por clube e compativel com a primeira
# observacao em que SAF=1. Qualquer inconsistencia interrompe o script,
# em vez de transformar o clube em controle.
auditoria_coorte <- painel_sem %>%
  dplyr::group_by(Clube) %>%
  dplyr::summarise(
    N_valores_Ano_SAF = dplyr::n_distinct(stats::na.omit(Ano_SAF)),
    Ano_SAF_informado = if (N_valores_Ano_SAF == 1) {
      unique(stats::na.omit(Ano_SAF))
    } else {
      NA_real_
    },
    Anos_com_SAF1 = sum(SAF == 1, na.rm = TRUE),
    Primeiro_ano_SAF1 = if (any(SAF == 1, na.rm = TRUE)) {
      min(Ano[SAF == 1], na.rm = TRUE)
    } else {
      NA_real_
    },
    Reversao_SAF = any(diff(SAF[order(Ano)]) < 0),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Situacao = dplyr::case_when(
      N_valores_Ano_SAF > 1 ~ "ERRO: mais de um Ano_SAF",
      Reversao_SAF ~ "ERRO: SAF volta de 1 para 0",
      Anos_com_SAF1 > 0 & is.na(Ano_SAF_informado) ~
        "ERRO: SAF=1 sem Ano_SAF",
      Anos_com_SAF1 > 0 & Ano_SAF_informado != Primeiro_ano_SAF1 ~
        "ERRO: Ano_SAF difere do primeiro ano com SAF=1",
      Anos_com_SAF1 == 0 & !is.na(Ano_SAF_informado) &
        Ano_SAF_informado <= max(painel_sem$Ano) ~
        "ERRO: Ano_SAF dentro do painel sem SAF=1",
      is.na(Ano_SAF_informado) ~ "Nunca tratado (controle)",
      Ano_SAF_informado > max(painel_sem$Ano) ~
        "Adocao futura (controle ate o fim do painel)",
      TRUE ~ "Tratado no painel"
    )
  )

print(table(auditoria_coorte$Situacao))

openxlsx::write.xlsx(
  list(Auditoria_coortes = auditoria_coorte),
  "11_Auditoria_Coortes_SAF.xlsx",
  overwrite = TRUE
)

if (any(grepl("^ERRO", auditoria_coorte$Situacao))) {
  print(auditoria_coorte[grepl("^ERRO", auditoria_coorte$Situacao), ])
  stop("Coortes de SAF inconsistentes. Veja 11_Auditoria_Coortes_SAF.xlsx.")
}

# Estudo de evento Sun-Abraham para tratamento escalonado.
# Nunca tratados recebem a coorte 10000, como no exemplo da documentacao
# do fixest. Adocoes posteriores ao ultimo ano do painel mantem o ano
# real: so tem periodos relativos negativos e, por isso, o sunab() tambem
# as usa como grupo de referencia.
painel_sem <- painel_sem %>%
  dplyr::left_join(
    auditoria_coorte %>%
      dplyr::transmute(
        Clube,
        Coorte_SAF = ifelse(is.na(Ano_SAF_informado), 10000, Ano_SAF_informado)
      ),
    by = "Clube"
  )

desfechos_evento <- c(
  "Desempenho_Ponderado", "Pontos_Brasileirao",
  "Desempenho_Relativo_Divisao"
)

modelos_evento <- list()
n_coortes_tratadas <- dplyr::n_distinct(
  painel_sem$Coorte_SAF[painel_sem$Coorte_SAF <= max(painel_sem$Ano)]
)

if (n_coortes_tratadas >= 2 && any(painel_sem$Coorte_SAF == 10000)) {
  for (y in desfechos_evento) {
    f_evento <- stats::as.formula(
      paste0(y, " ~ sunab(Coorte_SAF, Ano, ref.p = -1) | Clube + Ano")
    )
    modelos_evento[[y]] <- tryCatch(
      fixest::feols(
        f_evento, data = painel_sem, vcov = ~Clube, notes = FALSE
      ),
      error = function(e) {
        message("Estudo de evento nao estimado para ", y, ": ",
                conditionMessage(e))
        NULL
      }
    )
  }
} else {
  message("Sun-Abraham nao executado: sao necessarias ao menos duas coortes tratadas e clubes controle.")
}

modelos_evento <- modelos_evento[
  !vapply(modelos_evento, is.null, logical(1))
]
modelo_sunab <- modelos_evento[["Desempenho_Ponderado"]]

if (length(modelos_evento) > 0) {

  modelsummary::modelsummary(
    modelos_evento,
    output = "11_Estudo_de_Evento_Sun_Abraham.html",
    stars = TRUE,
    title = "Estudo de evento para adocao escalonada da SAF"
  )

  abas_evento <- list()
  testes_evento <- list()
  efeitos_medios <- list()

  for (y in names(modelos_evento)) {
    m <- modelos_evento[[y]]

    tab <- as.data.frame(fixest::coeftable(m))
    tab <- cbind(Termo = rownames(tab), tab)
    rownames(tab) <- NULL
    abas_evento[[substr(paste0("Coef_", y), 1, 31)]] <- tab

    w <- fixest::wald(m, keep = "^Ano::-", print = FALSE)
    testes_evento[[y]] <- data.frame(
      Desfecho = y,
      Teste = "Leads conjuntamente iguais a zero",
      Estatistica = unname(w[["stat"]]),
      gl1 = unname(w[["df1"]]),
      gl2 = unname(w[["df2"]]),
      p_valor = unname(w[["p"]]),
      N = stats::nobs(m),
      Clubes = dplyr::n_distinct(painel_sem$Clube[fixest::obs(m)])
    )

    att <- as.data.frame(fixest::coeftable(summary(m, agg = "att")))
    efeitos_medios[[y]] <- cbind(Desfecho = y, att, row.names = NULL)
  }

  tabela_pre_tendencias <- dplyr::bind_rows(testes_evento)
  tabela_efeito_medio <- dplyr::bind_rows(efeitos_medios)

  cat("\nTeste conjunto de pre-tendencias:\n")
  print(tabela_pre_tendencias)
  cat("\nEfeito medio pos-adocao (ATT agregado):\n")
  print(tabela_efeito_medio)

  # Quais linhas do painel entram no estudo de evento principal e por que
  # as demais ficam de fora.
  amostra_evento <- painel_sem %>%
    dplyr::mutate(
      Na_estimacao = dplyr::row_number() %in% fixest::obs(modelo_sunab),
      Motivo = dplyr::case_when(
        Na_estimacao ~ "Usada",
        is.na(Desempenho_Ponderado) ~
          "Desfecho ausente na base",
        TRUE ~ "Clube com um unico ano valido (absorvido pelo efeito fixo)"
      )
    ) %>%
    dplyr::select(
      Clube, Ano, Serie_Brasileirao, SAF, Coorte_SAF,
      Desempenho_Ponderado, Na_estimacao, Motivo
    )

  print(table(amostra_evento$Motivo))

  openxlsx::write.xlsx(
    c(
      abas_evento,
      list(
        Teste_pre_tendencias = tabela_pre_tendencias,
        Efeito_medio_ATT = tabela_efeito_medio,
        Amostra_evento = amostra_evento,
        Auditoria_coortes = auditoria_coorte
      )
    ),
    "12_Resultados_Sun_Abraham.xlsx",
    overwrite = TRUE
  )
}


# 14. COBERTURA FINANCEIRA EFETIVA ------------------------------------

cat("\n========== COBERTURA FINANCEIRA ==========\n")

vars_fin <- intersect(
  c("Receita_Total", "Divida_Total", "Folha_Salarial",
    "Valor_Elenco", "Gasto_Contratacoes", "Investimento_SAF"),
  names(painel_fin)
)

cobertura_fin <- data.frame(
  variavel = vars_fin,
  n_validos = vapply(
    vars_fin,
    function(v) sum(!is.na(painel_fin[[v]])),
    numeric(1)
  ),
  n_total = nrow(painel_fin)
)

if (nrow(cobertura_fin) > 0) {
  cobertura_fin$percentual <- 100 *
    cobertura_fin$n_validos / cobertura_fin$n_total
  print(cobertura_fin)

  openxlsx::write.xlsx(
    cobertura_fin,
    "08_Cobertura_Efetiva_Financeira_TCC.xlsx",
    overwrite = TRUE
  )
}


# 15. EXPORTACAO DAS BASES E DESCRITIVAS ------------------------------

cat("\n========== EXPORTACAO FINAL ==========\n")

abas_base <- list(
  Painel_sem_financeiro = painel_sem,
  Painel_com_financeiro = painel_fin,
  Descritivas_gerais = descritivas,
  Variacao_SAF = variacao_saf
)

if (exists("cobertura_financeira") &&
    nrow(cobertura_financeira) > 0) {
  abas_base$Cobertura_financeira <- cobertura_financeira
}

openxlsx::write.xlsx(
  abas_base,
  "09_Base_e_Resultados_Auxiliares_TCC.xlsx",
  overwrite = TRUE
)


# 16. AVISOS METODOLOGICOS --------------------------------------------

cat("\n========== AVISOS METODOLOGICOS ==========\n")
cat("* Interpretar F, LM, Hausman e diagnosticos em conjunto.\n")
cat("* Os modelos principais usam efeitos fixos de clube e ano,\n")
cat("  com erros-padrao agrupados por clube.\n")
cat("* A adocao da SAF e escalonada; TWFE convencional pode nao\n")
cat("  identificar um efeito causal medio adequado se os efeitos\n")
cat("  variarem entre coortes ou ao longo do tempo.\n")
cat("* O estudo de evento Sun-Abraham e complementar; o teste conjunto\n")
cat("  de pre-tendencias esta em 12_Resultados_Sun_Abraham.xlsx.\n")
cat("* O percentil de pontos compara clubes apenas dentro da mesma\n")
cat("  divisao e ano; nao representa pontos absolutos.\n")
cat("* Com cinco anos, testes de raiz unitaria em painel nao sao\n")
cat("  prioridade nesta etapa.\n")
cat("* Titulos, vices, terceiros e podios devem respeitar a auditoria\n")
cat("  historica e as regras metodologicas da base.\n")
cat("* Divida_Total deve ser interpretada conforme a definicao da fonte;\n")
cat("  na base pesquisada com Sports Value, trata-se de divida liquida.\n")
cat("* Folha salarial, gastos com contratacoes e investimento SAF\n")
cat("  so devem entrar em modelos se houver cobertura valida suficiente.\n")
cat("* Resultados estatisticos nao substituem a discussao de identificacao\n")
cat("  causal, selecao para adocao da SAF e tendencias anteriores.\n")

cat("\n========== SCRIPT CONCLUIDO ==========\n")
cat("Arquivos salvos em:", getwd(), "\n")
cat("Confira os avisos exibidos no Console antes de interpretar os resultados.\n")
