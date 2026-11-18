# =============================================================================
# 01_dati.R — Scarica i dati della lezione e ne salva uno snapshot in data/
#
# Lezione: "Le azioni proteggono dall'inflazione?"
# Fonti:   Kenneth R. French Data Library (pacchetto frenchdata)
#          FRED, Federal Reserve Bank of St. Louis (pacchetto quantmod)
#
# Uso (dalla cartella seminario_inflazione_azioni/):
#   Rscript R/01_dati.R
#
# Lo script sovrascrive i file in data/. La lezione legge solo lo snapshot,
# quindi una volta scaricati i dati funziona anche senza connessione e i
# risultati restano riproducibili (Ken French e FRED rivedono le serie).
# =============================================================================

# ---- Pacchetti --------------------------------------------------------------
pacchetti <- c("frenchdata", "quantmod", "dplyr")
mancanti  <- pacchetti[!vapply(pacchetti, requireNamespace, logical(1), quietly = TRUE)]
if (length(mancanti) > 0) {
  stop("Installare prima: install.packages(c(",
       paste0('"', mancanti, '"', collapse = ", "), "))")
}
# FRED ha cambiato l'indirizzo di download: con quantmod 0.4.26 il download
# fallisce, con la 0.4.29 funziona (verificato a novembre 2026).
if (packageVersion("quantmod") < "0.4.29") {
  stop("Serve quantmod >= 0.4.29 (installata: ", packageVersion("quantmod"),
       "). Aggiornare con install.packages(\"quantmod\").")
}

suppressPackageStartupMessages({
  library(frenchdata)
  library(quantmod)
  library(dplyr)
})

dir.create("data", showWarnings = FALSE)

# ---- Funzioni di supporto ---------------------------------------------------

# Converte le date di Ken French (AAAAMM, numeriche) nel primo giorno del mese
data_french <- function(x) as.Date(paste0(x, "01"), format = "%Y%m%d")

# Estrae da un dataset di frenchdata il sottoinsieme con il nome indicato
# (stringa vuota = primo sottoinsieme, cioè i dati mensili dei fattori)
sottoinsieme <- function(dataset, nome) {
  s <- dataset$subsets
  i <- if (nome == "") 1 else which(s$name == nome)
  if (length(i) != 1) stop("Sottoinsieme non trovato: ", nome)
  out <- s$data[[i]]
  if (any(nchar(as.character(out$date)) != 6)) stop("Il sottoinsieme non è mensile: ", nome)
  out$date <- data_french(out$date)
  out
}

# Scarica una serie mensile da FRED con quantmod. FRED non garantisce le
# richieste senza chiave API: se quantmod fallisce, si legge direttamente il
# CSV pubblico di FRED (stessa fonte, nessun pacchetto aggiuntivo).
scarica_fred <- function(codice) {
  x <- tryCatch(
    suppressWarnings(getSymbols(codice, src = "FRED", auto.assign = FALSE)),
    error = function(e) NULL
  )
  if (!is.null(x)) {
    out <- data.frame(date = as.Date(index(x)), valore = as.numeric(x[, 1]))
  } else {
    message("quantmod non ha scaricato ", codice, ": uso il CSV di FRED")
    url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", codice)
    out <- utils::read.csv(url, na.strings = ".")
    names(out) <- c("date", "valore")
    out$date <- as.Date(out$date)
  }
  names(out)[2] <- codice
  out
}

# ---- 1. Fattori di Fama e French (mercato USA e tasso privo di rischio) -----
# Mkt-RF: rendimento in eccesso del mercato (tutte le azioni NYSE, AMEX e
#         NASDAQ, ponderate per capitalizzazione), % mensile
# RF:     rendimento del T-bill a un mese, % mensile
ff3 <- download_french_data("Fama/French 3 Factors") |>
  sottoinsieme("") |>
  transmute(date, mkt_rf = `Mkt-RF`, smb = SMB, hml = HML, rf = RF)

# ---- 2. Dodici portafogli settoriali (ponderati per capitalizzazione) -------
ind12 <- download_french_data("12 Industry Portfolios") |>
  sottoinsieme("Average Value Weighted Returns -- Monthly")

# ---- 3. Fattori europei (per l'esercizio sull'area euro; in dollari USA) ----
ff3_eu <- download_french_data("Fama/French European 3 Factors") |>
  sottoinsieme("") |>
  transmute(date, mkt_rf_eu = `Mkt-RF`, rf_us = RF)

# ---- 4. Serie macroeconomiche da FRED ---------------------------------------
serie_fred <- c(
  CPIAUCSL           = "Indice dei prezzi al consumo (CPI-U), destagionalizzato",
  CPILFESL           = "CPI al netto di alimentari ed energia (core), destagionalizzato",
  MICH               = "Inflazione attesa a 12 mesi, Università del Michigan (%)",
  EXPINF1YR          = "Inflazione attesa a 1 anno, modello della Cleveland Fed (%)",
  INDPRO             = "Indice della produzione industriale",
  CP0000EZ19M086NEST = "HICP area euro (indice, non destagionalizzato)",
  EXUSEU             = "Cambio dollari per euro (media mensile)"
)
macro <- Reduce(
  function(a, b) full_join(a, b, by = "date"),
  lapply(names(serie_fred), scarica_fred)
) |> arrange(date)

# ---- 5. Unione e salvataggio ------------------------------------------------
mensile <- ff3 |>
  full_join(ff3_eu, by = "date") |>
  full_join(macro, by = "date") |>
  arrange(date) |>
  rename(cpi = CPIAUCSL, cpi_core = CPILFESL, mich = MICH,
         expinf_cleveland = EXPINF1YR, indpro = INDPRO,
         hicp_ea = CP0000EZ19M086NEST, usd_per_eur = EXUSEU) |>
  select(-rf_us)

write.csv(mensile, "data/mensile.csv", row.names = FALSE)
write.csv(ind12,   "data/industrie12.csv", row.names = FALSE)

ultima <- function(x, v) format(max(x$date[!is.na(x[[v]])]), "%Y-%m")
metadati <- c(
  paste("Scaricato il:", format(Sys.Date())),
  "",
  "data/mensile.csv (% mensili per i rendimenti; livelli per indici e tassi)",
  "  mkt_rf, smb, hml, rf: Fama/French 3 Factors (Kenneth R. French Data Library)",
  "  mkt_rf_eu: Fama/French European 3 Factors, rendimento in eccesso in USD",
  paste0("  ", names(serie_fred), ": ", serie_fred, " (FRED)"),
  "",
  "data/industrie12.csv: 12 Industry Portfolios, Average Value Weighted Returns -- Monthly",
  "",
  "Ultima osservazione disponibile:",
  paste("  Fattori USA:", ultima(mensile, "mkt_rf")),
  paste("  Industrie:  ", ultima(ind12, "Enrgy")),
  paste("  CPI:        ", ultima(mensile, "cpi")),
  paste("  Michigan:   ", ultima(mensile, "mich")),
  paste("  Produzione: ", ultima(mensile, "indpro"))
)
writeLines(metadati, "data/LEGGIMI_dati.txt")
cat(metadati, sep = "\n")
