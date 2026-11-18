# =============================================================================
# 03_soluzioni_esercizi.R — Soluzioni degli esercizi della lezione
#
# Uso (dalla cartella seminario_inflazione_azioni/):
#   Rscript R/03_soluzioni_esercizi.R
#
# Riusa dati, funzioni e oggetti di R/02_analisi.R (senza salvare output).
# =============================================================================

SALVA_OUTPUT <- FALSE
source("R/02_analisi.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(strucchange))

titolo <- function(x) cat("\n", strrep("=", 70), "\n", x, "\n", strrep("=", 70), "\n", sep = "")
mostra <- function(righe) {
  print(righe |>
          transmute(Specificazione, Campione,
                    beta  = paste0(f2(beta),  " (", f2(se_beta),  ")"),
                    gamma = paste0(f2(gamma), " (", f2(se_gamma), ")"),
                    `p: beta = gamma = 1` = fp(p_copertura), N),
        row.names = FALSE)
}

# -----------------------------------------------------------------------------
titolo("Esercizio 1. Replica del campione di Fama e Schwert (1953-1971)")
fs7 <- campione |> filter(date <= as.Date("1971-12-01"))
mostra(bind_rows(stima_fs(fs7, "e_tb", "T-bill")$riga,
                 stima_fs(fs7, "e_ar", "AR(3)")$riga))
# Come in Fama e Schwert (1977), entrambi i coefficienti sono negativi, e quello
# dell'inflazione attesa è grande e significativo: nel 1953-1971 un punto di
# inflazione attesa in più si associava a rendimenti nominali più bassi.

# -----------------------------------------------------------------------------
titolo("Esercizio 2. Sensibilità all'ordine del modello AR")
righe_p <- bind_rows(lapply(c(1, 3, 6, 12), function(p) {
  z <- d |> mutate(e_p = prev_ar(pi, p = p)) |>
    filter(date >= as.Date("1953-01-01"), !is.na(R), !is.na(e_p))
  stima_fs(z, "e_p", paste0("AR(", p, ")"))$riga
}))
mostra(righe_p)
# Le stime cambiano poco con l'ordine del modello: il risultato non dipende
# da questa scelta.

# -----------------------------------------------------------------------------
titolo("Esercizio 3. Inflazione di fondo (CPI senza alimentari ed energia)")
core <- d |>
  filter(!is.na(cpi_core)) |>
  mutate(pi_core = 100 * log(cpi_core / lag(cpi_core))) |>
  filter(!is.na(pi_core)) |>
  mutate(e_core = prev_ar(pi_core)) |>
  filter(!is.na(e_core), !is.na(R)) |>
  mutate(pi = pi_core)                     # stima_fs usa la colonna pi
mostra(stima_fs(core, "e_core", "AR(3), CPI core")$riga)
# Il campione parte dal 1962 (il CPI core inizia nel 1957 e il modello AR
# richiede cinque anni di dati). Con l'inflazione di fondo, che esclude energia
# e alimentari, gamma è più negativo e significativo che con il CPI generale.
# Ipotesi da discutere in aula: le sorprese sui prezzi energetici sono buone
# notizie per una parte del mercato (il settore energia) e attenuano la
# relazione misurata con il CPI generale.

# -----------------------------------------------------------------------------
titolo("Esercizio 4. Orizzonte annuale: sondaggio Michigan contro T-bill")
ann <- d |>
  filter(!is.na(R)) |>
  mutate(anno = as.integer(format(date, "%Y"))) |>
  group_by(anno) |>
  summarise(mesi = n(),
            R    = 100 * (prod(1 + R / 100) - 1),
            TB   = 100 * (prod(1 + rf / 100) - 1),     # rendimento del T-bill nell'anno
            infl = 100 * (exp(sum(pi) / 100) - 1),
            mich_dic = last(mich), .groups = "drop") |>
  filter(mesi == 12) |>
  mutate(e_mich = lag(mich_dic)) |>                   # attesa a dicembre dell'anno prima
  filter(!is.na(e_mich))
cat("Anni:", min(ann$anno), "-", max(ann$anno), "\n")
cat("RMSE T-bill:  ", f2(sqrt(mean((ann$infl - ann$TB)^2))), "\n")
cat("RMSE Michigan:", f2(sqrt(mean((ann$infl - ann$e_mich)^2))), "\n")
m_ann <- lm(R ~ e_mich + I(infl - e_mich), data = ann)
print(coeftest(m_ann, vcov. = vcov_hac(m_ann, lag = 2)))
# Su orizzonte annuale il sondaggio prevede meglio del T-bill, in linea con
# Ang, Bekaert e Wei (2007). Con circa 45 osservazioni annue, però, la
# regressione dei rendimenti è molto imprecisa.

# -----------------------------------------------------------------------------
titolo("Esercizio 5. Area euro, orizzonte di 12 mesi")
# Il rendimento europeo di Ken French è in dollari: lo si converte in euro con
# il cambio dollari per euro (media mensile, un'approssimazione del cambio di
# fine mese). L'HICP non è destagionalizzato: le variazioni su 12 mesi
# eliminano la stagionalità.
eu <- mensile |>
  arrange(date) |>
  mutate(r_eur = 100 * log((1 + (mkt_rf_eu + rf) / 100) * lag(usd_per_eur) / usd_per_eur),
         r_usa = 100 * log(1 + (mkt_rf + rf) / 100),
         pi_ea = 100 * log(hicp_ea / lag(hicp_ea)),
         pi_us = 100 * log(cpi / lag(cpi))) |>
  mutate(across(c(r_eur, r_usa, pi_ea, pi_us),
                ~ as.numeric(stats::filter(.x, rep(1, 12), sides = 1)),
                .names = "{.col}_12")) |>
  filter(date >= as.Date("2000-12-01"), !is.na(r_eur_12), !is.na(pi_ea_12))
cat("Periodo:", mese(min(eu$date)), "-", mese(max(eu$date)), " (", nrow(eu), "mesi, sovrapposti)\n")
m_eu <- lm(I(r_eur_12 - pi_ea_12) ~ pi_ea_12, data = eu)    # rendimento reale su inflazione
m_us <- lm(I(r_usa_12 - pi_us_12) ~ pi_us_12, data = eu)
cat("\nArea euro: rendimento reale a 12 mesi su inflazione a 12 mesi\n")
print(coeftest(m_eu, vcov. = vcov_hac(m_eu, lag = 18)))
cat("\nUSA, stesso periodo\n")
print(coeftest(m_us, vcov. = vcov_hac(m_us, lag = 18)))
# Nell'area euro il rendimento reale è significativamente più basso quando
# l'inflazione è alta; negli USA, nello stesso periodo, la stima è vicina a zero.
# Con finestre di 12 mesi sovrapposte, due osservazioni consecutive hanno 11
# mesi in comune: i residui sono autocorrelati per costruzione almeno fino al
# ritardo 11. Errori standard OLS sarebbero molto troppo piccoli.

# -----------------------------------------------------------------------------
titolo("Esercizio 6. Test sup-F di Andrews (1993) con data di rottura ignota")
fs_stat <- Fstats(R ~ attesa + inattesa, data = campione, from = 0.15,
                  vcov. = function(x, ...) NeweyWest(x, lag = LAG_HAC, prewhite = FALSE))
print(sctest(fs_stat, type = "supF"))
cat("Data di rottura stimata:", mese(campione$date[fs_stat$breakpoint]), "\n")
# Il test sup-F verifica la stabilità di TUTTI i coefficienti, intercetta
# compresa, e cerca la data che massimizza la statistica F. La data stimata può
# quindi riflettere cambiamenti del rendimento medio, non solo di gamma, e
# differire dal 2000 suggerito dalle finestre mobili.
