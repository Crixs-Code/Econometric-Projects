# =============================================================================
# 02_analisi.R — Analisi della lezione "Le azioni proteggono dall'inflazione?"
#
# Legge lo snapshot creato da R/01_dati.R e stima, nell'ordine:
#   1. il fatto stilizzato (rendimenti reali annui e inflazione);
#   2. tre misure dell'inflazione attesa e la loro capacità previsiva;
#   3. la regressione di Fama e Schwert (1977) con errori standard HAC;
#   4. l'instabilità della relazione (sottocampioni, finestra mobile, test);
#   5. l'ipotesi "proxy" di Fama (1981): il ruolo dell'attività reale;
#   6. l'eterogeneità settoriale e la verifica fuori campione sul 2021-22.
#
# Uso (dalla cartella seminario_inflazione_azioni/):
#   Rscript R/02_analisi.R          -> tabelle e figure in output/
#
# I blocchi "## ---- nome ----" sono letti da lezione.Rmd e slides.Rmd con
# knitr::read_chunk(): il codice esiste in un solo posto.
# =============================================================================

## ---- setup ----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(sandwich)   # matrici di covarianza robuste (Newey-West)
  library(lmtest)     # coeftest(): test t con covarianza robusta
  library(car)        # linearHypothesis(): test di Wald su vincoli lineari
})

# Lo script salva figure e tabelle in output/; la lezione no
if (!exists("SALVA_OUTPUT")) SALVA_OUTPUT <- TRUE
if (SALVA_OUTPUT) dir.create("output", showWarnings = FALSE)

# Colori (palette categoriale validata: blu, arancio, verde acqua) e stile
COL <- c(blu = "#2a78d6", arancio = "#eb6834", acqua = "#1baf7a",
         inchiostro = "#0b0b0b", secondario = "#52514e", tenue = "#898781",
         griglia = "#e1e0d9", base = "#c3c2b7", banda = "#b7d3f6",
         sfondo = "#fcfcfb")

theme_lezione <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = COL[["sfondo"]], colour = NA),
      panel.grid.major = element_line(colour = COL[["griglia"]], linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(colour = COL[["tenue"]]),
      axis.title       = element_text(colour = COL[["secondario"]]),
      plot.title       = element_text(colour = COL[["inchiostro"]], face = "bold"),
      plot.subtitle    = element_text(colour = COL[["secondario"]]),
      plot.caption     = element_text(colour = COL[["tenue"]], hjust = 0),
      strip.text       = element_text(colour = COL[["inchiostro"]], face = "bold"),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(colour = COL[["secondario"]])
    )
}
theme_set(theme_lezione())

salva_fig <- function(p, nome, w = 8, h = 5) {
  if (SALVA_OUTPUT) ggsave(file.path("output", paste0(nome, ".png")), p,
                           width = w, height = h, dpi = 200)
  invisible(p)
}
salva_tab <- function(x, nome) {
  if (SALVA_OUTPUT) write.csv(x, file.path("output", paste0(nome, ".csv")), row.names = FALSE)
  invisible(x)
}

# Errori standard di Newey e West (1987): robusti a eteroschedasticità e
# autocorrelazione dei residui fino a 12 mesi
LAG_HAC <- 12
vcov_hac <- function(m, lag = LAG_HAC) NeweyWest(m, lag = lag, prewhite = FALSE)

# Formattazione all'italiana (virgola decimale) per tabelle e testo
f1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
f2 <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
f3 <- function(x) formatC(x, format = "f", digits = 3, decimal.mark = ",")
fp <- function(p) ifelse(p < 0.001, "<0,001", f3(p))
mese <- function(x) format(x, "%m/%Y")

## ---- dati ----
# Al primo avvio i dati non ci sono ancora: li scarica R/01_dati.R
if (!file.exists("data/mensile.csv")) source("R/01_dati.R", encoding = "UTF-8")
mensile <- read.csv("data/mensile.csv") |> mutate(date = as.Date(date))
ind12   <- read.csv("data/industrie12.csv") |> mutate(date = as.Date(date))

# Il CPI di ottobre 2025 non è mai stato pubblicato (chiusura del governo
# federale USA): lo si ricostruisce per interpolazione geometrica tra settembre
# e novembre, cioè si divide in parti uguali la variazione dei due mesi.
interpola_geom <- function(x) {
  ok <- which(!is.na(x))
  exp(approx(ok, log(x[ok]), xout = seq_along(x), rule = 1)$y)
}
mesi_imputati <- mensile$date[is.na(mensile$cpi) &
                                mensile$date > min(mensile$date[!is.na(mensile$cpi)]) &
                                mensile$date < max(mensile$date[!is.na(mensile$cpi)])]
mensile <- mensile |> mutate(cpi = interpola_geom(cpi), cpi_core = interpola_geom(cpi_core))
mesi_imputati

# Inflazione mensile in % (variazione logaritmica del CPI destagionalizzato)
d <- mensile |>
  filter(!is.na(cpi)) |>
  arrange(date) |>
  mutate(
    pi   = 100 * log(cpi / lag(cpi)),
    R    = mkt_rf + rf,                                    # rendimento nominale del mercato
    g12  = 100 * (log(lead(indpro, 12)) - log(indpro))     # crescita della produzione nei 12 mesi successivi
  ) |>
  filter(!is.na(pi))

## ---- misure-attese ----
# Previsione AR(p) ricorsiva: per il mese t si stimano i coefficienti con i
# soli dati fino a t-1 (finestra crescente), quindi senza usare il futuro.
prev_ar <- function(y, p = 3, min_oss = 60) {
  n <- length(y)
  out <- rep(NA_real_, n)
  for (t in (min_oss + p + 1):n) {
    yy <- y[1:(t - 1)]
    X  <- embed(yy, p + 1)                       # colonne: y_s, y_{s-1}, ..., y_{s-p}
    b  <- qr.solve(cbind(1, X[, -1, drop = FALSE]), X[, 1])
    out[t] <- sum(b * c(1, yy[(t - 1):(t - p)]))
  }
  out
}

d <- d |>
  mutate(
    e_tb   = rf,               # 1. tasso del T-bill a un mese (Fama, 1975)
    e_ar   = prev_ar(pi),      # 2. modello AR(3) ricorsivo
    e_mich = lag(mich) / 12    # 3. sondaggio Michigan del mese precedente, in % mensile
  )

# Campione della lezione: da gennaio 1953 (come Fama e Schwert, 1977)
# all'ultimo mese con i rendimenti di Ken French
campione <- d |> filter(date >= as.Date("1953-01-01"), !is.na(R), !is.na(e_ar))
c(inizio = mese(min(campione$date)), fine = mese(max(campione$date)), N = nrow(campione))

## ---- fig-motivazione ----
annuale <- d |>
  filter(!is.na(R)) |>
  mutate(anno = as.integer(format(date, "%Y"))) |>
  group_by(anno) |>
  summarise(mesi = n(),
            R    = 100 * (prod(1 + R / 100) - 1),       # rendimento nominale annuo
            infl = 100 * (exp(sum(pi) / 100) - 1),      # inflazione dicembre su dicembre
            .groups = "drop") |>
  filter(mesi == 12, anno >= 1953) |>
  mutate(R_reale = 100 * ((1 + R / 100) / (1 + infl / 100) - 1),
         evidenza = anno %in% c(1973, 1974, 1979, 1980, 2021, 2022))

cor_annua <- cor(annuale$R_reale, annuale$infl)

p_motivazione <- ggplot(annuale, aes(infl, R_reale)) +
  geom_hline(yintercept = 0, colour = COL[["base"]]) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = COL[["secondario"]], linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(colour = evidenza), size = 2.6) +
  geom_text_repel(data = filter(annuale, evidenza), aes(label = anno),
                  colour = COL[["inchiostro"]], size = 3.6, seed = 1) +
  scale_colour_manual(values = c(`FALSE` = COL[["blu"]], `TRUE` = COL[["arancio"]]),
                      labels = c("Altri anni", "Anni di inflazione alta"), name = NULL) +
  labs(title = "Quando l'inflazione sale, i rendimenti reali delle azioni scendono",
       subtitle = sprintf("Mercato azionario USA, %d-%d: correlazione %s",
                          min(annuale$anno), max(annuale$anno), f2(cor_annua)),
       x = "Inflazione dell'anno (%, dicembre su dicembre)",
       y = "Rendimento reale annuo (%)",
       caption = "Fonti: Kenneth R. French Data Library, FRED (CPI-U).")
salva_fig(p_motivazione, "fig1_motivazione")
p_motivazione

## ---- tab-misure ----
# Confronto delle tre misure sul campione comune (dal 1978, quando inizia il Michigan)
comune <- campione |> filter(!is.na(e_mich))

valuta_misura <- function(e, nome) {
  err <- comune$pi - comune[[e]]
  mz  <- lm(comune$pi ~ comune[[e]])          # regressione di Mincer e Zarnowitz
  w   <- linearHypothesis(mz, c("(Intercept) = 0", "comune[[e]] = 1"),
                          vcov. = vcov_hac(mz))
  data.frame(Misura = nome,
             `Errore medio` = mean(err),
             RMSE = sqrt(mean(err^2)),
             `Pendenza MZ` = unname(coef(mz)[2]),
             `p (a=0, b=1)` = w[2, "Pr(>F)"],
             check.names = FALSE)
}
tab_misure <- bind_rows(
  valuta_misura("e_tb",   "T-bill a un mese"),
  valuta_misura("e_ar",   "AR(3) ricorsivo"),
  valuta_misura("e_mich", "Sondaggio Michigan / 12")
)
salva_tab(tab_misure, "tab1_misure")
tab_misure |>
  mutate(across(c(`Errore medio`, RMSE, `Pendenza MZ`), f3),
         `p (a=0, b=1)` = fp(`p (a=0, b=1)`))

## ---- fig-misure ----
# Somme su 12 mesi: inflazione realizzata e attesa sullo stesso orizzonte
dodici <- function(x) as.numeric(stats::filter(x, rep(1, 12), sides = 1))
serie_misure <- campione |>
  transmute(date,
            Realizzata = dodici(pi),
            `T-bill a un mese` = dodici(e_tb),
            `AR(3) ricorsivo`  = dodici(e_ar),
            `Sondaggio Michigan` = dodici(e_mich)) |>
  pivot_longer(-c(date, Realizzata), names_to = "misura", values_to = "attesa") |>
  mutate(misura = factor(misura, levels = c("T-bill a un mese", "AR(3) ricorsivo",
                                            "Sondaggio Michigan")))

p_misure <- ggplot(serie_misure, aes(date)) +
  geom_line(aes(y = Realizzata, linetype = "Inflazione realizzata"),
            colour = COL[["tenue"]], linewidth = 0.5, na.rm = TRUE) +
  geom_line(aes(y = attesa, colour = misura), linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(~ misura, ncol = 1) +
  scale_colour_manual(values = unname(COL[c("blu", "arancio", "acqua")]), guide = "none") +
  scale_linetype_manual(values = "solid") +
  labs(title = "Tre modi di misurare l'inflazione attesa",
       subtitle = "Inflazione su 12 mesi: realizzata (grigio) e attesa secondo ciascuna misura (%)",
       x = NULL, y = NULL,
       caption = "Le misure attese sono somme delle previsioni mensili sugli stessi 12 mesi.")
salva_fig(p_misure, "fig2_misure", h = 7)
p_misure

## ---- stima-fs ----
# Regressione di Fama e Schwert (1977):
#   R_t = alpha + beta * E_{t-1}[pi_t] + gamma * (pi_t - E_{t-1}[pi_t]) + e_t
# Copertura completa: beta = 1 (inflazione attesa) e gamma = 1 (inattesa).
stima_fs <- function(dati, e, etichetta = e) {
  dati$attesa   <- dati[[e]]
  dati$inattesa <- dati$pi - dati$attesa
  m  <- lm(R ~ attesa + inattesa, data = dati)
  V  <- vcov_hac(m)
  ct <- coeftest(m, vcov. = V)
  w  <- linearHypothesis(m, c("attesa = 1", "inattesa = 1"), vcov. = V)
  list(modello = m, ct = ct,
       riga = data.frame(
         Specificazione = etichetta,
         Campione = paste(format(min(dati$date), "%Y"), format(max(dati$date), "%Y"), sep = "-"),
         beta = ct["attesa", 1],   se_beta = ct["attesa", 2],   p_beta = ct["attesa", 4],
         gamma = ct["inattesa", 1], se_gamma = ct["inattesa", 2], p_gamma = ct["inattesa", 4],
         p_copertura = w[2, "Pr(>F)"],
         R2 = summary(m)$r.squared,
         N = nobs(m)))
}

fs <- list(
  stima_fs(campione, "e_tb",   "T-bill"),
  stima_fs(campione, "e_ar",   "AR(3)"),
  stima_fs(comune,   "e_ar",   "AR(3)"),
  stima_fs(comune,   "e_mich", "Michigan")
)
tab_fs <- bind_rows(lapply(fs, `[[`, "riga"))
salva_tab(tab_fs, "tab2_fama_schwert")

# Tabella leggibile: coefficienti con errore standard HAC tra parentesi
tab_fs_testo <- tab_fs |>
  transmute(Specificazione, Campione,
            `beta (attesa)`    = paste0(f2(beta),  " (", f2(se_beta),  ")"),
            `gamma (inattesa)` = paste0(f2(gamma), " (", f2(se_gamma), ")"),
            `p: beta = gamma = 1` = fp(p_copertura),
            R2 = f2(R2), N)
tab_fs_testo

## ---- fig-coefficienti ----
etichette_spec <- paste0(tab_fs$Specificazione, " (", tab_fs$Campione, ")")
p_coef <- bind_rows(
  tab_fs |> transmute(Specificazione, Campione, coef = "beta: inflazione attesa",
                      stima = beta, se = se_beta),
  tab_fs |> transmute(Specificazione, Campione, coef = "gamma: inflazione inattesa",
                      stima = gamma, se = se_gamma)
) |>
  mutate(spec = factor(paste0(Specificazione, " (", Campione, ")"),
                       levels = rev(etichette_spec))) |>
  ggplot(aes(stima, spec)) +
  geom_vline(xintercept = 0, colour = COL[["base"]]) +
  geom_vline(xintercept = 1, colour = COL[["arancio"]], linetype = "dashed") +
  geom_errorbarh(aes(xmin = stima - 1.96 * se, xmax = stima + 1.96 * se),
                 height = 0.2, colour = COL[["blu"]], linewidth = 0.6) +
  geom_point(colour = COL[["blu"]], size = 2.8) +
  facet_wrap(~ coef) +
  labs(title = "Le azioni non coprono l'inflazione: le stime sono lontane da 1",
       subtitle = "Coefficienti con intervallo di confidenza al 95% (errori standard di Newey-West)",
       x = "Coefficiente (la linea tratteggiata è la copertura completa)", y = NULL)
salva_fig(p_coef, "fig3_coefficienti", h = 4.5)
p_coef

## ---- sottocampioni ----
periodi <- list(c("1953-01-01", "1999-12-01"),
                c("2000-01-01", "2020-12-01"),
                c("2021-01-01", "2100-01-01"))
tab_sotto <- bind_rows(lapply(periodi, function(p) {
  z <- campione |> filter(date >= as.Date(p[1]), date <= as.Date(p[2]))
  stima_fs(z, "e_ar", "AR(3)")$riga
}))
salva_tab(tab_sotto, "tab3_sottocampioni")

# Test formale: gamma cambia dopo il 2000? (interazione con una dummy, HAC)
campione <- campione |>
  mutate(attesa = e_ar, inattesa = pi - e_ar,
         dopo2000 = as.numeric(date >= as.Date("2000-01-01")))
m_rottura <- lm(R ~ (attesa + inattesa) * dopo2000, data = campione)
ct_rottura <- coeftest(m_rottura, vcov. = vcov_hac(m_rottura))

tab_sotto |>
  transmute(Campione,
            `beta (attesa)`    = paste0(f2(beta),  " (", f2(se_beta),  ")"),
            `gamma (inattesa)` = paste0(f2(gamma), " (", f2(se_gamma), ")"),
            `p: gamma = 0` = fp(p_gamma), N)

## ---- test-rottura ----
data.frame(
  Coefficiente = c("gamma fino al 1999", "variazione di gamma dal 2000"),
  Stima = f2(ct_rottura[c("inattesa", "inattesa:dopo2000"), 1]),
  `Errore std. HAC` = f2(ct_rottura[c("inattesa", "inattesa:dopo2000"), 2]),
  p = fp(ct_rottura[c("inattesa", "inattesa:dopo2000"), 4]),
  row.names = NULL, check.names = FALSE)

## ---- fig-rolling ----
# Coefficiente gamma stimato su finestre mobili di 10 anni (120 mesi)
FINESTRA <- 120
rolling <- bind_rows(lapply(FINESTRA:nrow(campione), function(t) {
  z <- campione[(t - FINESTRA + 1):t, ]
  m <- lm(R ~ attesa + inattesa, data = z)
  data.frame(date = z$date[FINESTRA],
             gamma = unname(coef(m)["inattesa"]),
             se = sqrt(vcov_hac(m)["inattesa", "inattesa"]))
}))

p_rolling <- ggplot(rolling, aes(date, gamma)) +
  annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2022-12-31"),
           ymin = -Inf, ymax = Inf, fill = COL[["griglia"]], alpha = 0.6) +
  geom_ribbon(aes(ymin = gamma - 1.96 * se, ymax = gamma + 1.96 * se),
              fill = COL[["banda"]], alpha = 0.8) +
  geom_hline(yintercept = 0, colour = COL[["base"]]) +
  geom_hline(yintercept = 1, colour = COL[["arancio"]], linetype = "dashed") +
  geom_line(colour = COL[["blu"]], linewidth = 0.8) +
  annotate("text", x = as.Date("2022-01-01"), y = max(rolling$gamma + 1.96 * rolling$se),
           label = "2021-22", colour = COL[["secondario"]], size = 3.4, vjust = 1) +
  labs(title = "La relazione tra azioni e inflazione inattesa cambia segno nel tempo",
       subtitle = "Coefficiente gamma su finestre mobili di 10 anni, con intervallo al 95% (HAC)",
       x = "Fine della finestra", y = "gamma",
       caption = "Inflazione attesa: AR(3) ricorsivo. Linea tratteggiata: copertura completa (gamma = 1).")
salva_fig(p_rolling, "fig4_rolling")
p_rolling

## ---- proxy ----
# Ipotesi proxy di Fama (1981): l'inflazione segnala una crescita reale più
# bassa; se si controlla per la crescita futura, il suo effetto si attenua.
proxy <- campione |> filter(!is.na(g12))
m_base  <- lm(R ~ attesa + inattesa, data = proxy)
m_proxy <- lm(R ~ attesa + inattesa + g12, data = proxy)
riga_proxy <- function(m, nome) {
  ct <- coeftest(m, vcov. = vcov_hac(m))
  data.frame(Modello = nome,
             `beta (attesa)`    = paste0(f2(ct["attesa", 1]),   " (", f2(ct["attesa", 2]),   ")"),
             `gamma (inattesa)` = paste0(f2(ct["inattesa", 1]), " (", f2(ct["inattesa", 2]), ")"),
             `crescita futura`  = if ("g12" %in% rownames(ct))
               paste0(f2(ct["g12", 1]), " (", f2(ct["g12", 2]), ")") else "",
             R2 = f2(summary(m)$r.squared), N = nobs(m),
             check.names = FALSE)
}
tab_proxy <- bind_rows(riga_proxy(m_base, "Senza attività reale"),
                       riga_proxy(m_proxy, "Con crescita della produzione a 12 mesi"))
salva_tab(tab_proxy, "tab4_proxy")
tab_proxy

## ---- industrie ----
# gamma per settore stimato fino al 2020, poi confronto con il 2021-22
settori <- setdiff(names(ind12), "date")
nomi_settori <- c(NoDur = "Beni non durevoli", Durbl = "Beni durevoli",
                  Manuf = "Manifattura", Enrgy = "Energia", Chems = "Chimica",
                  BusEq = "Tecnologia", Telcm = "Telecomunicazioni", Utils = "Utility",
                  Shops = "Commercio", Hlth = "Sanità", Money = "Finanza", Other = "Altro")

di <- campione |>
  select(date, pi, attesa, inattesa) |>
  inner_join(ind12, by = "date")
prima <- di |> filter(date <= as.Date("2020-12-01"))
dopo  <- di |> filter(date >= as.Date("2021-01-01"), date <= as.Date("2022-12-01"))
infl_cum <- 100 * (exp(sum(dopo$pi) / 100) - 1)

tab_ind <- bind_rows(lapply(settori, function(k) {
  m <- lm(prima[[k]] ~ attesa + inattesa, data = prima)
  data.frame(codice = k, Settore = nomi_settori[[k]],
             gamma = unname(coef(m)["inattesa"]),
             se = sqrt(vcov_hac(m)["inattesa", "inattesa"]),
             reale_2021_22 = 100 * (prod(1 + dopo[[k]] / 100) / (1 + infl_cum / 100) - 1))
})) |> arrange(desc(gamma))
mercato_2021_22 <- 100 * (prod(1 + filter(campione, date >= as.Date("2021-01-01"),
                                          date <= as.Date("2022-12-01"))$R / 100) /
                          (1 + infl_cum / 100) - 1)
cor_ind <- c(pearson  = cor(tab_ind$gamma, tab_ind$reale_2021_22),
             spearman = cor(tab_ind$gamma, tab_ind$reale_2021_22, method = "spearman"),
             spearman_senza_energia = with(filter(tab_ind, codice != "Enrgy"),
                                           cor(gamma, reale_2021_22, method = "spearman")))
salva_tab(tab_ind, "tab5_industrie")

## ---- fig-industrie ----
p_ind <- ggplot(tab_ind, aes(gamma, reale_2021_22)) +
  geom_hline(yintercept = 0, colour = COL[["base"]]) +
  geom_vline(xintercept = 0, colour = COL[["base"]]) +
  geom_point(colour = COL[["blu"]], size = 2.8) +
  geom_text_repel(aes(label = Settore), colour = COL[["inchiostro"]], size = 3.4, seed = 2) +
  labs(title = "L'energia, unico settore con gamma positivo, ha dominato il 2021-22",
       subtitle = sprintf("Inflazione cumulata 2021-22: %s%%. Mercato: %s%% in termini reali",
                          f1(infl_cum), f1(mercato_2021_22)),
       x = "gamma stimato 1953-2020 (sensibilità all'inflazione inattesa)",
       y = "Rendimento reale cumulato 2021-22 (%)",
       caption = "Portafogli settoriali di Ken French, ponderati per capitalizzazione.")
salva_fig(p_ind, "fig5_industrie")
p_ind

## ---- fine ----
if (SALVA_OUTPUT) message("Figure e tabelle salvate in output/")
