# Le azioni proteggono dall'inflazione?

Materiale per un seminario di econometria finanziaria (corso triennale, 60-90
minuti): lezione in R Markdown, slide, script R ed esercizi con soluzioni. Usa
solo dati pubblici, scaricati con due pacchetti R.

**Domanda di ricerca.** I rendimenti azionari compensano l'inflazione, attesa e
inattesa? La relazione è stabile nel tempo? I settori che in passato hanno
protetto dall'inflazione l'hanno fatto anche nel 2021-22?

**Metodi.** Regressione di Fama e Schwert (1977), tre misure dell'inflazione
attesa (T-bill, AR ricorsivo, sondaggio Michigan), regressione di Mincer e
Zarnowitz, errori standard di Newey-West, test di Wald, stime su finestre
mobili, test di rottura strutturale, variabile omessa (ipotesi proxy di Fama,
1981), verifica fuori campione.

## Perché questo tema

Il tema è stato scelto perché:

- i dati sono tutti pubblici e si scaricano con due pacchetti R (`frenchdata`
  per la Kenneth R. French Data Library, `quantmod` per FRED), senza
  registrazioni né chiavi;
- il modello è una regressione lineare, ma porta a temi econometrici avanzati e
  spiegabili: variabili non osservate, inferenza robusta, instabilità dei
  parametri, variabili omesse, valutazione fuori campione;
- la letteratura è consolidata (Fama e Schwert, 1977; Fama, 1981) e ha sviluppi
  recenti (Boons et al., 2020; Cieslak e Pflueger, 2023), e l'inflazione del
  2021-22 dà un caso concreto da discutere in aula.

## Contenuto

| File | Cosa contiene |
|---|---|
| `lezione.Rmd` | Dispensa completa per gli studenti: testo, codice, tabelle, figure, esercizi, riferimenti |
| `slides.Rmd` | 15 slide (ioslides) per presentare il seminario |
| `R/01_dati.R` | Scarica i dati da Ken French e FRED e ne salva una copia in `data/` |
| `R/02_analisi.R` | Tutta l'analisi; lezione e slide ne leggono i blocchi di codice |
| `R/03_soluzioni_esercizi.R` | Soluzioni dei sei esercizi |
| `data/` | Creata da `R/01_dati.R` al primo avvio; non è nel repository (vedi *Note sui dati*) |

`R/02_analisi.R` è l'unica fonte del codice: `lezione.Rmd` e `slides.Rmd` ne
leggono i blocchi `## ---- nome ----` con `knitr::read_chunk()`. Una modifica
allo script si riflette quindi in entrambi i documenti.

## Come eseguire

Dalla cartella `seminario_inflazione_azioni/`.

Pacchetti necessari (una volta sola):

```r
install.packages(c("frenchdata", "quantmod", "dplyr", "tidyr", "ggplot2",
                   "ggrepel", "sandwich", "lmtest", "car", "strucchange",
                   "rmarkdown", "knitr"))
```

Serve `quantmod` 0.4.29 o successivo: con la 0.4.26 il download da FRED non
funziona più. Se `getSymbols()` fallisce comunque, `R/01_dati.R` legge
direttamente il CSV pubblico di FRED.

Scaricare i dati. Non è obbligatorio: lezione, slide e script lo fanno da soli
al primo avvio. Rieseguirlo aggiorna i dati e sovrascrive la copia in `data/`.

```bash
Rscript R/01_dati.R
```

Compilare la lezione e le slide:

```bash
Rscript -e 'rmarkdown::render("lezione.Rmd")'
```

```bash
Rscript -e 'rmarkdown::render("slides.Rmd")'
```

Eseguire l'analisi come script (tabelle e figure in `output/`) e le soluzioni:

```bash
Rscript R/02_analisi.R
```

```bash
Rscript R/03_soluzioni_esercizi.R
```

Le slide richiedono `rmarkdown` installato da CRAN: il pacchetto di sistema
delle distribuzioni Debian e Ubuntu (`r-cran-rmarkdown`) non include i font di
ioslides e la compilazione si ferma con un errore su `fonts.css`.

## Scaletta del seminario

| Minuti (90') | Minuti (60') | Blocco | Materiale |
|---|---|---|---|
| 0-10 | 0-7 | La domanda e il fatto stilizzato | slide 2-3, lezione § 1 |
| 10-20 | 7-14 | Il modello di Fama e Schwert e le ipotesi | slide 4, lezione § 2 |
| 20-30 | 14-20 | Dati e R: download in diretta, il CPI mancante di ottobre 2025 | slide 5, lezione § 3 |
| 30-42 | 20-28 | Misurare l'inflazione attesa e valutarne le previsioni | slide 6-7, lezione § 4 |
| 42-57 | 28-40 | Stima con errori di Newey-West e test di Wald | slide 8-9, lezione § 5 |
| 57-69 | 40-48 | Instabilità: sottocampioni, finestre mobili, test | slide 10-11, lezione § 6 |
| 69-77 | — | Ipotesi proxy e variabile omessa | slide 12, lezione § 7 |
| 77-87 | 48-56 | Settori e verifica fuori campione 2021-22 | slide 13-14, lezione § 8 |
| 87-90 | 56-60 | Cosa portare a casa, esercizi | slide 15 |

Nella versione da 60 minuti si salta l'ipotesi proxy (si cita solo nella
chiusura) e si mostra il grafico delle finestre mobili senza la tabella dei
sottocampioni.

## Risultati principali (dati scaricati il 18 novembre 2026)

- Su 1953-2026 i coefficienti dell'inflazione attesa e inattesa sono negativi
  (circa -1) e la copertura completa è rifiutata (p = 0,010).
- Il modello AR(3) ricorsivo prevede l'inflazione mensile meglio del T-bill e
  del sondaggio Michigan (RMSE 0,249 contro 0,331 e 0,265, dal 1978).
- La sensibilità all'inflazione inattesa è -2,97 nel 1953-1999 e +1,26 nel
  2000-2020; la differenza è significativa (p = 0,002).
- Controllando per la crescita futura della produzione, il coefficiente
  dell'inflazione attesa passa da -1,36 a -0,08 (ipotesi proxy).
- L'energia, unico settore con sensibilità positiva fino al 2020, ha reso il
  133% reale nel 2021-22. Correlazione tra sensibilità storica e rendimento
  2021-22: 0,78 con Pearson, 0,21 con Spearman, -0,03 con Spearman senza
  l'energia.

## Note sui dati

- I dati non sono versionati: il repository è pubblico e alcune serie, come il
  sondaggio dell'Università del Michigan, sono protette da copyright. Ogni
  utente li scarica con `R/01_dati.R` (gratis, senza registrazione). Ken French
  e FRED rivedono periodicamente le serie, quindi un download successivo può
  dare numeri leggermente diversi da quelli riportati qui; la copia in `data/`
  rende stabili i risultati sulla propria macchina.
- Rendimenti: `Fama/French 3 Factors` (mercato = Mkt-RF + RF) e
  `12 Industry Portfolios` (ponderati per capitalizzazione), % mensili.
- Inflazione: CPI-U destagionalizzato (`CPIAUCSL`), variazione logaritmica
  mensile. Il CPI di ottobre 2025 non è stato pubblicato (chiusura del governo
  federale): lo si ricostruisce per interpolazione geometrica tra settembre e
  novembre.
- Inflazione attesa del sondaggio: `MICH` (12 mesi successivi, diviso per 12).
- Le previsioni AR usano i dati attuali, già rivisti, e non quelli disponibili
  in tempo reale.
- Per gli esercizi `R/01_dati.R` scarica anche il CPI core (`CPILFESL`),
  l'inflazione attesa della Cleveland Fed (`EXPINF1YR`), l'HICP dell'area euro,
  il cambio dollaro/euro e i fattori europei di Ken French.
