# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# CAPITOLO 3
# ==========
# le righe precedute dal commento "RCMDR: ..." sono prodotte da menu di Rcmdr
# e non sono eseguibili se non è caricato Rcmdr


# INIZIO SESSIONE ---------------------------------------------------------

# avviare RCommander
library(Rcmdr)

# libreria del laboratorio
library(LabRS)

# Da Menu di RCommander o da R: Impostare la directory di lavoro
# setwd("C:/.../Laboratorio R")

# Caricare l'area di lavoro precedentemente caricata
# load(".RData")


# DISTRIBUZIONI DI FREQUENZA ----------------------------------------------

# dati
data("MYSLID")

# RCMDR distribuzioni di frequenza
local({
  .Table <- with(MYSLID, table(Lingua))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

# table - una variabile
table(MYSLID$Lingua)

tab1 <- table(MYSLID$Lingua)
tab1
attributes(tab1)

# valori mancanti
table(MYSLID$Genere, useNA = "always")
table(MYSLID$Genere, useNA = "ifany")
table(MYSLID$Lingua, useNA = "ifany")

sum(is.na(MYSLID$Lingua))

# funzione personalizzata
nmiss(MYSLID$Lingua)
nval(MYSLID$Lingua)

# marginali - una variabile
sum(tab1)
margin.table(tab1)
addmargins(tab1)

# prop.table - una variabile
tab1 / sum(tab1)                # tab1 / totale
prop.table(tab1)

# frequenze percentuali
prop.table(tab1)*100

# arrotondamenti
round(prop.table(tab1)*100, digits = 2)   # due decimali
round(prop.table(tab1)*100, 1)            # un decimale

# funzione percent
percent(tab1)
percent(tab1, digits = 1)

# frequenze cumulate
tab2 <- table(MYSLID$Eta.classi)
cumsum(tab2)
cumsum(percent(tab2, 2))

# frequenze retrocumulate
cumsum(rev(tab2))
cumsum(percent(rev(tab2), 2))

# tabella di frequenza completa
# funzione tabfreq
tabfreq(MYSLID$Lingua)
tabfreq(MYSLID$Genere, digits = 1, totali = FALSE)

# funzione frequenze
frequenze(MYSLID, write = FALSE)
frequenze(MYSLID, file = "res/frequenze.csv")

# GRAFICI -----------------------------------------------------------------

# RCMDR grafici a barre
with(MYSLID, Barplot(Eta.classi, xlab="Eta.classi", ylab="Frequency",
                     main="Classi di Età"))

# RCMDR grafico a torta
library(colorspace, pos=16)
with(MYSLID, pie(table(Lingua), labels=levels(Lingua), xlab="", ylab="",
                 main="Lingua parlata", col=rainbow_hcl(3)))

# RCMDR istogramma
with(MYSLID, Hist(Retribuzione, scale="frequency", breaks="Sturges",
                  col="darkgray", main="Retribuzione"))


# PERSONALIZZARE I COLORI

# palette di colori in RCommander
palette(rainbow(8))

palette("default")

with(MYSLID, pie(table(Lingua), labels=levels(Lingua),
                 xlab="", ylab="",
                 main="Lingua parlata",
                 col=c(3,6,8)))                        # colore


# VALORI CARATTERISTICI ---------------------------------------------------

# RCMDR statistiche riassuntive
numSummary(MYSLID[,"Istruzione"], statistics=c("mean", "sd",
    "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))


# TENDENZA CENTRALE -------------------------------------------------------

# moda
table(MYSLID$Lingua)

# mediana
median(MYSLID$Eta)
median(MYSLID$Retribuzione)
median(MYSLID$Retribuzione, na.rm = TRUE)

cumsum(percent(tab2, 2))
# o
cumsum(percent(table(MYSLID$Eta.classi), 2))

median(MYSLID$Eta.classi, na.rm = T)
median(unclass(MYSLID$Eta.classi), na.rm = T)
median(unclass(MYSLID$Lingua), na.rm = T)

# funzione personalizzata med
med(MYSLID$Retribuzione)       # vettore con casi mancanti
med(MYSLID$Eta.classi)         # fattore ordinato
med(MYSLID$Lingua)             # fattore non ordinato

# media aritmetica
mean(MYSLID$Eta, na.rm=TRUE)
mean(MYSLID$Istruzione, na.rm = T)

sum(MYSLID$Istruzione, na.rm = T) / nval(MYSLID$Istruzione)

# scarti dalla media
scarti <- MYSLID$Eta - mean(MYSLID$Eta)
round(sum(scarti))


# VARIAZIONE --------------------------------------------------------------

# campo di variazione
range(MYSLID$Retribuzione, na.rm = T)
min(MYSLID$Retribuzione, na.rm = T)
max(MYSLID$Retribuzione, na.rm = T)

# quartili
quantile(MYSLID$Eta, na.rm = T)
quantile(MYSLID$Eta, na.rm = T, probs = c(0.25, 0.5, 0.75))

# decili
quantile(MYSLID$Eta, na.rm = T, probs = seq(0, 1, 0.10))

# quartili per fattori ordinati
quantile(unclass(MYSLID$Eta.classi), na.rm = T)

# differenza interquartile
IQR(MYSLID$Eta, na.rm=TRUE)

# devianza
sum(scarti)       # scarti
sum((scarti)^2)   # devianza

# varianza
mean(scarti^2)
var(MYSLID$Eta, na.rm=TRUE)

# funzione devianza()
var(MYSLID$Eta, na.rm=TRUE) * (nval(MYSLID$Eta) - 1)
devianza(MYSLID$Eta)

# deviazione standard
sd(MYSLID$Eta, na.rm=TRUE)
sqrt(var(MYSLID$Eta, na.rm=TRUE))

# coefficiente di variazione (calcolo)
# media
media <- mean(MYSLID$Retribuzione, na.rm = T)
# deviazione standard non corretta
SX <- sqrt(mean((MYSLID$Retribuzione - media)^2, na.rm = T))
# coeff. var.
SX / abs(media)
rm(media, SX)  # elimino gli oggetti


# FREQUENZE IN CLASSI PER VARIABILI CONTINUE ------------------------------

# RCMDR (da statistiche riassuntive)
binnedCounts(MYSLID[,"Eta", drop=FALSE])


# SUMMARY -----------------------------------------------------------------

summary(MYSLID$Genere)
summary(MYSLID$Retribuzione)
summary(MYSLID)


# DISTRIBUZIONE NORMALE ---------------------------------------------------

# frequenza relativa di un singolo valore

prop.table(table(MYSLID$Genere))

sum(MYSLID$Eta == 27)/nval(MYSLID$Eta)


# valori Z
mean(scale(MYSLID$Eta))
sd(scale(MYSLID$Eta))

# errore standard della media
sd(MYSLID$Eta, na.rm = T) / sqrt(nval(MYSLID$Eta))

## intervalli di confidenza

# quantili e probabilità
0.05 / 2        # area di errore / 2 (code)
qnorm(0.025)    # quantile


# ERRORE STANDARD DELLA MEDIA

# calcolare l'errore standard
SE <- sd(MYSLID$Eta, na.rm = T) / sqrt(nval(MYSLID$Eta))
# sottrarre SE * 1.96 dalla media
mean(MYSLID$Eta, na.rm = T) - (SE * 1.96)    # limite inferiore
[1] 43.58028
# sommare SE * 1.96 alla media
mean(MYSLID$Eta, na.rm = T) + (SE * 1.96)    # limite superiore
[1] 44.38524
# rimuovo SE
rm (SE)

# funzione personalizzata esm()
esm(MYSLID$Eta, digits = 3)
esm(MYSLID$Eta)["ES"]

qnorm(0.01 / 2)                     # trovare il quartile
esm(MYSLID$Eta, q = 2.58, digits = 3)
esm(MYSLID$Eta, qnorm(0.01/2), digits = 3)


# DISTRIBUZIONE T ---------------------------------------------------------

qt(0.05 / 2, df = 7424)
qt(0.05 / 2, df = 749)
qt(0.05 / 2, df = 74)
qt(0.05 / 2, df = 24)


# FORMA DELLA DISTRIBUZIONE -----------------------------------------------

# Istogramma di densità'
# Rcmdr
with(MYSLID, Hist(Eta, scale="density", breaks="Sturges",
                  col="darkgray",
                  xlab="", ylab="Densità", main="Età"))

# curva normale
curve(dnorm(x, mean = mean(MYSLID$Eta),
            sd = sd(MYSLID$Eta)),
      add = T)

# Rcmdr
with(MYSLID, Hist(Retribuzione, scale="density", breaks="Sturges",
                  col="darkgray",
                  xlab="", ylab="Densità", main="Retribuzione"))
# curva normale
curve(dnorm(x, mean = mean(MYSLID$Retribuzione, na.rm = TRUE),
            sd = sd(MYSLID$Retribuzione, na.rm = TRUE)),
      add = T)

# grafico di densità
# Rcmdr
densityPlot( ~ Eta, method="kernel", data=MYSLID,
             bw="SJ", adjust=1, kernel="gaussian", xlab="Età",
             ylab="Densità")
# curva normale
curve(dnorm(x, mean = mean(MYSLID$Eta),
            sd = sd(MYSLID$Eta)),
      lty = 2,                     # linea diversa
      add = T)

# Grafico a scatola e baffi
#Rcmdr
Boxplot( ~ Retribuzione, data=MYSLID, id.method="identify",
         main="Retribuzione")

# variabili standardizzate
boxplot(scale(MYSLID[2:4]))

# Grafico quantili-quantili
# Rcmdr
with(MYSLID, qqPlot(Retribuzione, dist="norm",
                    id=list(method="y", n=2,
                            labels=rownames(MYSLID))))

# Rcmdr
with(MYSLID, qqPlot(Eta, dist="norm",
                    id=list(method="y", n=2,
                            labels=rownames(MYSLID))))


# ASIMMETRIA --------------------------------------------------------------

# coefficiente di asimmetria
mm <- mean(MYSLID$Retribuzione, na.rm = T) -
  median(MYSLID$Retribuzione, na.rm = T)       # media - mediana

(3 * mm) / sd(MYSLID$Retribuzione, na.rm = T)  # coefficiente

rm(mm)

# skewness
mean(scarti^3)/sqrt((mean(scarti^2))^3)
# oppure
mean(scarti^3)/(mean(scarti^2))^(3/2)

skewness(MYSLID$Eta, type = 1)
skewness(MYSLID$Eta, type = 2) # RCommander, SAS, SPSS
skewness(MYSLID$Eta, type = 3) # default

# kurtosis
(mean(scarti^4)/(mean(scarti^2))^(2)) - 3

kurtosis(MYSLID$Eta, type = 1)
kurtosis(MYSLID$Eta, type = 2) # RCommander, SAS, SPSS
kurtosis(MYSLID$Eta, type = 3) # default



