# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# CAPITOLO 5

# le righe precedute dal commento "RCMDR: ..." sono prodotte da menu di Rcmdr
# e non sono eseguibili se non è caricato Rcmdr


# INIZIO SESSIONE ---------------------------------------------------------

# Da Menu di RCommander o da R: Impostare la directory di lavoro
# setwd("C:/.../Laboratorio R")

# libreria del laboratorio
library(LabRS)

# Dati
data("MYSLID")


# ASSI --------------------------------------------------------------------

# proporzioni

# variabile categoriale
barplot(tab1)

barplot(tab1,
        ylim = c(0, max(tab1)+1000))          # limiti asse y

barplot(tab1,
        ylim = c(0, max(tab1)+1000),
        xlim = c(0,6))                        # limiti asse x

# variabile numerica
plot(cars$speed, cars$dist)                   # default
abline(lm(data = cars, dist ~ speed))

plot(cars$speed, cars$dist,
     asp = 1)                                 # 1:1
abline(lm(data = cars, dist ~ speed))


# limiti e bordo

plot(cars,
     xlim = c(0, max(cars$speed) / 100 * 110),
     ylim = c(0, max(cars$dist) / 100 * 110),
     bty = "l")                               # bordo a L


# LAYERS O LIVELLI --------------------------------------------------------

# primo livello
plot(cars)

# griglia
grid()

# oppure
plot(cars,
     panel.first = grid())                    # disegnata sotto

# terzo livello
abline(lm(data = cars, dist ~ speed))

# quarto livello
title(main = "Retta dei minimi quadrati")     # aggiunge il titolo

# oppure
plot(cars,
     main = "Retta dei minimi quadrati")      # titolo nel testo
grid()
abline(lm(data = cars, dist ~ speed))

title(xlab = "Velocità",
      ylab = "Spazio di frenata")

plot(data = cars, dist ~ speed,
     xlab = "Velocità",
     ylab = "Spazio di frenata")


# GRAFICI ESPLORATIVI -----------------------------------------------------

# variabili categoriali: Figura 1
plot(MYSLID$Eta.classi)

coordx <- plot(MYSLID$Eta.classi)
coordx

# due variabili categoriali (mosaico)
plot(MYSLID$Lingua, MYSLID$Genere)

plot(MYSLID$Genere, MYSLID$Lingua)


# GRAFICI A BARRE ---------------------------------------------------------

barplot(table(MYSLID$Eta.classi),            # Fig. 1
        main = "Classi di età")              # titolo

attributes(table(MYSLID$Eta.classi))

barplot(tab3,                                # Fig. 3
        xlab = "Lingua",                     # titolo asse x
        ylab = "Frequency")                  # titolo asse y

barplot(tab3,                               # Fig. 14
        beside = TRUE,                      # barre affiancate
        main = "Lingue parlate e genere",
        xlab = "Genere",
        ylab = "Frequency",
        legend = levels(MYSLID$Lingua))     # legenda

barplot(tabM,                               # Fig. 15
        main = "Identificazione di partito e genere",
        beside = TRUE,
        xlab = "Genere",
        ylab = "Frequency",
        legend = row.names(tabM))          # legenda

barplot(tab2,
        horiz = T)                          # orizzontali

# etichette
barplot(tab1,
        names.arg = c("Anglofoni", "Francofoni", "Altro"),
        xlab = "Gruppo linguistico")


# GRAFICI A TORTA ---------------------------------------------------------

pie(table(MYSLID$Lingua),                   # Fig. 2
    main = "Lingua parlata")

pie(tab1,
    main = "Lingua parlata")

# etichette
names(tab1)

pie(tab1,
    labels = c("Anglofoni", "Francofoni", "Altro"))

pie(tab1,
    labels = percent(tab1, 1))              # etichette percentuali

pie(tab1,
    clockwise = TRUE,
    labels = paste(names(tab1), percent(tab1, 1), "%"))


# ISTOGRAMMI --------------------------------------------------------------

hist(MYSLID$Retribuzione,                    # Fig. 3
     col = "darkgray",                       # colore
     main = "Retribuzione",
     xlab = NULL,                            # senza titolo x
     ylab = NULL)                            # senza titolo y

hist(MYSLID$Eta,                             # Fig. 6
     freq = FALSE,
     main = "Età",
     xlab = NULL,
     ylab = "Densità")

hist(MYSLID$Retribuzione,                    # Fig. 7
     prob = TRUE,
     main = "Retribuzione",
     xlab = NULL,
     ylab = "Densità")

# etichette
hist(MYSLID$Eta,
     ylim = c(0, 900),                      # limiti dell'asse y
     labels = TRUE)


# BOXPLOT -----------------------------------------------------------------

boxplot(MYSLID$Retribuzione)                  # Fig. 9

# confronto distribuzioni
boxplot(scale(MYSLID[2:4]))                   # Fig. 10

# retta della media
abline(h = 0,                                 # media = 0
       lty = 2)                               # linea tratteggiata

# due variabili
boxplot(MYSLID$Retribuzione ~ MYSLID$Genere)  # Fig. 17

boxplot(data = MYSLID, Retribuzione ~ Genere)

# etichette
boxplot(data = MYSLID, Retribuzione ~ Genere,
        names = c("Donne", "Uomini"))


# GRAFICI A DISPERSIONE ---------------------------------------------------

# grafici a dispersione: simboli
plot(cars,
     pch = "*")

plot(cars,
     pch = 19,
     col = "blue")

plot(cars,
     pch = 22,
     col = "blue",                            # bordo
     bg = "pink")                             # riempimento

# usare i valori di una variabile
plot(data = MYSLID, Retribuzione ~ Istruzione,
     pch = as.numeric(MYSLID$Genere))


# LINEE -------------------------------------------------------------------

# abline
plot(data = cars, dist ~ speed)               # Fig. 20
abline(lm(data = cars, dist ~ speed))         # retta

# retta orizzontale (media)
boxplot(scale(MYSLID[2:4]))                   # Fig. 10
abline(h = 0,                                 # media = 0
       lty = 2)                               # linea tratteggiata

# retta verticale (mediana)
hist(MYSLID$Eta, prob = TRUE)                 # Fig. 6
abline(v = median(MYSLID$Eta),                # mediana
       lty = 2)                               # linea

# segments
hist(MYSLID$Eta)

segments(median(MYSLID$Eta), -30,             # punto di inizio (x, y)
         median(MYSLID$Eta), 850,             # punto di fine (x, y)
         lwd = 2)                             # spessore della linea

# etichetta
text(x = median(MYSLID$Eta) + 8,
     y = 850,
     "mediana")


# CURVA NORMALE -----------------------------------------------------------

# distribuzione normale                       # Fig. 4
curve(dnorm(x,
            mean = 0,                         # media
            sd = 1),                          # sd
      -3, 3                                   # limiti
)
grid(ny = 0)                                  # griglia verticale


# curva normale aggiunta
hist(MYSLID$Eta,                              # Fig. 6
     prob = TRUE
)
curve(dnorm(x,
            mean = mean(MYSLID$Eta),
            sd = sd(MYSLID$Eta)),
      add = TRUE)


# curva normale aggiunta a variabile standardizzata
hist(scale(MYSLID$Eta),                       # Fig. 6
     prob = TRUE
)
curve(dnorm(x),
      add = TRUE,
      lty = 3,                                # linea a punti
      lwd = 2)                                # linea più spessa


# ALTRI GRAFICI -----------------------------------------------------------

# grafico di densità
plot(density(MYSLID$Eta))                     # Fig. 8


# grafici quantili-quantili
qqnorm(MYSLID$Retribuzione)                   # Fig. 11
qqnorm(MYSLID$Eta)                            # Fig. 11

qqline(MYSLID$Eta)                            # linea norm.


# grafici del pacchetto RcmdrMisc
RcmdrMisc::Barplot(MYSLID$Eta.classi)         # Fig. 1
RcmdrMisc::Hist(MYSLID$Retribuzione)          # Fig. 3

# grafici del pacchetto car
car::densityPlot(MYSLID$Eta)                  # Fig. 8
car::Boxplot(MYSLID$Retribuzione)             # Fig. 9
car::qqPlot(MYSLID$Retribuzione)              # Fig. 11
car::scatterplot(data = cars, dist ~ speed)   # Fig. 18


# COLORI E RIEMPIMENTI ----------------------------------------------------


# palette

palette()
palette(rainbow(10))
palette()
palette(c("red", "green", "green4", "blue", "cyan"))
palette("default")

# vedere i colori di una paletta
n = 8
barplot(rep(1, n), col = rainbow(n))

palette("default")


# indicare i colori

barplot(tab1,
        col = "pink",
        border = "blue")


# RAPPRESENTARE I DATI

# mosaico
data(faccende)

mosaicplot(
  faccende[,c(1,2,4,3)],
  type = "pearson",
  las = 2,                                    # orientamento testo assi
  col = c("magenta", "gray86", "#E6E6E6", "blue"),
  main = "Faccende"                           # titolo
)

# grafico a dispersione
library(colorspace)
palette(rev(sequential_hcl(5)))

plot(cars,
     pch = 21,
     col = 3,
     bg = cars$speed/max(cars$speed)*5)     # variabile trasformata

palette("default")                          # paletta di default

# terza variabile
palette(cm.colors(6))

plot(data = MYSLID, Retribuzione ~ Istruzione,
     pch = 19,
     col = MYSLID$Eta.classi)

palette("default")


# RIEMPIMENTI

pie(tab1,
    clockwise = TRUE,
    density = c(0, 5, 10),
    angle = c(0, 180, 45))


# ETICHETTE DEI DATI ------------------------------------------------------

# scatterplot
plot(cars)

text(x = cars$speed, y = cars$dist,   # coordinate
     labels = row.names(cars),        # etichette
     cex = 0.6,                       # dimensioni del testo
     pos = 2)                         # posizione

# oppure con punti piccoli
plot(cars,
     pch = ".")

text(x = cars$speed, y = cars$dist,
     labels = row.names(cars),
     cex = 0.6
)


# grafico CA
coord <- rbind(
  CA.res$row$coord,
  CA.res$col$coord)

plot(coord[,1:2],
     ylim = c(-1.5, 1),
     asp = 1)
abline(v = 0,
       lty = 2)                       # linea tratteggiata
abline(h = 0,
       lty = 2)
text(coord[1:13,1:2],
     labels = row.names(coord)[1:13],
     cex = 0.7,                       # dimensione carattere
     col = "blue",
     pos = 3)                         # posizione etichetta
text(coord[14:17,1:2],
     labels = row.names(coord)[14:17],
     cex = 0.7,
     col = "red",
     pos = 3)
rm(coord)


# grafico a barre
coordx <- barplot(tab3,
                  beside = TRUE,
                  ylim = c(0, 3200),      # limiti dell'asse Y
                  main = "Genere e gruppo linguistico")

labs <- percent(tab3, 1, 2)                # percentuali

text(x = coordx,
     y = tab3,
     labels = paste(labs, "%", sep = ""),  # aggiunge %
     pos = 3,                              # sopra
     cex = 0.8,
     font = 2)                             # grassetto

text(x = coordx,
     y = tab3,
     labels = tab3,                        # valore assoluto
     pos = 3,
     cex = 0.8,
     font = 2
)

rm(coordx, labs)

# istogramma
coordx <- hist(MYSLID$Eta,
               ylim = c(0, 900),
               main = NULL)       # senza titolo

str(coordx)

text(x = coordx$mids,             # midpoints
     y = coordx$counts - 20,      # frequenze, etichette all'interno
     labels = coordx$counts,
     cex = 0.8)

rm(coordx)


# LEGENDA -----------------------------------------------------------------

# Figura 15
data("tabM")
barplot(tabM,
        main = "Identificazione di partito e genere",
        beside = TRUE,
        legend = rownames(tabM))              # argomento della funzione

# Figura 16
# linee
curve(dchisq(x, df = 3),
      0, 20)
grid(ny = 0)
curve(dchisq(x, df = 9),
      0, 20,
      add = T,
      lwd = 2,
      lty = 2)
curve(dchisq(x, df = 15),
      0, 20,
      add = T,
      lwd = 2,
      lty = 3)
legend("topright",
       title = "Gradi di libertà",
       legend = c("3","9","15"),              # testo
       lty = c(1, 2, 3),                      # linee
       lwd = 2,                               # spessore linee
       cex = 0.9,                             # dimensione testo
       bty = "n"                              # senza box
       )


# Figura 13 e Figura 14
# colori
par(xpd = NA)                                 # usare i margini
barplot(tab3,
        xlab = "Genere",
        ylab = "Frequenze"
)

legend(0, 4500,                               # coordinate
       legend = rownames(tab3),               # testo
       fill = gray.colors(3),                 # colori
       horiz = TRUE,                          # orizzontale
       title = "Lingua"                       # titolo
)

par(xpd = FALSE)


# legenda per grafico con punti
par(xpd = NA)
palette(cm.colors(6))
plot(MYSLID$Istruzione, MYSLID$Retribuzione,
     xlab = "Istruzione",
     ylab = "Retribuzione",
     pch = 19,
     col = MYSLID$Eta.classi)

legend(-1, 62,
       legend = levels(MYSLID$Eta.classi),    # testo della legenda
       fill = cm.colors(6),                   # bordo dei punti
       horiz = TRUE,
       cex = 0.8,                             # dimensione carattere
       bty = "n",                             # senza box
       title = "Classi di età")
par(xpd = FALSE)


# PARAMETRI GRAFICI -------------------------------------------------------

par(mar = c(1, 0, 1, 0))          # margini del grafico in linee
par(bty = "l")                    # bordo del grafico a L

# dimensioni del carattere ridotte, bordo a L
par(list(cex = 0.8, bty = "l"))

# inserire elementi nei margini
par(xpd = NA)

par()

# salvare una copia dei default
def.par <- par()

# ripristinare il default
par(def.par)

