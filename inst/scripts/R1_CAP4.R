# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# CAPITOLO 4
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
load(".RData")


# Tabelle di contingenza --------------------------------------------------

# RCMDR Tabella di contingenza
local({
  .Table <- xtabs(~Lingua+Genere, data=MYSLID)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

# table
table(MYSLID$Lingua, MYSLID$Genere)

# percentuali
tab3 <- table(MYSLID$Lingua, MYSLID$Genere)
percent(tab3, 1, margin = 2)

# funzione tabcont()
tabcont(MYSLID$Lingua, MYSLID$Genere)


# Grafici a barre bivariati -----------------------------------------------

# RCMDR grafico a barre sovrapposte
with(MYSLID, Barplot(Genere, by=Lingua, style="divided",
  legend.pos="above", xlab="Genere", ylab="Frequency"))

# RCMDR grafico a barre affiancate
with(MYSLID, Barplot(Genere, by=Lingua, style="parallel",
  legend.pos="above", xlab="Genere", ylab="Frequency"))


# Test del chi quadrato ---------------------------------------------------

# primo esempio
chisq.test(tab3)
summary(tab3)

# secondo esempio
data("tabM")
tabM
chi.res <- chisq.test(tabM)
chi.res
str(chi.res)
chi.res$expected
chi.res$residuals
chi.res$residuals^2
sum(chi.res$residuals^2)

# distribuzione del chi quadrato
pchisq(30.07015, df = 2)       # area tratteggiata
1 - pchisq(30.07015, df = 2)   # area alfa

# dimensioni del campione
chisq.test(tab3/3)
0.24422/3

# gradi di libertà
1- pchisq(8, df = 3)
1 - pchisq(8, df = 9)
1 - pchisq(8, df = 15)


# CONFRONTI FRA MEDIE -----------------------------------------------------

# Rcmdr tabella delle statistiche
numSummary(MYSLID[,"Retribuzione", drop=FALSE],
           groups=MYSLID$Genere,
           statistics=c("mean", "sd"), quantiles=c(0,.25,.5,.75,1))

# Rcmdr boxplot bivariato
Boxplot(Retribuzione~Genere, data=MYSLID, id=list(method="y"))

# TEST DELLE MEDIE
t.test(Retribuzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID)
t.test(Retribuzione~Genere, alternative='less',
       conf.level=.95, var.equal=FALSE, data=MYSLID)

# controllo delle altre variabili
t.test(Eta~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID)
t.test(Istruzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID)

# ESERCIZIO: controllo per <66 anni
t.test(Retribuzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID[MYSLID$Eta<66,])
t.test(Eta~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID[MYSLID$Eta<66,])
t.test(Istruzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID[MYSLID$Eta<66,])


# ANOVA -------------------------------------------------------------------

# tabella delle statistiche
numSummary(MYSLID[,"Retribuzione", drop=FALSE],
           groups=MYSLID$Eta.classi, statistics=c("mean", "sd"))

# subset
MYSLID.2 <- subset(MYSLID, subset=MYSLID$Eta<66)
table(MYSLID.2$Eta.classi)   # controlliamo il risultato

# Rcmdr
AOV.res <- aov(Retribuzione ~ Eta.classi, data=MYSLID.2)
summary(AOV.res)
with(MYSLID.2, numSummary(Retribuzione, groups=Eta.classi,
                          statistics=c("mean", "sd")))
oneway.test(Retribuzione ~ Eta.classi, data=MYSLID.2) # Welch test

# F
10194 / 51

# varianze
40777 / 4
180232 / 3521
5
# gradi di libertà
6309 - 2783 - 5

# devianza
devianza(MYSLID.2$Retribuzione)
40777 + 180232

# devianza esterna
sum(
  ((9.341336 - 15.56)^2 * 726),
  ((15.300589 - 15.56)^2 * 967),
  ((18.067923 - 15.56)^2 * 915),
  ((18.606244 - 15.56)^2 * 639),
  ((17.406882 - 15.56)^2 * 279)
)

# devianza interna
sum(
  devianza(MYSLID.2[MYSLID.2$Eta.classi == "Fino a 25",
                    "Retribuzione"]),
  devianza(MYSLID.2[MYSLID.2$Eta.classi == "26-35", "Retribuzione"]),
  devianza(MYSLID.2[MYSLID.2$Eta.classi == "36-45", "Retribuzione"]),
  devianza(MYSLID.2[MYSLID.2$Eta.classi == "46-55", "Retribuzione"]),
  devianza(MYSLID.2[MYSLID.2$Eta.classi == "56-65", "Retribuzione"])
)


# LA CORRELAZIONE ---------------------------------------------------------

data(cars)

# grafico a dispersione
scatterplot(dist~speed, regLine=FALSE, smooth=FALSE,
            boxplots=FALSE, data=cars)

plot(cars)

# la covarianza
cov(cars$speed, cars$dist)
cov(cars$speed, cars$dist) /(sd(cars$speed) * sd(cars$dist))
cov(scale(cars$speed), scale(cars$dist))

# coefficiente di correlazione
cor(cars$speed, cars$dist)

# matrice di correlazione
cor(cars)

# RCMDR matrice di correlazione
cor(cars[,c("dist","speed")], use="complete")

# casi con valori mancanti
cor(MYSLID[,c("Eta", "Istruzione", "Retribuzione")])
# o
# cor(MYSLID[2:4])

cor(MYSLID[,c("Eta", "Istruzione", "Retribuzione")],
    use = "complete")

# test di correlazione
cor.test (cars$dist, cars$speed)
cor.test(MYSLID$Retribuzione, MYSLID$Istruzione)


# ANALISI DI REGRESSIONE --------------------------------------------------

# RCMDR scatterplot con retta di regressione
scatterplot(dist~speed, regLine=TRUE, smooth=FALSE, boxplots=FALSE,
            data=cars)

# RCMDR regressione lineare
LM.res <- lm(dist~speed, data=cars)
summary(LM.res)

# errore standard dei residui
sqrt(sum(LM.res$residuals^2)/48)


# R^2
DT.Y <- devianza(cars$dist)            # devianza totale
DR <- sum(LM.res$residuals^2)          # dev. non spiegata
DM <- DT.Y - DR                        # dev. spiegata
R2 <- DM / DT.Y                        # R^2

R2 / (1 - R2) * 48                 # F
1 - pf(89.57, 1, 48)
rm(DT.Y, DR, DM, R2)


# coefficiente di regressione
# b = cov / var
cov(cars$speed, cars$dist) / var(cars$speed)
cov(cars$speed, cars$dist) / var(cars$dist)
# e coefficiente di regressione
3.932409 * 0.1655676
# e coefficiente di correlazione
sqrt(3.932409 * 0.1655676)


# ANALISI DELLE CORRISPONDENZE --------------------------------------------

# plugin di RCommander
install.packages("RcmdrPlugin.FactoMineR")

data("faccende")
faccende

# percentuali di riga
rbind(
  percent(faccende, margin = 1, digits = 1),
  TOTALE = percent(margin.table(faccende, 2), 1))


# CA
# Rcmdr
faccende.CA<-faccende[c("Bucato", "Pasto_princ", "Cena",
                        "Colazione", "Rassettare", "Piatti",
                        "Shopping", "Documenti", "Guidare",
                        "Finanze", "Assicurazioni",
                        "Riparazioni", "Ferie"),
                      c("Moglie", "Alternati", "Marito",
                        "Insieme")]
CA.res<-CA(faccende.CA, ncp=5, row.sup=NULL, col.sup=NULL,
           graph = FALSE)
ellipseCA(CA.res, ellipse=c(""), axes=c(1, 2), col.row="red",
          col.col="blue", label=c("col", "col.sup", "row",
                                  "row.sup"))

summary(CA.res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")
write.infile(CA.res$eig, file ="res/CA.csv",append=FALSE)
write.infile(CA.res$col, file ="res/CA.csv",append=TRUE)
write.infile(CA.res$row, file ="res/CA.csv",append=TRUE)
write.infile(dimdesc(CA.res, axes=1:5), file ="res/CA.csv",append=TRUE)
remove(faccende.CA)

# CA.res<-CA(faccende)

CA.res$eig
dimdesc(CA.res, axes=1:2)

