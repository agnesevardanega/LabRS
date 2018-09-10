# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# CAPITOLO 2
# ==========
# le righe precedute dal commento "RCMDR: ..." sono prodotte da menu di Rcmdr
# e non sono eseguibili se non è caricato Rcmdr


# INIZIO SESSIONE ---------------------------------------------------------

# avviare RCommander
library(Rcmdr)

# Da Menu di RCommander o da R: Impostare la directory di lavoro
# setwd("C:/.../Laboratorio R")


# IMPORTAZIONE DEI DATI ---------------------------------------------------

# RCMDR: importare MYSLID
MYSLID <-
  readXL("C:/.../Laboratorio R/MYSLID.xlsx",
   rownames=FALSE, header=TRUE, na="NA", sheet="Foglio1",
   stringsAsFactors=TRUE)

# controllare lo spazio di lavoro
ls()

# controllare il dataset
names(MYSLID)
str(MYSLID)

# controllare le variabili
str(MYSLID$wages)
str(MYSLID$language)

# visualizzare il dataset
View(MYSLID)

# carica un dataset di esempio
data(ChickWeight)
data(Animals, package = "MASS")
help("ChickWeight")

rm(ChickWeight, Animals)


# INFORMAZIONI SUGLI OGGETTI ----------------------------------------------

# informazioni sul dataset
names(MYSLID)
str(MYSLID)

# informazioni sulle variabili
str(MYSLID$wages)
str(MYSLID$language)


# RICODIFICA e trasformazioni delle variabili -----------------------------

# RCMDR: rinominare le variabili
names(MYSLID)[c(5)] <- c("Genere")
names(MYSLID)[c(4,3,6,2)] <- c("Eta","Istruzione","Lingua",
                               "Retribuzione")

# controllare il risultato
names(MYSLID)

# RCMDR: ricodificare una variabile categoriale
MYSLID <- within(MYSLID, {
 Genere <- Recode(Genere, '"Female" = "Donna"; "Male" = "Uomo"',
 as.factor=TRUE)
})

# cambiare a mano le righe di comando
MYSLID <- within(MYSLID, {
  Lingua <- Recode(
    Lingua,
    '"English" = "Inglese";
    "French" = "Francese"; "Other" = "Altro"',
    as.factor = TRUE
  )
})

# controllare il risultato
levels(MYSLID$Genere)
levels(MYSLID$Lingua)

# RCMDR: riordinare i livelli di un fattore
MYSLID$Lingua <- with(MYSLID, factor(Lingua,
                     levels=c('Inglese','Francese','Altro')))

# controllare il risultato
levels(MYSLID$Lingua)

# RCMDR: ricodificare in una nuova variabile
MYSLID <- within(MYSLID, {
  Eta.classi <- Recode(Eta,
                       '16:25 = "Fino a 25"; 26:35 = "26-35";
                       36:45 = "36-45"; 46:55 = "46-55";
                       56:65 = "56-65"; 66:95 = "Oltre i 65"',
                       as.factor=TRUE)
})

# controllare il risultato
levels(MYSLID$Eta.classi)

# RCMDR: convertire a fattore ordinato
MYSLID$Eta.classi <- with(MYSLID, factor(Eta.classi,
                                         levels=c('Fino a 25', '26-35',
                                                  '36-45', '46-55',
                                                  '56-65','Oltre i 65'),
                                         ordered=TRUE))

# controllare il risultato
class(MYSLID$Eta.classi)

# salvare il file di dati
save(MYSLID, file = "data/MYSLID.rda")


# STRUTTURE DI DATI -------------------------------------------------------

# vettori numerici
var.num <- c(1, 0, 9, 2, 5, 3, 10)
ls()
str(var.num)
var.num[4]
var.num[var.num > 3]
var.num[var.num == 5]

# vettori carattere
sette.nani <- c("Dotto", "Mammolo", "Pisolo", "Brontolo",
                "Gongolo", "Eolo", "Cucciolo")
str(sette.nani)
sette.nani[6]
sette.nani[sette.nani != "Eolo"]

# vettori logici
sette.nani == "Eolo"

# liste
lista <- list("baci" = var.num, "sette.nani" = sette.nani,
              "somma" = 3+5, "pippo" = "pippo")
str(lista)

lista[3]
lista[1]

str(lista[3])       # controlliamo la struttura

lista[[1]]
str(lista[[1]])

lista$somma         # equivale a lista[[3]]
lista[["somma"]]    # equivale a lista[[3]]
lista["somma"]      # equivale a lista[3]

lista$baci[3]
lista[["baci"]][3]
lista$sette.nani[3]
lista$sette.nani[sette.nani == "Mammolo"]

# dataframe
df <- data.frame("sette.nani" = sette.nani, "baci" = var.num)
df
str(df)

df[1,]                    # prima riga (caso)
df[,2]                    # seconda colonna (variabile)
df[1,2]                   # prima riga e seconda colonna

df[1]             # colonna del df (sottoinsieme)
df[[1]]           # vettore, corrisponde a df[,1]
df$sette.nani

df$baci[3]

# righe e colonne
attributes(lista)
attributes(df)
names(df)
row.names(df)
colnames(df)     # come names
rownames(df)     # come row.names

rm(var.num, sette.nani, lista, df)


# FINE SESSIONE -----------------------------------------------------------

# (consigliabile) salvare il file dei comandi e dei risultati
# chiudendo RCommander
