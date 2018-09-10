# SCRIPT TRASFORMAZIONE DEI DATI

# pacchetti
library(Rcmdr)

# importazione dei dati
MYSLID <-
  readXL("data/MYSLID.xlsx",
         rownames=FALSE, header=TRUE, na="NA", sheet="Foglio1",
         stringsAsFactors=TRUE)

# rinominare le variabili
names(MYSLID)[c(5)] <- c("Genere")
names(MYSLID)[c(4,3,6,2)] <- c("Eta","Istruzione","Lingua",
                               "Retribuzione")

# ricodificare Genere
MYSLID <- within(MYSLID, {
  Genere <- Recode(Genere, '"Female" = "Donna"; "Male" = "Uomo"',
                   as.factor=TRUE)
})

# ricodificare Lingua
MYSLID <- within(MYSLID, {
  Lingua <- Recode(
    Lingua,
    '"English" = "Inglese";
    "French" = "Francese"; "Other" = "Altro"',
    as.factor = TRUE
  )
})

# riordinare i livelli di Lingua
MYSLID$Lingua <- with(MYSLID, factor(Lingua,
                                     levels=c('Inglese','Francese','Altro')))

# Nuova variabile "Eta.classi"
MYSLID <- within(MYSLID, {
  Eta.classi <- Recode(Eta,
                       '16:25 = "Fino a 25"; 26:35 = "26-35";
                       36:45 = "36-45"; 46:55 = "46-55";
                       56:65 = "56-65"; 66:95 = "Oltre i 65"',
                       as.factor=TRUE)
})

# "Eta.classi" come fattore ordinato
MYSLID$Eta.classi <- with(MYSLID, factor(Eta.classi,
                                         levels=c('Fino a 25', '26-35',
                                                  '36-45', '46-55',
                                                  '56-65','Oltre i 65'),
                                         ordered=TRUE))

# salvare il file di dati
save(MYSLID, file = "data/MYSLID.rda")

