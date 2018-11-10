# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# CAPITOLO 6

# le righe precedute dal commento "RCMDR: ..." sono prodotte da menu di Rcmdr
# e non sono eseguibili se non è caricato Rcmdr


# INIZIO SESSIONE ---------------------------------------------------------

# libreria del laboratorio
library(LabRS)

# Da Menu di RCommander o da R: Impostare la directory di lavoro
# setwd("C:/.../Laboratorio R")


# OGGETTI DELL'AREA DI LAVORO ---------------------------------------------
# (salvati in sottocartella "dati")

# dataset
save("MYSLID", file="dati/MYSLID.rda")

# oggetto
save(tabM, file = "dati/tabAgresti.rds")

load("dati/tabAgresti.rds")


# DATASET -----------------------------------------------------------------

# csv standard italiano (sep = ";", dec = ",")
write.table(MYSLID, "dati/MYSLID.csv",
            sep=";",               # punto e virgola
            row.names = FALSE,
            dec = ",",
            na = "")

# oppure
write.csv2(MYSLID, "dati/MYSLID2.csv",
           row.names = FALSE,
           na = "")

# funzione expdata
expdata(MYSLID, "MYSLID.csv")

# spss
library(foreign)
write.foreign(MYSLID, "dati/MYSLID.txt",
              "dati/MYSLID.sps", package = "SPSS")

# sas
write.foreign(MYSLID, "dati/MYSLID.txt",
              "dati/MYSLID.sas", package="SAS")


# TABELLE -----------------------------------------------------------------

# csv (standard Italia)
write.table(tabcont(MYSLID$Lingua, MYSLID$Genere),
            file = "res/tabella.csv",
            sep = ";",
            dec = ",",
            col.names = NA)

# oggetto di un modello
write.table(chi.res$residuals,
            file = "res/chi_residuals.csv",
            sep = ";",
            dec = ",",
            col.names = NA)

# PIU' TABELLE IN UN FILE
write.table(tab2,
            "res/tabella.csv",   # stesso file
            sep = ";",
            dec = ",",
            col.names = NA,
            append = TRUE)       # append = TRUE

# funzione exptab

# una tabella
exptab(list("tabella 1" = tab1),       # lista con nome
       file = "res/tabella.csv")

exptab(list(chi.res$expected),
       dids = c("frequenze attese"),   # didascalia
       file = "res/tabella.csv",
       aggiungi = TRUE)                # file precedente

# tabelle diverse
exptab(list("tabella 1" = tab1, "tabella 2" = tab2),  # lista con nomi
       file = "res/tabelle.csv",
       aggiungi = FALSE)                              # nuovo file

exptab(list(tab3, chi.res$residuals),                 # lista
       file = "res/tabelle.csv",
       dids = c("tabella 3", "residui"),              # vettore con didascalie
       aggiungi = TRUE)                               # nuovo file

# distribuzioni di frequenza di un dataset in csv

# funzione 'frequenze'
# un dataframe
frequenze(MYSLID, file = "res/frequenze.csv")

# una variabile
frequenze(MYSLID[6], file = "res/frequenze.csv",
          aggiungi = TRUE)

# alcune variabili
frequenze(MYSLID[c(5,7)], file = "res/frequenze.csv")


# GRAFICI -----------------------------------------------------------------

# funzione expchart()

expchart(file = "res/chart1.png")

expchart(png,                      # tipo di file
         file = "res/chart2.png",  # nome del file
         ppi = 300,                # risoluzione
         hprop = 4,                # altezza 4 * ppi
         wprop = 4)                # larghezza 4 * ppi

# R MARKDOWN --------------------------------------------------------------

# per il PDF / LaTex
install.packages("tinytex")
install_tinytex()

# rendering da console
rmarkdown::render(choose.files(),
                  output_format = "odt_document")

rmarkdown::render(choose.files())

# FINE SESSIONE -----------------------------------------------------------

# salvare il file dei comandi e dei risultati chiudendo RCommander
# salvare l'area di lavoro chiudendo R
