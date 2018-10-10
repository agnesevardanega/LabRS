# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# REPORT: descrizione

# pacchetti
library(Rcmdr)    # 2.4-4
library(LabRS)    # 0.1.0

# dati
data("MYSLID")

# Genere
with(MYSLID, Barplot(Genere, xlab="Genere", ylab="Frequency"))

local({
  .Table <- with(MYSLID, table(Genere))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})


# Lingua
with(MYSLID, Barplot(Lingua, xlab="Lingua", ylab="Frequency"))

local({
  .Table <- with(MYSLID, table(Lingua))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})


# Classi di Età
with(MYSLID, Barplot(Eta.classi, xlab="Eta.classi", 
                     ylab="Frequency"))

local({
  .Table <- with(MYSLID, table(Eta.classi))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})


# Età, Istruzione, Retribuzione: istogrammi
with(MYSLID, Hist(Eta, scale="frequency", breaks="Sturges", 
                  col="darkgray"))
with(MYSLID, Hist(Istruzione, scale="frequency", breaks="Sturges", 
                  col="darkgray"))
with(MYSLID, Hist(Retribuzione, scale="frequency", breaks="Sturges", 
                  col="darkgray"))


# Età, Istruzione, Retribuzione: statistiche riassuntive
library(abind, pos=19)
library(e1071, pos=20)
numSummary(MYSLID[,c("Eta", "Istruzione", "Retribuzione"), drop=FALSE], 
           statistics=c("mean", "sd", "IQR", "quantiles"), 
           quantiles=c(0,.25,.5,.75,1))
