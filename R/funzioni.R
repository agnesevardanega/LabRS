#' Casi validi
#'
#' Numero di casi validi in un oggetto.
#'
#' @param x oggetto (vettore, matrice, dataframe ...)
#' @return Numero di casi validi
#' @examples nval(cars)
#' nval(ChickWeight$Diet)
#' @export
nval <- function(x) {
  length(x[is.na(x)==FALSE])
}


#' Casi mancanti
#'
#' Numero di casi mancanti in un oggetto.
#'
#' @param x oggetto (vettore, matrice, dataframe ...)
#' @return Numero di casi mancanti
#' @examples nmiss(cars)
#' nmiss(ChickWeight$Diet)
#' @export
nmiss <- function(x) {
  length(x[is.na(x)==TRUE])
}


#' Percentuali
#'
#' Calcola le percentuali, con arrotondamento.
#'
#' @param x vettore, matrice, tabella
#' @param digits numero dei decimali (vedi \code{\link[base]{round}})
#' @param margin marginale (vedi \code{\link[base]{prop.table}})
#' @return valori percentuali
#' @examples percent(table(ChickWeight$Diet), 2)
#' percent(HairEyeColor, 1, margin = c(3,2))
#' @export
percent <- function(x, digits = 8, margin = NULL) {
  (round (prop.table(x, margin) * 100, digits))
}


#' Tabelle di frequenza
#'
#' Produce una tabella delle frequenze per la presentazione dei dati, 
#' in valori assoluti e percentuali (due decimali di default)
#'
#' @param x fattore
#' @param digits numero dei decimali (vedi \code{\link[base]{round}})
#' @param totali logico: TRUE = con totale e casi mancanti
#' @return una matrice con due colonne
#' @examples tabfreq(ChickWeight$Diet)
#' tabfreq(ChickWeight$Diet, totali = FALSE)
#' @export
tabfreq <- function (x, digits = 2, totali = TRUE) {
  if (!is.factor(x)) {
    return(NA)
  }
  tab <- table(x)
  tab.1 <- cbind("V.A." = tab, "%" = percent(tab, digits))
  tab.2 <- addmargins(tab.1, 1)
  rownames(tab.2) <- c(rownames(tab), "Totale")
  tab.2 <- rbind(tab.2, "NA" = c(sum(is.na(x)),
                                 round(sum(is.na(x)) / length(x), digits)))
  if (totali == FALSE){ print(tab.1) }
  else
    print(tab.2)
}


#' Tabelle di contingenza
#'
#' Produce una tabella di contingenza per la presentazione dei dati, 
#' in valori percentuali di colonna (un decimale di default), totali
#' di colonna in VA opzionali
#'
#' @param x fattore: variabile di riga
#' @param y fattore: variabile di colonna
#' @param digits numero dei decimali (vedi \code{\link[base]{round}})
#' @param totali logico: TRUE = con totali di colonna in VA
#' @return una matrice con la tabella
#' @export
tabcont <- function (x, y, digits = 1, totali = TRUE) {
  if (!is.factor(x) | !is.factor(y)) {
    return(NA)
  }
  tab <- addmargins(table(x, y))
  # col percent
  tab.1 <- cbind(percent(table(x, y), digits, 2), Totale = percent(table(x), digits))
  tab.2 <- rbind(tab.1, Totale = tab[nrow(tab),])
  if (totali == FALSE){ print(tab.1) }
  else
    print(tab.2)
}

#' Mediana
#'
#' Calcola la mediana anche per i fattori ordinati.
#' Per default, considera solo i casi validi.
#'
#' @param x vettore o fattore ordinato
#' @param na.rm valore logico (TRUE o FALSE) (vedi \code{\link[stats]{median}})
#' @param ... altri argomenti ereditati da \code{\link[stats]{median}}
#' @return mediana (valore o livello)
#' @examples med(Orange$Tree) # fattore ordinato
#' med(cars$speed) # vettore numerico
#' @export
med <- function (x, na.rm = TRUE, ...)
{
  if (is.factor(x)) {
    if (!is.ordered(x)) {
      warning("La mediana non si puo' calcolare!!!")
      return(NA)
    }
    me <- median(unclass(x), na.rm, ...)
    if (me - floor(me) != 0) {
      warning("Mediana indeterminata")
      return(NA)
    }
    else {
      levels(x)[me]
    }
  }
  else
    median(x, na.rm, ...)
}


#' Coefficiente di variazione
#'
#' Calcola il coefficiente di variazione.
#' Di default, considera solo i casi validi.
#'
#' @param x vettore
#' @return Valore del coefficiente di variazione
#' @export
cvar <- function (x)
{
  media <- mean(x, na.rm = T)
  SX <- sqrt(mean((x - media)^2, na.rm = T))
  SX / abs(media)
}


#' Errore standard (della media)
#'
#' Calcola l'errore standard dalla media, e gli estremi dell'intervallo di confidenza.
#' Di default, considera solo i casi validi e l'intervallo di confidenza è del 95\%.
#'
#' @param x vettore
#' @param vt qualunque valore della distribuzione t di Student:
#' default = 1.96 (int. conf 95\%); per un intervallo di confidenza
#' del 99\% impostarlo a 2.58.
#' @return Valore dell'errore standard, media, estremi dell'intervallo di confidenza, errore standard relativo (\%).
#' @examples esm(cars$speed)
#' @export
esm <- function (x, vt = 1.96)
{
  media <- mean(x, na.rm = T)
  es <- sqrt( var(x, na.rm = T) / nval(x))
  es.p <- es / media * 100
  res <- c(es, media, media - vt * es, media + vt * es, es.p)
  names(res) <- c("ES", "Media", "Inf", "Sup", "ES Rel")
  res
}


#' Coefficiente di asimmetria di Pearson
#'
#' Calcola il coefficiente di asimmetria di Pearson, basato sulla differenza
#' fra media e mediana (i casi mancanti sono eliminati dal computo):
#' 3 * ( mean - median ) / sd.
#' @param x vettore
#' @return Valore del coefficiente di asimmetria
#' @examples diffmm(cars$speed)
#' @export
diffmm <- function(x) {
  mm <- mean(x, na.rm = T) - median(x, na.rm = T)
  (3 * mm) / sd(x, na.rm = T)
}


#' Quantili
#'
#' Calcola i quantili anche per i fattori ordinati.
#' Per default, restituisce i soli quartili e considera solo i casi validi.
#'
#' @param x vettore o fattore ordinato
#' @param probs vettore numerico di probabilità con valori compresi fra 0 e 1
#' (vedi \code{\link[stats]{quantile}})
#' @param na.rm valore logico (TRUE o FALSE) (vedi \code{\link[stats]{quantile}})
#' @param ... altri argomenti ereditati da \code{\link[stats]{quantile}}
#' @return quantili (valori o livelli)
#' @examples quant(Orange$Tree) # fattore ordinato
#' quant(cars$speed) # vettore numerico
#' @export
quant <- function (x,
                   probs = c(0.25, 0.5, 0.75),
                   na.rm = TRUE,
                   ...)
{
  if (is.factor(x)) {
    if (!is.ordered(x)) {
      warning("I quartili non si possono calcolare!!!")
      return(NA)
    }
    qu <- quantile(unclass(x), probs, na.rm, ...)
    lev <- levels(x)[qu]
    names(lev) <- names(qu)
    lev
  }
  else
    quantile(x, probs, na.rm, ...)
}

#' Differenza interquartile
#'
#' Calcola la differenza interquartile anche per i fattori ordinati.
#' Per default, considera solo i casi validi.
#'
#' @param x vettore o fattore ordinato
#' @param na.rm valore logico (TRUE o FALSE) (vedi \code{\link[stats]{IQR}})
#' @param ... altri argomenti ereditati da \code{\link[stats]{IQR}}
#' @return quantili (valori o livelli)
#' @examples iqr(Orange$Tree) # fattore ordinato
#' iqr(cars$speed) # vettore numerico
#' @export
iqr <- function (x, na.rm = TRUE, ...)
{
  if (is.factor(x)) {
    if (!is.ordered(x)) {
      warning("I quartili non si possono calcolare!!!")
      return(NA)
    }
    IQR(unclass(x), na.rm, ...)
  }
  else
    IQR(x, na.rm, ...)
}


#' Esporta un dataset in txt tab delimited
#'
#' Esportazione semplificata di un dataset in formato txt tab delimited,
#' secondo lo standard Excel in lingua italiana:
#' separatore di decimali = virgola, NA = celle vuote. L'attributo
#' row.names = FALSE di default (il dataset da esportare ha una colonna
#' per gli ID), ma può essere modificato.
#' @param x dataset
#' @param file nome del file di esportazione (con estensione txt)
#' @param ... altri argomenti ereditati da \code{\link[utils]{write.table}}
#' @return file in formato txt
#' @export
expdata <- function(x, file, ...) {
  write.table(x, file,
              sep="\t",
              dec = ",",
              na = "",
              row.names = FALSE,
              ...
  )
}

#' Esporta una tabella in txt tab delimited
#'
#' Esportazione semplificata di una tabella in formato txt tab delimited,
#' secondo lo standard Excel in lingua italiana:
#' separatore di decimali = virgola, NA = celle vuote,
#' col.names = NA.
#' @param x oggetto table o xtabs, matrice
#' @param file nome del file di esportazione (con estensione txt)
#' @param ... altri argomenti ereditati da \code{\link[utils]{write.table}}
#' @return file in formato txt
#' @export
exptbl <- function(x, file, ...) {
  write.table(x, file,
              sep="\t",
              dec = ",",
              na = "",
              col.names = NA, ...
  )
}

#' Esporta una lista di tabelle in un file txt tab delimited
#'
#' Esportazione semplificata di più tabelle in formato txt tab delimited,
#' secondo lo standard Excel in lingua italiana, in uno stesso file:
#' separatore di decimali = virgola, NA = celle vuote,
#' col.names = NA.
#' @param x vettore carattere, o lista, con i nomi delle tabelle
#' @param file nome del file di esportazione (con estensione txt)
#' @param ... altri argomenti ereditati \code{\link[utils]{write.table}}
#' @return file in formato txt
#' @examples # tutte le tabelle sono esportate nel file tabelle.txt
#' exptbls(c("tab3", "tab6", "tab8"), file = "tabelle.txt")
#' exptbls(list("tab2", "tab4", "tab5"), file = "tabelle.txt")
#' exptbls("tab1", file = "tabelle.txt")
#' @export
exptbls <- function(x, file, ...) {
  tbs <- lapply(x, get)
  lapply(
    tbs,
    write.table,
    file,
    sep = "\t",
    dec = ",",
    na = "",
    col.names = NA,
    append = TRUE,
    ...
  )
}

#' Tabelle 'kable' con formato dei numeri in italiano
#'
#' Genera tabelle con standard dei numeri in lingua italiana, con la funzione
#' kable di knitr:
#' separatore di decimali = virgola, separatore di migliaia = punto
#' @param x oggetto table o xtabs, matrice, o dataframe
#' @param ... altri argomenti ereditati da \code{\link[knitr]{kable}}
#' @return vettore carattere del codice della tabella
#' @export
kabbit <- function(x, ...) {
  kable (x,
         format.args = list(decimal.mark = ",", big.mark = "."),
         ...)
}






