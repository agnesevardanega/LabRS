#' Tabelle di contingenza
#'
#' Produce una tabella di contingenza per la presentazione dei dati,
#' in valori percentuali di colonna (un decimale di default), totali
#' di colonna in VA opzionali
#'
#' @param x fattore: variabile di riga
#' @param y fattore: variabile di colonna
#' @param digits numero dei decimali (vedi \code{\link[base]{round}})
#' @param margin margin marginale (vedi \code{\link[base]{prop.table}})
#' @param totali logico: TRUE = con totali di colonna in VA
#' @return una matrice con la tabella
#' @examples data("MYSLID")
#' tabcont(MYSLID$Lingua, MYSLID$Genere)
#' tabcont(MYSLID$Lingua, MYSLID$Genere, totali = FALSE)
#' @export
tabcont <- function (x, y, digits = 1, margin = 2, totali = TRUE) {
  if (!is.factor(x) | !is.factor(y)) {
    return(NA)
  }
  
  tab <- addmargins(table(x, y))
  
  if (margin == 1) {
    # row percent
    tab.1 <- rbind(percent(table(x, y), digits, 1),
                   Totale = percent(table(y), digits))
    tab.2 <- cbind(tab.1, Totale = tab[, ncol(tab)])
  } else {
    # col percent
    tab.1 <- cbind(percent(table(x, y), digits, 2),
                   Totale = percent(table(x), digits))
    tab.2 <- rbind(tab.1, Totale = tab[nrow(tab),])
  }
  if (totali == FALSE){ return(tab.1) }
  else
    return(tab.2)
}
