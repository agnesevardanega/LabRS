#' Tabelle di frequenza
#'
#' Produce una tabella delle frequenze per la presentazione dei dati,
#' in valori assoluti e percentuali (due decimali di default)
#'
#' @param x fattore
#' @param digits numero dei decimali (vedi \code{\link[base]{round}})
#' @param totali logico: TRUE = con totale e casi mancanti
#' @return una matrice con due colonne
#' @examples data("MYSLID")
#' tabfreq(MYSLID$Lingua)
#' @export
tabfreq <- function (x, digits = 2, totali = TRUE) {
  if (!is.factor(x)) {
    return(NA)
  }
  tab <- table(x)
  tab.1 <- cbind("N" = tab, "%" = percent(tab, digits))
  tab.2 <- addmargins(tab.1, 1)
  rownames(tab.2) <- c(rownames(tab), "Totale")
  tab.2 <- rbind(tab.2, "NA" = c(sum(is.na(x)),
                                 round(sum(is.na(x)) / length(x), digits)))
  if (totali == FALSE){ return(tab.1) }
  else
    return(tab.2)
}

