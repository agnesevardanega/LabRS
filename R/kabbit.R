#' Tabelle 'kable' con formato dei numeri in italiano
#'
#' Genera tabelle con standard dei numeri in lingua italiana, con la funzione
#' kable di knitr:
#' separatore di decimali = virgola, separatore di migliaia = punto
#' @param x oggetto table o xtabs, matrice, o dataframe
#' @param ... altri argomenti ereditati da \code{\link[knitr]{kable}}
#' @return vettore carattere del codice della tabella
#' @examples data("MYSLID")
#' kabbit(table(MYSLID$Lingua))
#' @import knitr
#' @export
kabbit <- function(x, ...) {
  kable (x,
         format.args = list(decimal.mark = ",", big.mark = "."),
         ...)
}
