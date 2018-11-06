#' Differenza interquartile (fattori ordinati)
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
#' @import stats
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
