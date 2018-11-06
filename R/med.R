#' Mediana (fattori ordinati)
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
#' @import stats
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
