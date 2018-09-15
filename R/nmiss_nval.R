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
  sum(!is.na(x))
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
  sum(is.na(x))
}




















