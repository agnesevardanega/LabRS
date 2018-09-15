#' Devianza
#'
#' Calcola la devianza come \code{\link[stats]{cor}} * \code{\link[LabRS]{nval}} - 1.
#' Sono considerati i soli casi validi.
#'
#' @param x vettore
#' @param ... altri argomenti ereditati da \code{\link[stats]{cor}}
#' @return Valore della devianza
#' @export
devianza <- function (x, ...)
{
  var(x, na.rm = TRUE, ...) * (nval(x) - 1)
}

