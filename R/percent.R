#' Percentuali
#'
#' Calcola le percentuali, con arrotondamento (se x = dataframe
#' -> as.matrix, se x = double -> as.array).
#'
#' @param x array (dataframe, numeric).
#' @param digits numero dei decimali (vedi \code{\link[base]{round}})
#' @param margin marginale (vedi \code{\link[base]{prop.table}})
#' @return matrice dei valori percentuali
#' @examples percent(table(ChickWeight$Diet), 2)
#' percent(HairEyeColor, 1, margin = c(3,2))
#' @export
percent <- function(x, digits = 8, margin = NULL) {
  x = if (is.data.frame(x)){
    as.matrix(x)
  } else if (is.double(x)) {
    as.array(x)} else x
  (round (prop.table(x, margin) * 100, digits))
}
