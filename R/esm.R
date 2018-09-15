#' Errore standard (della media)
#'
#' Calcola l'errore standard dalla media, e gli estremi dell'intervallo di confidenza.
#' Di default, considera solo i casi validi con un intervallo di confidenza del 95\%.
#'
#' @param x vettore
#' @param q quantile (o qualunque valore) della distribuzione normale
#' default = 1.96 (int. conf 95\%); per un intervallo di confidenza
#' del 99\% impostarlo a 2.58.
#' @param digits decimali
#' @return Valore della media, della deviazione standard, dell'errore standard,
#' estremi dell'intervallo di confidenza, valore p dell'area.
#' @examples esm(cars$speed)
#' @export
esm <- function (x, q = 1.96, digits = 6)
{
  media <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  es <- sd / sqrt(nval(x))
  q = abs(q)
  p <- pnorm(q)*2 - 1
  res <- c(media, sd, es, media - q * es, media + q * es, p)
  names(res) <- list("Media", "sd", "ES",
                     paste("-", round(q, 2)), paste("+", round(q, 2)), "p")
  round(res, digits)
}
