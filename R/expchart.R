#' Esporta i grafici
#'
#' Esportazione semplificata di immagini non vettoriali dei grafici con \code{\link[grDevices]{dev.copy}}.
#' I valori di default sono impostati in modo da esportare una immagine png con
#' la risoluzione e le dimensioni prodotte da R di default.
#' @param file nome del file di esportazione (con estensione in base al tipo di
#' immagine - device)
#' @param device default = png; formato immagine (png, jpeg, tiff, bmp; non pdf e svg) vedi \code{\link[grDevices]{dev.copy}}
#' @param ppi default = 72; risoluzione dell'immagine
#' @param hprop default = 6.7; altezza dell'immagine, in proporzione ai ppi (es: 4)
#' @param wprop default = 6.7; larghezza dell'immagine, in proporzione ai ppi (es: 4)
#' @param ... altri argomenti ereditati \code{\link[grDevices]{dev.copy}}
#' @examples expchart(png, file = "chart.png", ppi = 300, hprop = 4, wprop = 4)
#' @return file immagine
#' @export
expchart <- function(device = png, file, ppi = 72, hprop = 6.7, wprop = 6.7, ...)
{
  dev.copy(device,
           file,
           width = wprop*ppi,
           height = hprop*ppi,
           res = ppi,
           ...)
  dev.off()
}
