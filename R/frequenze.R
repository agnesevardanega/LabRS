#' Tabelle di frequenza di un dataframe (formato csv)
#'
#' Produce le tabelle delle frequenze per tutte le variabili categoriali
#' (fattori) di un file, con \code{\link[LabRS]{tabfreq}},
#' e le esporta in formato csv con \code{\link[LabRS]{exptab}}.
#'
#' @param x dataframe
#' @param write logico; esporta o no le frequenze in un file csv
#' @param file (se write = TRUE) carattere; nome del file di esportazione (con estensione csv)
#' @param aggiungi (se write = TRUE) logico; aggiunge le tabelle ad un file esistente (come append)
#' @param ... argomenti ereditati da \code{\link[LabRS]{tabfreq}}
#' @return file in formato csv con le tabelle,  o lista con le tabelle
#' @examples data("MYSLID")
#' frequenze(MYSLID, "frequenze.csv")
#' @import utils
#' @export
frequenze <- function (x, write = TRUE, file, aggiungi = FALSE, ...) {
  res <- lapply(x, tabfreq)
  res <- res[!is.na(res)]

  if (write == TRUE) {
    for (i in 1:length(res)){
      write(names(res)[i],
            file = file,
            append = ifelse(i == 1, aggiungi, TRUE))

      write.table(res[[i]],
                  file = file,
                  dec = ",",
                  sep = ";",
                  na = "",
                  col.names = NA,
                  append = T)

      write("", file = file, append = TRUE)
    }
  } else
    res
}
