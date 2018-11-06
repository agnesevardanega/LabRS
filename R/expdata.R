#' Esporta un dataset in csv (standard italiano)
#'
#' Esportazione semplificata di un dataframe in formato csv,
#' con \code{\link[utils]{write.table}}, e
#' secondo lo standard italiano:
#' sep = ";",
#' dec = "," (separatore di decimali = virgola), NA = "" (celle vuote),
#' row.names = FALSE
#' (il dataframe da esportare ha una colonna / variabile per gli
#' identificativi di caso).
#' @param x dataset
#' @param file nome del file di esportazione (con estensione csv)
#' @param ... argomenti ereditati da \code{\link[utils]{write.table}}
#' @return file in formato csv
#' @examples data("MYSLID")
#' expdata(MYSLID, file = "MYSLID.csv")
#' @import utils
#' @export
expdata <- function(x, file, ...) {
  write.table(x, file,
              dec = ",",
              sep = ";",
              na = "",
              row.names = FALSE,
              ...
  )
}
