#' Esporta una o piu' tabelle in un unico file csv (standard italiano)
#'
#' Esportazione semplificata di più tabelle in formato csv in uno stesso file
#' con \code{\link[utils]{write.table}}, e
#' secondo lo standard Excel in lingua italiana:
#' sep = ";",
#' dec = "," (separatore di decimali = virgola), NA = "" (celle vuote),
#' row.names = TRUE,
#' col.names = NA (per le colonne nel file csv).
#' @param tab lista degli oggetti (table); se la lista è con nomi, questi saranno
#' utilizzati come didascalie
#' @param file nome del file di esportazione (con estensione csv)
#' @param dids vettore carattere con le discalie delle tabelle;
#' per default, è costituito dal nome degli elementi della lista
#' @param aggiungi aggiunge le tabelle ad un file esistente (come append), logico
#' @param ... altri argomenti ereditati \code{\link[utils]{write.table}}
#' @examples data("MYSLID")
#' tab1 = tabfreq(MYSLID$Genere)
#' tab2 = tabcont(MYSLID$Lingua, MYSLID$Genere)
#' # una tabella
#' exptab(tab1, file = "tabelle.csv")
#' # tabelle diverse
#' exptab(list(tab1, tab2), file = "tabelle.csv",
#'         dids = c("Genere", "Lingua parlata per Genere"),
#'         aggiungi = TRUE)
#' @return file in formato csv con le tabelle
#' @export
exptab <- function(tab, file, dids = names(tab),
                   aggiungi = FALSE, ...)
{
  for (i in 1:length(tab)){
    write(dids[[i]],
          file,
          append = ifelse(i == 1, aggiungi, TRUE))
    write.table(tab[[i]],
               file,
               dec = ",",
               sep = ";",
               na = "",
               row.names = ifelse(length(dimnames(tab[[i]]))==1,
                                  FALSE, TRUE),
               col.names = ifelse(length(dimnames(tab[[i]]))==1,
                                  TRUE, NA),
               append = TRUE,
               ...)
    write(" ",
          file,
          append = TRUE)
  }
}

