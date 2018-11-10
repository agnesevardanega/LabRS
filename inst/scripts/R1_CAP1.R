# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# CAPITOLO 1


# LA CONSOLE DI R ---------------------------------------------------------

3 + 2

# FUNZIONI DI AIUTO

help(chisq.test)
?chisq.test
??chisq


# RCOMMANDER --------------------------------------------------------------

install.packages("Rcmdr", dependencies = TRUE)
library(Rcmdr)


# WORKSPACE ---------------------------------------------------------------

# operatore di assegnamento
a <- 3 + 5
a

# elenco degli oggetti
ls()

# rimuovere uno o più oggetti
rm(a)

# salvare e caricare area di lavoro
# save.image("C:/Documenti/mydir/myfile.RData")
save.image(file.choose())

# caricare area di lavoro
# load("C:/Documenti/mydir/myfile.RData")
load(file.choose())


# DIRECTORY DI LAVORO -----------------------------------------------------

# per sapere quale è:
getwd()

# per cambiarla:
# setwd("C:/Documenti/mydir")
setwd(choose.dir())


# USARE GLI SCRIPTS DI LabRS ----------------------------------------------

# LabRS
install.packages("devtools", dependencies = TRUE)
devtools::install_github("agnesevardanega/LabRS")

# per trovare i file del pacchetto:
system.file(package = "LabRS")
system.file("extdata", package = "LabRS")  # per il file Excel
system.file("script", package = "LabRS")   # per gli scripts

# per aprire la cartella
shell.exec("C:/.../Documents/R/win-library/3.5/LabRS/")



