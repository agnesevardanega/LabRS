# Ricerca Sociale con R
# Agnese Vardanega - avardanega@unite.it

# pacchetti
library(Rcmdr)    # 2.4-4
library(LabRS)    # 0.1.0

# dati
data("MYSLID")


# GENERE -> RETRIBUZIONE ------------------------------------------

# Rcmdr tabella delle statistiche
numSummary(MYSLID[,"Retribuzione", drop=FALSE],
           groups=MYSLID$Genere,
           statistics=c("mean", "sd"), quantiles=c(0,.25,.5,.75,1))

# Rcmdr boxplot bivariato
Boxplot(Retribuzione~Genere, data=MYSLID, id=list(method="y"))


# TEST DELLE MEDIE
t.test(Retribuzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID)

t.test(Retribuzione~Genere, alternative='less',
       conf.level=.95, var.equal=FALSE, data=MYSLID)


# CONTROLLI -------------------------------------------------------

# controllo Gruppo linguistico
tab3 <- table(MYSLID$Lingua, MYSLID$Genere)
chisq.test(tab3)

# controllo Età
t.test(Eta~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID)

# controllo Istruzione
t.test(Istruzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE, data=MYSLID)

# controllo per <65 anni
t.test(Retribuzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE,
       data=MYSLID[MYSLID$Eta<66,])

t.test(Eta~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE,
       data=MYSLID[MYSLID$Eta<66,])

t.test(Istruzione~Genere, alternative='two.sided',
       conf.level=.95, var.equal=FALSE,
       data=MYSLID[MYSLID$Eta<66,])
