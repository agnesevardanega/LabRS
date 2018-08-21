\name{MYSLID}
\alias{MYSLID}
\docType{data}
\title{MYSLID (dati)}
\description{Dataset adattato da \link[carData]{SLID}. Tradotto e ricodificato}
\usage{data("MYSLID")}
\format{
  Data frame con 7425 casi e 7 variabili.
  \describe{
    \item{\code{ID}}{Identificativo di caso}
    \item{\code{Retribuzione}}{Retribuzione oraria (indice composito)}
    \item{\code{Istruzione}}{Numero di anni di scuola}
    \item{\code{Eta}}{In anni}
    \item{\code{Genere}}{Fattore con i livelli \code{Donna} \code{Uomo}}
    \item{\code{Lingua}}{Fattore con i livelli \code{Inglese} \code{Francese} \code{Altro}}
    \item{\code{Eta.classi}}{Fattore ordinato con i livelli \code{Fino a 25} < \code{26-35} < \code{36-45} < \code{46-55} < \code{55-65} < \code{Oltre 65}}
  }
}

\source{
Dataset tratto da carData}

\examples{
data(MYSLID)
}
\keyword{datasets}
