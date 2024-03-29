% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ew_add_merge.R
\name{ew_add_merge}
\alias{ew_add_merge}
\title{Einwillungen und Adressen verbinden}
\usage{
ew_add_merge(ew_dat, add_dat, stat_exclude, n_max, count_surv, resp_time)
}
\arguments{
\item{ew_dat}{Einwilligungsdatei}

\item{add_dat}{Addressdatei}

\item{stat_exclude}{Teilnehmerstatus in Modys der ausgeschlossen werden soll}

\item{n_max}{Maximale Teilnehmerzahl die angeschrieben werden soll}

\item{count_surv}{Zähler für Survey / Welle}

\item{resp_time}{ab zweiter Welle: Zeitraum, in Monaten, der zwischen Limesurvey Antwortdatum und derzeitigen Datum}
}
\description{
ew_add_merge() erlaubt das Verbinden Adress- und Einwillungsinformationen
}
\examples{
ew_add_merge()
}
\keyword{Adressen,}
\keyword{Einwilligungen}
