#' Einwilligungen einlesen
#'
#' read_ew() erlaubt das Einlesen von Einwilligungen
#' @param path_ew Dateipfad für Einwilligungen; tab-delimited csv
#' @param id_col Name der ID-Spalte
#' @param datum_col Name der Einwilligungsdatumsspalte
#' @param s_int Tage zwischen Einwilligung und Einladung
#' @param m_int Anzahl der Monate zwischen Einladungen
#' @param n_survey Anzahl der Einladungswellen / Surveys
#' @keywords Einwillungen
#' @export
#' @examples
#' read_ew()

read_ew <- function(path_ew,
                    id_col,
                    datum_col,
                    s_int,
                    # Tage zwischen Einwilligung und Einladung
                    m_int,
                    # Anzahl der Monate zwischen Einladungen
                    n_survey # Anzahl der Einladungswellen / Surveys
)
{
  dat_ew <- read_delim(path_ew, delim = "\t") %>%
    rename(ID_L3 = id_col,
           Datum_EW = datum_col) %>%
    mutate(Befragung = NA,
           ID_L3 = as.character(ID_L3))
  for (i in 1:n_survey) {
    dat_ew <- dat_ew %>%
      # Falls der Zeitraum zwischen dem Einladungsdatum und heute mindestens i x 6 Monate beträgt, dann weise Befragung i zu, ansonsten vorangegangene Befragung
      mutate(Befragung = ifelse(Datum_EW %m+% months(i * m_int) <= today(), i, Befragung))
  }
  dat_ew %>%
    distinct()
}
