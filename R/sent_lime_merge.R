#' Verbinden von LimeSurvey mit bisherigen Teilnehmerantwortdaten
#'
#' sent_lime_merge() erlaubt das Verbinden bisherigen eingeladenden Teilnehmerinformationen von Informationen aus LimeSurvey und Aktualisierung des Erinnerungsstatus
#' @param dat_sent Bisherige eingeladene Teilnehmerdaten, dataframe
#' @param dat_lime Limesurvey Antwortdaten, dataframe
#' @param int_len Länge des Intervalls, in Tagen, zwischen heutigen und Limesurvey-Antwortdatum
#' @param int_n Maximale Anzahl der Reminder
#' @export
#' @examples
#' sent_lime_merge()

sent_lime_merge <- function(dat_sent,
                            dat_lime,
                            int_len,
                            int_n) {
  # Achtung: Teilnehmer
  dat_sent_2 <- dat_sent %>%
    #select(-Email_vorhanden) %>%
    left_join(
      dat_lime %>%
        mutate(ID_L3 = as.character(ID_L3)) %>%
        # filter Einträge in limesurvey die mindestens eine Seite (bzw. n_resp_min Seiten)
        # ausgefüllt haben
        filter(!is.na(Letzte_Seite)) %>%
        select(ID_L3,
               Datum_Antwort = Datum_gestartet,
               Letzte_Seite) #%>%
      #mutate(Erinnerung_neu = NA)
    )
  # update Erinnerungsspalte für die einen reminder bekommen sollen:
  # zahl zeigt an wieviele Errinnerung versenden werden/wurden
  for (i in 1:(int_n + 1)) {
    #i <- 2
    dat_sent_2 <- dat_sent_2 %>%
      mutate(Erinnerung =
               ifelse(
                 is.na(Datum_Antwort) &
                   (today() - as_date(Datum_Einladung_S1)) >= int_len * i,
                 i,
                 Erinnerung
               ))
  }
  dat_sent_2
}
