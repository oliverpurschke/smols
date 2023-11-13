#' Einwillungen und Adressen verbinden
#'
#' ew_add_merge() erlaubt das Verbinden Adress- und Einwillungsinformationen
#' @param ew_dat Einwilligungsdatei
#' @param add_dat Addressdatei
#' @param stat_exclude Status der Teilnehmer in Modys der ausgeschlossen werden soll
#' @param n_max Maximale Teilnehmerzahl die angeschrieben werden soll
#' @param count_surv Zähler für Survey / Welle
#' @param resp_time ab zweiter Welle: Zeitraum, in Monaten, der zwischen Limesurvey Antwortdatum und derzeitigen Datum
#' @keywords Adressen, Einwilligungen
#' @export
#' @examples
#' ew_add_merge()


ew_add_merge <- function(ew_dat,
                         add_dat,
                         stat_exclude,
                         n_max,
                         count_surv,
                         # Zähler für Survey / Welle

                         resp_time # mindestzeitraum zwischen Anwort in vorangegangener Welle und Einladung (heute) in dieser Welle (z.b. nur Teilnehmer einladen bei denen die Antwort auf den letzten Survey mindestens resp_time Monate zurückliegt, bzw. die im letzten survey nicht geantwortet haben)
)
{
  ew_add_dat <- ew_dat %>%
    inner_join(add_dat) %>%
    filter(Befragung == count_surv,
           Email_vorhanden == 1,
           Station != stat_exclude) %>%

    mutate(Datum_Einladung_S1 = today(),
           #Befragung = count_surv,
           Erinnerung = 0) %>%
    ungroup()
  # if n_max !is.na, dann slice to n
  if (!is.na(n_max)) {
    ew_add_dat_2 <- slice(ew_add_dat, 1:n_max)
  } else{
    ew_add_dat_2 <- slice(ew_add_dat, 1:nrow(ew_add_dat))
  }
  # ab der zweiten Welle, filtern welche neuen Teilnehmer für diese Welle in der vorangegangenen Welle vor mindestens x (z.b. 5) Monaten geantwortet haben
  # die, die im vorherigen Survey nicht geantwortet haben, werden nicht entfernt (! nur die entfernen wo diff heute - antwort < 10 monate)
  # if count > 1, dann left_join(dat_sent %>% filter(Befragung == count - 1) %>% filter(is.na(Datum_Antwort | diff(antwort - today > 5 monate)))
  if (count_surv > 1) {
    ew_add_dat_3 <- ew_add_dat_2 %>%
      anti_join(
        dat_sent %>%
          filter(
            Befragung == count_surv - 1,
            Datum_Antwort %m+% months(resp_time) > today()
          ) %>%
          select(ID_L3, Datum_Antwort)
      )
  }
  else{
    ew_add_dat_3 <- ew_add_dat_2
  }
  ew_add_dat_3
}
