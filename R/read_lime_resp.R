#' Antwortstatus und -datum von LimeSurvey einlesen
#'
#' read_lime_resp() erlaubt das Einlesen von Informationen aus LimeSurvey
#' @param user LimeSurvey-User
#' @param pw LimeSurvey-Passwort
#' @param api LimeSurvey-API-Schnittstelle
#' @param complete vollständigkeitsstatus der Antworten
#' @param id LimeSurvey-ID
#' @export
#' @examples
#' read_lime_resp()

read_lime_resp <- function(user,
                           pw,
                           api,
                           complete,
                           id) {
  options(lime_api = api)
  options(lime_username = user)
  options(lime_password = pw)
  get_session_key()
  get_responses(
    iSurveyID = as.character(id),
    sLanguageCode = 'de',
    sResponseType = 'long',
    sCompletionStatus = "all",
    encode = c("form"),
    sHeadingType = "full" # 'code','full' or 'abbreviated' Optional defaults to 'code'
  ) %>%
    tbl_df() %>%
    select(
      ID_L3= 6,
      Datum_gestartet = "Datum.gestartet",
      #Datum_letzte_Aktivität = "Datum.letzte.AktivitÃ.t",
      Datum_Abgeschickt = "Datum.Abgeschickt",
      #Antwort_ID = "ï..Antwort.ID",
      Letzte_Seite = "Letzte.Seite"#,
    ) %>%
    mutate_at(vars(matches("Datum")), as_datetime)
}
