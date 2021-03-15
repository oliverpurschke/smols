#' Update der Befragungsspalte auf der Teilnehner-Datenbank
#'
#' update_welle_on_db() erlaubt ein Update der Befragungsspalte auf der Teilnehner-Datenbank
#' @param befragung_code Befragungswelle
#' @param token Token/L3-ID des Befragungswelle die geupdatet werden soll
#' @export
#' @examples
#' update_welle_on_db()

update_welle_on_db <-
  function(befragung_code,
           token) {
    dbSendQuery(
      con,
      paste(
        "UPDATE Teilnehmer SET Befragung =",
        befragung_code,
        "WHERE Token IN",
        "(",
        token,
        ")"
      )
    )
  }
