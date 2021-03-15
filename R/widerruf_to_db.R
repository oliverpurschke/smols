#' Widerrufe auf die Teilnehmerdatenbank schreiben
#'
#' widerruf_to_db() erlaubt das Scheiben von Widerrufen auf die Teilnehmerdatenbank
#' @param wid_code Kodierung von Widerrufen auf den Teilnehmerdatenbank
#' @param token Token/L3-ID des Widerufs
#' @export
#' @examples
#' widerruf_to_db()

widerruf_to_db <- function(wid_code,
                           token) {
  dbSendQuery(con,
              paste(
                "UPDATE Teilnehmer SET Widerruf =",
                wid_code,
                "WHERE Token =",
                token
              ))
}
