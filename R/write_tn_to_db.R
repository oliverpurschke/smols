#' Neue Token auf Teilnehmerdatenbank schreiben
#'
#' write_tn_to_db() erlaubt das Schreiben neuen Token/L3-IDs auf die Teilnehmerdatenbank
#' @param user DB-User
#' @param pw DB-Passwort
#' @param dbname DB-Name
#' @param host DB-Host
#' @param port DB-Port
#' @param tab_name Name der DB-Tabelle
#' @param tn_to_add Tabelle mit Teilnehmer ID die an die Datenbank drangehangen werden soll
#' @export
#' @examples
#' write_tn_to_db()

write_tn_to_db <- function(user,
                           pw,
                           dbname,
                           host,
                           port,
                           tab_name,
                           tn_to_add) {
  con <- dbConnect(
    RMariaDB::MariaDB(),
    user = user,
    #password=rstudioapi::askForPassword("Database password"),
    password = pw,
    dbname = dbname,
    host = host,
    port = port
  )
  dbWriteTable(
    conn = con,
    name = tab_name,
    value = tn_to_add,
    append = TRUE
  )
  dbReadTable(con, "Teilnehmer")  #dbSendQuery(con, "DELETE FROM Teilnehmer WHERE Token < 40000000")
  dbDisconnect(con)
}
