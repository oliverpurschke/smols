#' Einladungsmails versenden
#'
#' send_invite_fun() erlaubt das versenden von ersten Einladungsemails an die Teilnehmer innerhalb der ersten Welle
#' @param dat Dataframe mit Adressinformationen der Teilnehmer die angeschrieben werden sollen
#' @param senden Sollen mail sofort gesendet werden oder erst in Outlook geöffnet werden (TRUE, FALSE)
#' @export
#' @examples
#' send_invite_fun()

send_invite_fun <- function(dat, senden) {
  for (i in 1:dim(dat)[1]) {
    prepare_email(
      #embeddings = NULL,
      #embeddings = datlist,
      body = glue(
        "<html>",
        "Sehr " ,
        dat$Anrede[i],
        " ",
        dat$Titel[i],
        " ",
        #dat$Vorname[i],
        #" ",
        dat$Name[i],
        ",<p>",
        "Sie haben bei Ihrem letzten Besuch im NAKO Studienzentrum freundlicherweise eingewilligt, an einer
      zus&auml;tzlichen,
      sich alle 6 Monate wiederholenden Befragung teilzunehmen (Projektname: <b>&bdquo;Intensivierte
      Befragung&rdquo;</b>). Die Teilnahme
      ist nat&uuml;rlich freiwillig. Die Umfrage wird ausschlie&szlig;lich online durchgef&uuml;hrt und ca. 10
      Minuten Ihrer Zeit in Anspruch nehmen. <p>",

        "Da sich gesundheitliche Faktoren auch kurzfristig &auml;ndern k&ouml;nnen, werden Ihnen bei der Befragung
      jeweils die gleichen Fragen, unter anderem zur k&ouml;rperlichen Aktivit&auml;t und zum Schlaf, gestellt,
      die Sie bereits bei Ihrem Besuch im Studienzentrum der NAKO beantwortet haben. Mit Ihrer Teilnahme leisten
      Sie einen weiteren Beitrag zum besseren Verst&auml;ndnis der Entstehung von h&auml;ufigen Erkrankungen.
      <p>",

        "Bitte klicken Sie zur Durchf&uuml;hrung der Befragung auf den folgenden Link:<p> ",
        "<a href='https://webszh.uk-halle.de/intkohort/'>
      https://webszh.uk-halle.de/intkohort/</a><p>",

        "Sollte der Link nicht aktiviert sein, bitten wir Sie den Link zu kopieren und in das Adressfeld Ihres
      Browsers
      einzugeben. Sie k&ouml;nnen die Befragung starten, in dem Sie Ihre Zugangsnummer
      ",
        "<b>",
        dat$ID_L3[i],
        "</b>",
        " eingeben und auf den Link <b>&bdquo;NAKO - Intensivierte Befragung 1&rdquo;</b> klicken.<p> ",

        "M&ouml;chten Sie doch nicht mehr an der zus&auml;tzlichen &bdquo;intensivierten Befragung&rdquo;
      teilnehmen, antworten Sie bitte direkt auf diese Mail. <p> ",

        "F&uuml;r R&uuml;ckfragen zu der zus&auml;tzlichen Befragung oder zur NAKO allgemein k&ouml;nnen Sie
      uns unter der
      Telefonnummer <b>(0345) 557-7939</b> kontaktieren oder direkt auf diese Mail antworten.<p>",
        "Mit freundlichen Gr&uuml;&szlig;en und den besten W&uuml;nschen f&uuml;r Ihre Gesundheit<p> ",
        "Ihr Team des NAKO Studienzentrums in Halle</html>",
        .na = ""
      ),
      #to = "oliverpurschke@web.de",
      to = dat$Email[i],
      #cc = "jazzy_grind",
      subject = paste0(
        "Einladung zur Teilnahme an einer zusätzlichen Befragung der NAKO Gesundheitsstudie ",
        "(B",
        dat$Befragung[i],
        ", A",
        dat$Erinnerung[i] + 1,
        ")"
      ),
      attachments = NULL,
      css = "",
      send = senden,
      max_image_height = 380,
      max_image_width = 380,
      data_file_format = "csv",
      col_names = TRUE,
      image_file_format = "png"
    )
    message(ifelse(
      senden,
      paste("Einladung wurde an", dat$Email[i], "versendet"),
      "Bitte mail im Popup-Fenster manuell checken und versenden"
    ))
  }
}
