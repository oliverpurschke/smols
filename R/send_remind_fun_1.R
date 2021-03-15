#' Ersten Reminder versenden
#'
#' send_remind_fun_1() erlaubt das versenden der ersten Erinnerungsmail an die Teilnehmer innerhalb der ersten Welle
#' @param dat Dataframe mit Adressinformationen der Teilnehmer die angeschrieben werden sollen
#' @param senden Sollen mail sofort gesendet werden oder erst in Outlook geöffnet werden (TRUE, FALSE)
#' @export
#' @examples
#' send_remind_fun_1()

send_remind_fun_1 <-
  function(dat,
           senden) {
    for (i in 1:dim(dat)[1]) {
      prepare_email(
        embeddings = NULL,
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

          "vor ein paar Tagen haben wir Sie zu einer Befragung im Rahmen der NAKO-Gesundheitsstudie eingeladen.
          M&ouml;glicherweise ist Ihnen die E-Mail nicht zugestellt worden oder Sie haben die E-Mail &uuml;bersehen. Sie haben bei Ihrem letzten Besuch im NAKO Studienzentrum freundlicherweise eingewilligt, an dieser
      zus&auml;tzlichen,
      sich alle 6 Monate wiederholenden Befragung teilzunehmen. <p>",

          "Zur Durchf&uuml;hrung der Befragung klicken Sie bitte auf den folgenden Link:<p> ",
          "<a href='https://websz.uk-halle.de'>
            https://websz.uk-halle.de</a><p>",

          "Sollte der Link nicht aktiviert sein, bitten wir Sie den Link zu kopieren und in das Adressfeld Ihres
      Browsers
      einzugeben. Sie k&ouml;nnen die Befragung starten, in dem Sie Ihre Zugangsnummer
      ",
          "<b>",
          dat$ID_L3[i],
          "</b>",
          " eingeben und auf den Link <b>&bdquo;NAKO - Intensivierte Befragung 1&rdquo;</b> klicken.<p>  ",

          "M&ouml;chten Sie doch nicht mehr an der zus&auml;tzlichen &bdquo;intensivierten Befragung&rdquo;
      teilnehmen, antworten Sie bitte direkt auf diese Mail. <p> ",

          "F&uuml;r R&uuml;ckfragen zu der zus&auml;tzlichen Befragung oder zur NAKO allgemein k&ouml;nnen Sie
      uns unter der
      Telefonnummer <b>(0345) 557-7939</b> kontaktieren oder direkt auf diese Mail antworten.<p>",

          "Mit freundlichen Gr&uuml;&szlig;en und den besten W&uuml;nschen f&uuml;r Ihre Gesundheit<p> ",

          "Ihr Team des NAKO Studienzentrums in Halle</html>",
          .na = ""
        ),
        to = dat$Email[i],
        #to = "oliverpurschke@web.de",
        #cc = "jazzy_grind",
        subject = paste0(
          "1. Erinnerung an die Einladung zur Teilnahme an der zusätzlichen Befragung der NAKO Gesundheitsstudie ",
          "(B",
          dat$Befragung[i],
          ", A",
          dat$Erinnerung[i] + 1,
          ")"
        ),
        attachments = NULL,
        css = "",
        send = senden,
        max_image_height = 8,
        max_image_width = 8,
        data_file_format = "csv",
        col_names = TRUE,
        image_file_format = "png"
      )
      message(ifelse(
        senden,
        paste("1. Erinnerung wurde an", dat$Email[i], "versendet"),
        "Bitte mail im Popup-Fenster manuell checken und versenden"
      ))
    }
  }
