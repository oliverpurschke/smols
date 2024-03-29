#' Zweiten Reminder versenden
#'
#' send_remind_2_fun_2() erlaubt das versenden der zweiten Erinnerungsmail an die Teilnehmer innerhalb ab der zweiten Welle
#' @param dat Dataframe mit Adressinformationen der Teilnehmer die angeschrieben werden sollen
#' @param senden Sollen mail sofort gesendet werden oder erst in Outlook geöffnet werden (TRUE, FALSE)
#' @export
#' @examples
#' send_remind_2_fun_2()


send_remind_2_fun_2 <-
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

          "vor zwei Wochen haben wir Sie zu einer ",dat$Befragung[i],". Befragung im Rahmen der NAKO-Gesundheitsstudie eingeladen.
          M&ouml;glicherweise sind Ihnen die E-Mails nicht zugestellt worden oder Sie haben die E-Mails &uuml;bersehen. <p>",

          "Zur Durchf&uuml;hrung der Befragung klicken Sie bitte auf den folgenden Link:<p> ",
          "<a href='https://webszh.uk-halle.de/intkohort/'>
            https://webszh.uk-halle.de/intkohort/</a><p>",

          "Sollte der Link nicht aktiviert sein, bitten wir Sie den Link zu kopieren und in das Adressfeld Ihres
      Browsers
      einzugeben. Sie k&ouml;nnen die Befragung starten, in dem Sie Ihre Zugangsnummer
      ",
          "<b>",
          dat$ID_L3[i],
          "</b>",
          " eingeben und auf den Link <b>&bdquo;NAKO - Intensivierte Befragung ",dat$Befragung[i],"&rdquo;</b> klicken.<p>  ",

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
          dat$Erinnerung[i], ". ", "Erinnerung an den ",dat$Befragung[i],". Fragebogen der zusätzlichen Befragung der NAKO Gesundheitsstudie ",
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
        paste0(dat$Erinnerung[i], ". ", "Erinnerung für die ", dat$Befragung[i], ". Befragung wurde an ", dat$Email[i], " versendet"),
        "Bitte mail im Popup-Fenster manuell checken und versenden"
      ))
    }
  }

