#' Adressen einlesen
#'
#' read_address() erlaubt das Einlesen der MODYS-Adressinformationen
#' @param path_address Dateipfad für Adressen; Excel-Arbeitsmappe mit xlsm-Erweiterung
#' @param col_types Spaltentypen
#' @keywords Adressen
#' @export
#' @examples
#' read_address()


read_address <- function (path_address){
  read_excel(path_address, col_types = c("text")) %>%
    mutate(
      Email_korrekt = str_remove(Email, "\\;.+"),
      Email_korrekt = str_replace(Email_korrekt, "\\,",
                                  "\\."),
      Email_korrekt = str_replace(Email_korrekt,
                                  "\\.vom$", "\\.com"),
      Email_korrekt = ifelse(str_detect(Email_korrekt,
                                        "@"), Email_korrekt, NA),
      Email_korrekt = str_remove(Email_korrekt,
                                 ".od\\.+"),
      Email_korrekt = str_remove(Email_korrekt,
                                 "oder\\ .+"),
      Email_korrekt = str_replace(Email_korrekt,
                                  "\\ ", ""),
      Email_korrekt = str_replace(Email_korrekt,
                                  "\\;", ""),
      Email_korrekt = trimws(Email_korrekt),
      Name = gsub("ä", "&auml;", Name, ignore.case = T),
      Name = gsub("ö", "&ouml;", Name, ignore.case = T),
      Name = gsub("ü", "&uuml;", Name, ignore.case = T),
      Name = gsub("ß", "&szlig;", Name, ignore.case = T),
      Anrede = ifelse(Anrede == "Herr", "geehrter Herr",
                      "geehrte Frau"),
      Email_vorhanden = ifelse(is.na(Email),
                               0, 1),
      Email = Email_korrekt
    ) %>% select(-Email_korrekt) %>%
    rename(ID_L3 = "UDOKU_ID_FB") %>%
    rename_at(vars(matches("^mfk_sid$")), function(x) "Station") %>%
    filter(Email_vorhanden == 1) %>%
    filter(str_detect(ID_L3, "\\d{8}")) %>%
    group_by(ID_L3) %>%
    summarise(across(everything(), first))
}

