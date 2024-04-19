#' sportscode_colours
#'
#' @importFrom Magritte %>%
#' @param filename the file path of the xml file to import the colours from
#'
#' @return a tibble of sportscode colours for each instance, to be used in conjunction with export_scxml
#' @export
#'
sportscode_colours <- function(filename) {

  rows <- xml2::read_xml(filename) %>%
    xml2::xml_find_all(".//row")

  df <- tibble::tibble(code = NA,
               R = NA,
               G = NA,
               B = NA)
  for (i in 1:length(rows)) {
    Code <- rows[i] %>%
      xml2::xml_find_first(".//code") %>%
      xml2::xml_text()
    Red <- rows[i] %>%
      xml2::xml_find_all(".//R") %>%
      xml2::xml_text()
    Green <- rows[i] %>%
      xml2::xml_find_all(".//G") %>%
      xml2::xml_text()
    Blue <- rows[i] %>%
      xml2::xml_find_all(".//B") %>%
      xml2::xml_text()

    df_instance <- tibble::tibble(code = Code,
                          R = Red,
                          G = Green,
                          B = Blue)

    df <- dplyr::bind_rows(df, df_instance)
  }

  df <- df %>% na.omit()

  return(df)
}

