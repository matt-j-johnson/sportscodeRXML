#' sportscode_df
#'
#' @importFrom Magritte %>%
#' @param filename the file path of the xml file to import
#' @param remove_dup whether to remove duplicate labels and only retain the first label of each group for each instance. Default is FALSE.
#'
#' @return a tibble of the xml data in wide format, with ID, start, end and code columns intact in a structure required to then export back into a compatible xml file.
#' @export
#'
sportscode_df <- function(filename, remove_dup = FALSE) {

  instances <- xml2::read_xml(filename) %>%
    xml2::xml_find_all(".//instance")

  df <- tibble::tibble(ID = "0", start = "0", end = "0", code = NA, group = NA, text = NA)
  for(i in 1:length(instances)) {
    text = instances[i] %>%
      xml2::xml_find_all("label[not(group)]") %>%
      xml2::xml_text()

    ungrouped <- tibble::tibble(group = "ungrouped", text = text)

    xml2::xml_remove(instances[i] %>%
                       xml2::xml_find_all("label[not(group)]"))
    id <- instances[i] %>%
      xml2::xml_find_first(".//ID") %>%
      xml2::xml_text()
    Start <- instances[i] %>%
      xml2::xml_find_first(".//start") %>%
      xml2::xml_text()
    End <- instances[i] %>%
      xml2::xml_find_first(".//end") %>%
      xml2::xml_text()
    Code <- instances[i] %>%
      xml2::xml_find_first(".//code") %>%
      xml2::xml_text()
    Group <- instances[i] %>%
      xml2::xml_find_all(".//label/group") %>%
      xml2::xml_text()
    Text <- instances[i] %>%
      xml2::xml_find_all(".//label/text") %>%
      xml2::xml_text()

    df_instance <- tibble::tibble(ID = id, start = Start, end = End, code = Code, group = Group, text = Text)
    df_instance <- dplyr::bind_rows(df_instance, ungrouped) %>%
      tidyr::fill(1:4)
    df <- dplyr::bind_rows(df, df_instance)
  }

  df <- df %>%
    na.omit()

  if(remove_dup){
    df <- df %>%
      dplyr::distinct(ID, group, .keep_all = TRUE)
  }

  df <- df %>%
    tidyr::pivot_wider(names_from = group, values_from = text, values_fill = NA) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), function(x) ifelse(x == 'NULL', NA, x)))

  return(df)
}
