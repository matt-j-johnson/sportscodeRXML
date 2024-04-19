#' export_scxml
#'
#' @importFrom Magritte %>%
#' @param data a data frame or tibble that containes columns ID, start, end and code
#' @param label_names a vector of column names that you want to export as labels
#' @param file_name a character string to name the exported file
#' @param colours optional dataframe or tibble of colour codes to colour instances
#'
#' @return saves an xml file to the current working directory
#' @export
#'
export_scxml <- function(data, label_names, file_name = "SportsCode.xml", colours = NULL) {

  ### Create the head of the XML
  xml <- XML::xmlTree()
  suppressWarnings({xml$addTag("file", close = FALSE)})
  xml$addTag("ALL_INSTANCES", close = FALSE)

  ### Loop through each row and add xml instance
  for (i in 1:nrow(data)) {
    xml$addTag("instance", close = FALSE)

    xml$addTag("ID", data[i, ]$ID, close = TRUE)
    xml$addTag("start", data[i, ]$start, close = TRUE)
    xml$addTag("end", data[i, ]$end, close = TRUE)
    xml$addTag("code", data[i, ]$code, close = TRUE)

    for (k in 1:length(label_names)) {
      #if label text is not NA
      if (!is.na(data[i, label_names[k]])) {
        # create the Label
        suppressWarnings({
          xml$addTag("label", close = FALSE)
          xml$addTag("group", label_names[k], close = TRUE)
          xml$addTag("text", data[i, label_names[k]], close = TRUE)
          xml$closeTag() # </label> close the label
        })
      }

    }

    xml$closeTag() # </instance>

  }

  # close off the Instances tags
  xml$closeTag() # </ALL_INSTANCES>

  #If a set of colour information is provided - then add colours
  if(!is.null(colours)) {
    xml$addTag("ROWS", close = FALSE)

    for (i in 1:nrow(colours)) {
      xml$addTag("row", close = FALSE)

      xml$addTag("code", colours[i, ]$code, close = TRUE)
      xml$addTag("R", colours[i, ]$R, close = TRUE)
      xml$addTag("G", colours[i, ]$G, close = TRUE)
      xml$addTag("B", colours[i, ]$B, close = TRUE)

      xml$closeTag() # </row>
    }
    xml$closeTag() # </ROWS>
  }

  xml$closeTag() # </file>

  XML::saveXML(xml, file = file_name, encoding = "iso-8859-1")
}
