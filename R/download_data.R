#' @title download_data()
#'
#' @description Downloads and returns gym data from cloud platform
#' @param file_name File name
#' @param file_format File format
#' @param platform Cloud platform
#' @keywords download
#' @export
#' @examples
#' download_data()
download_data <- function(
  file_name,
  file_format,
  cloud_platform
) {
  if (cloud_platform == "gdrive") {
    googledrive::drive_download(file_name, type = file_format, overwrite = TRUE)
    path <- paste0(file_name, ".", file_format)
    data <- data.table::fread(path)
  }

  return(data)
}
