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
  if (cloud_platform == "googledrive") {
    googledrive::drive_find()

    path <- paste0(file_name, ".", file_format)
    googledrive::drive_download(path, overwrite = TRUE)
    data <- data.table::fread(path)
  }

  # TODO: other cloud platforms

  return(data)
}
