#' @title Download exercise data
#'
#' @description Downloads and returns data from google drive
#' @param file_name File name
#' @param file_format File format
#' @keywords download
#' @export
#' @examples
#' download_exercise_data()
download_exercise_data <- function(
  file_name = "exercise_data",
  file_format = "csv"
) {
  googledrive::drive_download(file_name, type = file_format, overwrite = TRUE)
  path <- paste0(file_name, ".", file_format)
  data <- data.table::fread(path)

  return(data)
}
