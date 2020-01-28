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
  # get data
  if (cloud_platform == "googledrive") {
    googledrive::drive_find()

    path <- paste0(file_name, ".", file_format)
    googledrive::drive_download(path, overwrite = TRUE)
  }

  # TODO: other cloud platforms

  # load data
  data <- data.table::fread(path)

  data <- data %>%

    # format column names
    rename_all(tolower) %>%
    rename("excercise_name" = "exercise name") %>%

    # unselect redundant cols
    select(-c("weight unit",
              "distance",
              "distance unit",
              "seconds",
              "notes",
              "workout notes"))

  return(data)
}
