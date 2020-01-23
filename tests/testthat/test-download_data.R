library(stRong)

# use local defaults for download
file_name <- "strong_data"
file_format <- "csv"
cloud_platform <- "googledrive"

# skip tests if no internet connection for download
check_download <- function() {
  if (curl::has_internet() != TRUE) {
    skip("download not available")
  }
}


test_that("strong data has appropriate number of columns", {
  check_download()

  data <- download_data(file_name, file_format, cloud_platform)

  expect_equal(length(colnames(data)), 12)
})


test_that("strong data has no missing data at 7 vital columns", {
  check_download()

  missing_data <- download_data(file_name,
                                file_format,
                                cloud_platform) %>%
    dplyr::filter_at(vars("Date",
                     "Workout Name",
                     "Exercise Name",
                     "Set Order",
                     "Weight",
                     "Weight Unit",
                     "Reps"),
                     all_vars(is.na(.)))
  expect_equal(nrow(data_missing), 0)
})


test_that("strong data has appropriate values", {
  check_download()

  data <- download_data(file_name, file_format, cloud_platform)

  # workout name strong never > 20 or < 4

  # excercise name limits

  # set order < 10

  # weight < 1000

  # reps < 100
})
