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
    dplyr::filter(is.na("Date") |
                  is.na("Workout Name") |
                  is.na("Exercise Name") |
                  is.na("Set Order") |
                  is.na("Weight") |
                  is.na("Weight Unit") |
                  is.na("Reps"))
  expect_equal(nrow(missing_data), 0)
})


test_that("strong data has appropriate values", {
  check_download()

  data <- download_data(file_name, file_format, cloud_platform)

  # workout name strong never > 20 or < 4
  expect_equal(sum(nchar(data[["Workout Name"]])>20), 0)
  expect_equal(sum(nchar(data[["Workout Name"]])<4), 0)

  # excercise name limits
  expect_equal(sum(nchar(data[["Exercise Name"]])>50), 0)
  expect_equal(sum(nchar(data[["Exercise Name"]])<5), 0)

  # set order < 10
  expect_equal(sum(data[["Set Order"]]>10), 0)

  # weight < 1000
  expect_equal(sum(data[["Weight"]]>1000), 0)

  # reps < 50
  expect_equal(sum(data[["Reps"]]>50), 0)
})
