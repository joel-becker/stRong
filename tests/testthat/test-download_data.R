library(stRong)

data <- download_data()

test_that("strong data has appropriate number of columns", {
  expect_equal(length(colnames(data)), 10)
})

data_missing <- data %>%
  filter(any_vars(is.na(.)))

test_that("strong data has no missing data", {
  expect_equal(nrow(data_missing), 0)
})
