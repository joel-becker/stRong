library(stRong)

data <- download_data()


test_that("strong data has appropriate number of columns", {
  expect_equal(length(colnames(data)), 12)
})


data_missing <- data %>%
  filter_at(vars(c("Date",
                   "Workout Name",
                   "Exercise Name",
                   "Set Order",
                   "Weight",
                   "Weight Unit",
                   "Reps")),
            all_vars(is.na(.)))

test_that("strong data has no missing data at 7 vital columns", {
  expect_equal(nrow(data_missing), 0)
})


test_that("strong data has appropriate values", {
  # workout name strong never > 20 or < 4

  # excercise name limits

  # set order < 10

  # weight < 1000

  # reps < 100
})
