#' @title wrangle_intensity_metrics()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons)
#' @param data Data from download_data()
#' @param sets_exponent Exponent for weighting number of sets
#' @keywords wrangle
#' @export
#' @examples
wrangle_intensity_metrics <- function(data = data,
                                      sets_exponent = 0.6
) {
  data <- data %>%

    # create arbitrary intensity measure
    dplyr::mutate(intensity = weight * (sets^sets_exponent)) %>%

    # intensity wrangling by exercise
    dplyr::group_by(exercise) %>%
    dplyr::mutate(
      # mean intensity of each exercise
      mean_intensity = mean(intensity),
      # index intensity to first exercise for each exercise
      index_intensity = intensity / first(intensity)
    ) %>%
    dplyr::ungroup() %>%

    # intensity wrangling by exercise type
    dplyr::mutate(
      # normalise intensity
      normalise_intensity = intensity / mean_intensity
    )

  # TODO: upweight intensity to the degree that movement involves many muscle groups
  # use `dplyr::mutate(weight_intensity = normalise_intensity * compound_factor)` to do so
  # perhaps same for aerobic exercise (* aerobic_factor?

  return(data)
}

# wrangle intensity metrics
# # merge with exercise compoundness info
# # within-workout
# # # weighted by compoundness
# # within-workout-exercise
# # # averaging over weight/sets
# # within-workout-exercise-set

# wrangle within exercise (over sets)
data <- data %>%
  group_by(Date, "Workout Name", "Exercise_Name")

#' @title merge_exercise_info
#'
#' @description merge strong data with info about exercises
#' @param data Data from download_data()
#' @keywords merge
#' @examples
#'
#' @noRd

#' @title wrangle_intensity_within_workout()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons) within workout
#' @param data Data from merge_exercise_info()
#' @keywords wrangle
#' @examples
#'
#' @noRd

#' @title wrangle_intensity_within_exercise()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons) within exercise
#' @param data Data from wrangle_intensity_within_workout()
#' @keywords wrangle
#' @examples
#'
#' @noRd

#' @title wrangle_intensity_within_set()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons) within set
#' @param data Data from wrangle_intensity_within_exercise()
#' @keywords wrangle
#' @examples
#'
#' @noRd
