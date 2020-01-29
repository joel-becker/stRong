#' @title merge_exercise_info
#'
#' @description merge strong data with info about exercises
#' @param data Data from download_data()
#' @param exercise_info Lookup table for exercises attached to package
#' @keywords merge
#' @examples
#'
#' @noRd
merge_exercise_info <- function(data,
                                exercise_info) {
  data <- data %>%
    full_join(exercise_info, by = "exercise_name") %>%
    arrange(date)

  return(data)
}

#' @title wrangle_intensity_within_set()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons) within set
#' @param data Data from merge_exercise_info()
#' @param weight_exponent Exponent weighting on weight
#' @param reps_exponent Exponent weighting on reps
#' @keywords wrangle
#' @examples
#'
#' @noRd
wrangle_intensity_within_set <- function(data,
                                         weight_exponent = 1.2, # marginal difficulty increase, inflate high weight
                                         reps_exponent = 0.8) { # marginal difficulty decrease, inflate low reps
  data <- data %>%
    # already at appropriate level so no need to group

    # create arbitrary intensity measure
    dplyr::mutate(intensity_set = (weight^weight_exponent) * (reps^reps_exponent) * total) %>%

    # intensity wrangling by exercise
    dplyr::group_by(exercise_name) %>%

    # index intensity to first exercise for each exercise
    dplyr::mutate(index_intensity_set = intensity_set / first(intensity_set)) %>%

    dplyr::ungroup()

  return(data)
}

#' @title wrangle_intensity_within_exercise()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons) within exercise
#' @param data Data from wrangle_intensity_within_set()
#' @param sets_exponent Exponent weighting on sets
#' @keywords wrangle
#' @examples
#'
#' @noRd
wrangle_intensity_within_exercise <- function(data,
                                              sets_exponent = 0.8) { # marginal difficulty decrease, inflate low sets
  data <- data %>%
    dplyr::group_by("date", "workout_name", "exercise_name") %>%

    # create arbitrary intensity measure
    dplyr::mutate(intensity_exercise = mean(intensity_set) * (n()^sets_exponent)) %>%

    # intensity wrangling by exercise
    dplyr::group_by(exercise_name) %>%

    # index intensity to first exercise for each exercise
    dplyr::mutate(index_intensity_exercise = intensity_exercise / first(intensity_exercise)) %>%

    dplyr::ungroup()

  return(data)
}

#' @title wrangle_intensity_within_workout()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons) within workout
#' @param data Data from wrangle_intensity_within_exercise()
#' @param exercise_exponent Exponent weighting on exercises
#' @keywords wrangle
#' @examples
#'
#' @noRd
wrangle_intensity_within_workout <- function(data,
                                             exercise_exponent = 0.8) { # marginal difficulty decrease, inflate low exercises
  data <- data %>%
    dplyr::group_by("date", "workout_name") %>%

    # create arbitrary intensity measure
    dplyr::mutate(intensity_workout = mean(intensity_exercise) * (n()^exercise_exponent)) %>%

    # intensity wrangling by exercise
    dplyr::group_by(exercise_name) %>%

    # index intensity to first exercise for each exercise
    dplyr::mutate(index_intensity_workout = intensity_workout / first(intensity_workout)) %>%

    dplyr::ungroup()

  return(data)
}

# TODO: check that above func is performing the correct within- calculation

# TODO: wrangle intensity within *WORKOUT NAME*

#' @title wrangle_intensity_metrics()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons)
#' @param data Data from download_data()
#' @param exercise_info Lookup table for exercises attached to package
#' @param weight_exponent Exponent weighting on weight
#' @param reps_exponent Exponent weighting on reps
#' @param sets_exponent Exponent weighting on sets
#' @param exercise_exponent Exponent weighting on exercises
#' @keywords wrangle
#' @export
#' @examples
wrangle_intensity_metrics <- function(data,
                                      exercise_info,
                                      weight_exponent = 1.2,
                                      reps_exponent = 0.8,
                                      sets_exponent = 0.8,
                                      exercise_exponent = 0.8) {
  data <- data %>%

    # merge with exercise info
    merge_exercise_info(.,
                        exercise_info) %>%

    # wrangle intensity within-set
    wrangle_intensity_within_set(.,
                                 weight_exponent,
                                 reps_exponent) %>%

    # wrangle intensity within-exercise
    wrangle_intensity_within_exercise(.,
                                      sets_exponent) %>%

    # wrangle intensity within-workout
    wrangle_intensity_within_workout(.,
                                     exercise_exponent)

  # TODO: factor in aerobic exercise, possibly with some aerobic_factor

  return(data)
}

