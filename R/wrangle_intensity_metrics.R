#' @title wrangle_intensity_metrics()
#'
#' @description Compute intensity metrics (for within- and between-exercise comparisons)
#' @param data Data from download_data()
#' @param sets_exponent Exponent for weighting number of sets
#' @keywords wrangle
#' @export
#' @examples
#' wrangle_intensity_metrics()
wrangle_intensity_metrics <- function(
  data = data,
  sets_exponent = 0.6,
  aerobic_intensity = 2,
  compound_intensity = 1.3,
  almost_compound_intensity = 1.1
) {
  # compute intensity metrics (for within- and between-exercise comparisons)

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
      normalise_intensity = intensity / mean_intensity,
      # weight intensity measure by exercise type
      weight_intensity = case_when(
        exercise %in% aerobic         ~ normalise_intensity * aerobic_intensity,
        #exercise %in% compound        ~ normalise_intensity * compound_intensity,
        #exercise %in% almost_compound ~ normalise_intensity * almost_compound_intensity,
        TRUE                          ~ normalise_intensity
      )
    )

  return(data)
}
