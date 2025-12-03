#' Create table of descriptive statistics
#'
#' @param data A data frame
#'
#' @returns A data.frame/tibble.
create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(
      value,
      base::list(mean = mean, sd = sd)
    )) |>
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.numeric),
      \(x) base::round(x,
        digits = 1
      )
    )) |>
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) |>
    dplyr::select(
      Metabolite = metabolite,
      "Mean SD" = MeanSD
    )
}

#' Create a plot of distributions
#'
#' @param data A plot object
#'
#' @returns A plot object
create_plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(
      ggplot2::aes(x = value)
    ) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite),
      scales = "free"
    ) +
    ggplot2::theme_minimal()
}
