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

#' Do some cleaning to fix issues in data.
#'
#' @param data The lipidomics data frame
#'
#' @returns a data.frame
clean <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>
    dplyr::summarise(
      value = mean(value),
      .groups = "keep"
    ) |>
    dplyr::ungroup()
}

#' Preprocessing the data
#'
#' @param data The lipidomics data
#'
#' @returns A data.frame
preprocess <- function(data) {
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value)
    )
}

#' Fitting model
#'
#' @param data the lipidomics data
#' @param model the formula
#'
#' @returns a data.frame
fit_model <- function(data, model) {
  stats::glm(
    formula = model,
    data = data,
    family = binomial
  ) |>
    broom::tidy(exponentiate = TRUE) |>
    dplyr::mutate(
      metabolite = base::unique(data$metabolite),
      model = base::format(model),
      .before = tidyselect::everything()
    )
}

#' Create model results
#'
#' @param data the lipidomics data
#'
#' @returns A data frame of model results
create_model_results <- function(data) {
  data |>
    dplyr::filter(metabolite == "Cholesterol") |>
    preprocess() |>
    fit_model(class ~ value)
}

#' Fitting all models
#'
#' @param data the lipidomics data
#'
#' @returns A data frame of model results
fit_all_models <- function(data) {
  list(
    class ~ value,
    class ~ value + gender + age
  ) |>
    purrr::map(\(model) fit_model(data, model = model)) |>
    purrr::list_rbind()
}
