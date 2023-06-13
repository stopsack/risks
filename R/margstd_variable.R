#' Parse exposure properties
#'
#' @details
#' For marginal standardization: Find exposure variable by is
#' given name vs. position; type of exposure variable (categorical
#' vs. continuous); levels of categorical variable if categorical
#'
#' @param fit fit
#' @param variable variable
#' @param at at
#'
#' @return A list
#' @noRd
find_margstd_exposure <- function(fit, variable = NULL, at = NULL) {
  # Find variable to standardize over
  if(!is.null(variable)) {  # If variable is given, check it exists in the model
    if(!(variable %in% names(fit$model)[2:length(names(fit$model))]))
      stop(paste0("Variable '", variable, "' is not part of the model."))
    predictor <- variable
  } else {  # Otherwise take first variable after intercept
    if(length(names(fit$model)) < 2)
      stop(paste("Model does not contain enough predictors. Variables found:",
                 paste(names(fit$model), sep = " ", collapse = " ")))
    predictor <- names(fit$model)[2]
  }

  # Find type of the variable
  model_vars <- tibble::tibble(vars = names(fit$model)) %>%
    dplyr::mutate(
      type    = purrr::map_chr(.x = .data$vars,
                               .f = ~class(fit$model %>%
                                             dplyr::pull(.x))[1]),
      nlevels = purrr::map_int(.x = .data$vars,
                               .f = ~length(unique(fit$model %>%
                                                     dplyr::pull(.x)))),
      categorical = dplyr::if_else(.data$type %in% c("character", "factor",
                                                     "logical", "ordered"),
                                   true = TRUE, false = FALSE)) %>%
    dplyr::filter(.data$vars == predictor)

  if(nrow(model_vars) == 0)
    stop(paste("No exposure variable identified."))

  margstd_levels <- NULL
  # Check levels to standardize at, if given
  if(!is.null(at)) {
    if(length(at) < 2)
      stop("'at' has fewer than 2 levels. Contrasts cannot be estimated.")
    if(
      inherits(
        x = fit$model %>% dplyr::pull(predictor),
        what = c("character", "factor", "ordered", "logical")) &
      sum(at %in% unique(fit$model %>%
                         dplyr::pull(predictor))) != length(at))
      stop(paste0("Some of the levels, specificied via 'at =', ",
                  "of the variable '",
                  predictor,
                  "' were not found in the model data."))
    if(
      inherits(
        x = fit$model %>% dplyr::pull(predictor),
        what = c("numeric", "integer"))) {
      if(min(fit$model %>% dplyr::pull(predictor)) > min(at) |
         max(fit$model %>% dplyr::pull(predictor)) < max(at))
        warning(paste0("Numeric levels provided via 'at = c(",
                       paste(at, sep = ", ", collapse = ", "),
                       ")' will lead to out-of-range predictions ",
                       "for the variable '", predictor, "'."))
    }
    margstd_levels <- at
    all_levels <- unique(fit$model %>% dplyr::pull(predictor))
  } else {
    # If no levels given, take from data for categorical variables:
    if(model_vars$categorical[1] == TRUE) {
      if(!is.null(fit$xlevels) & predictor %in% names(fit$xlevels))
        # retain level ordering as in model
        margstd_levels <- fit$xlevels[[predictor]]
      else  # if "hidden" categorical, use level orderings as in data
        margstd_levels <- unique(fit$model %>% dplyr::pull(predictor))
    }
    all_levels <- margstd_levels
  }

  # Find interaction terms. First, replace "(" in "factor(x)", then regex ":"
  predictor_replaced <- gsub(
    pattern = "\\(|\\)",
    replacement =  "",
    x = predictor)
  names_effects <- gsub(
    pattern = "\\(|\\)",
    replacement =  "",
    x = names(fit$effects))

  list(
    predictor = predictor,
    categorical = model_vars$categorical[1],
    margstd_levels = margstd_levels,
    all_levels = all_levels,
    interaction = (
      sum(
        grepl(
          pattern = ":",
          x = grep(
            pattern = paste0("^", predictor_replaced),
            x = names_effects,
            value = TRUE))) >
        sum(
          grepl(
            pattern = ":",
            x = as.character(
              unique(
                fit$model %>%
                  dplyr::pull(predictor)))))) |
      any(grepl(
        pattern = paste0(":", predictor_replaced),
        x = names_effects,
        fixed = TRUE)))
}
