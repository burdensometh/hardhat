#' Predict from a `{{model}}`
#'
#' @param object A `{{model}}` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions on the response scale, i.e. expected
#'   counts.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @details
#'
#' If the model was fit with `offset()` terms, `new_data` must contain the
#' columns required to compute them. [hardhat::forge()] recomputes the offset
#' from `new_data`, and it is included in the linear predictor before the
#' response scale is restored, so predictions always use the new samples' own
#' exposure, never the one seen at fit time.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -4]
#'
#' # Fit, with an offset
#' mod <- {{model}}(hp ~ cyl + offset(log(disp)), train)
#'
#' # Predict, with preprocessing. The offset is recomputed from `test`.
#' predict(mod, test)
#'
#' @export
predict.{{model}} <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_{{model}}_predict_types())
  predict_{{model}}_bridge(type, object, forged)
}

valid_{{model}}_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_{{model}}_bridge <- function(type, model, forged) {
  predictors <- as.matrix(forged$predictors)

  # `forge()` recomputes any `offset()` terms from `new_data`, so the offset
  # always comes from the new samples' own exposure.
  offset <- extract_{{model}}_offset(forged$extras)

  predict_function <- get_{{model}}_predict_function(type)
  predictions <- predict_function(model, predictors, offset)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_{{model}}_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_{{model}}_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_{{model}}_numeric <- function(model, predictors, offset) {
  # The offset is part of the linear predictor, not a learnable coefficient
  linear_predictor <- as.vector(predictors %*% model$coefs)

  if (!is.null(offset)) {
    linear_predictor <- linear_predictor + offset
  }

  # Restore the response scale with the inverse of the log link
  predictions <- stats::poisson()$linkinv(linear_predictor)

  hardhat::spruce_numeric(predictions)
}
