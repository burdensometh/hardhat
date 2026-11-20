# Reversible outcome transformations
#
# A "contract" for reversible outcome transformations:
#
# - Parameters are estimated from the *training* outcome only, inside
#   `fit_outcome_transform()`.
# - `mold_outcomes()` applies the fitted (frozen) transform and returns the
#   transformed training target.
# - `inverse_predictions()` uses the frozen transform to move model
#   predictions back to the original outcome scale. It works on numeric
#   predictions and on `quantile_pred` objects (the quantile levels are
#   preserved).
#
# The transform object is a plain list, so it survives saveRDS()/loadRDS()
# without any special support. Inverse-transforming a prediction never
# re-estimates anything from the data it is applied to.

# ------------------------------------------------------------------------------
# Constructors

#' Reversible outcome transformations
#'
#' @description
#' Outcome transformations change the scale on which a model is fit. A model
#' trained on a transformed outcome produces predictions on that transformed
#' scale, so the predictions have to be moved back to the original (business)
#' scale using the inverse transformation. These functions provide a small,
#' reversible "contract" that makes both halves of that workflow explicit and
#' reliable.
#'
#' * `outcome_log_transform()` creates a (parameter-free) log transformation.
#' * `outcome_box_cox_transform()` creates a Box-Cox power transformation whose
#'   power parameter `lambda` is estimated from the training outcome.
#'
#' The transformation itself is performed by [fit_outcome_transform()] followed
#' by [mold_outcomes()], and predictions are returned to the original scale with
#' [inverse_predictions()].
#'
#' @param base Base of the logarithm. Defaults to the natural logarithm
#'   (`base = exp(1)`). Must be a single positive number not equal to 1.
#'
#' @param lambda A single numeric value for the Box-Cox power parameter, or
#'   `NULL` (the default) to estimate `lambda` from the training outcome in
#'   [fit_outcome_transform()].
#'
#' @param limits Length-2 numeric vector giving the range of candidate values
#'   used when estimating `lambda`. Defaults to `c(-2, 2)`, the conventional
#'   Box-Cox search range.
#'
#' @param shift A single non-negative number added to the outcome before the
#'   transformation is applied. Required if the outcome contains values outside
#'   the transformation's domain (zero for the log transform, non-positive
#'   values for Box-Cox); it must be large enough to make every non-missing
#'   value strictly positive. When `NULL` (the default), no shift is applied and
#'   out-of-domain values raise an explicit error rather than being repaired
#'   silently.
#'
#' @return
#' An `outcome_transform` object. Before [fit_outcome_transform()] is called it
#' is "unfitted" and cannot be used to transform data; afterwards it carries the
#' frozen parameters and can be applied to any new values.
#'
#' @name outcome-transform
#'
#' @examples
#' # A parameter-free transform, ready to use without fitting
#' log_trans <- outcome_log_transform()
#' log_trans
#'
#' # A Box-Cox transform whose `lambda` will be estimated from the outcome
#' bc_trans <- outcome_box_cox_transform()
#' bc_trans
NULL

#' @rdname outcome-transform
#' @export
outcome_log_transform <- function(base = exp(1), shift = NULL) {
  check_number_decimal(base, min = .Machine$double.xmin, arg = "base")
  if (isTRUE(all.equal(base, 1))) {
    cli::cli_abort("{.arg base} must not be 1.")
  }
  check_number_decimal(shift, min = 0, allow_na = FALSE, allow_null = TRUE,
                       arg = "shift")

  new_outcome_transform(
    type = "log",
    params = list(base = base, shift = shift),
    fitted = FALSE,
    subclass = "outcome_log_transform"
  )
}

#' @rdname outcome-transform
#' @export
outcome_box_cox_transform <- function(
  lambda = NULL,
  limits = c(-2, 2),
  shift = NULL
) {
  check_number_decimal(lambda, allow_na = FALSE, allow_null = TRUE,
                       arg = "lambda")
  check_length_2_numeric(limits, arg = "limits")
  if (limits[[1]] >= limits[[2]]) {
    cli::cli_abort(
      "{.arg limits} must be strictly increasing, not {.val {limits}}."
    )
  }
  check_number_decimal(shift, min = 0, allow_na = FALSE, allow_null = TRUE,
                       arg = "shift")

  new_outcome_transform(
    type = "box_cox",
    params = list(lambda = lambda, limits = limits, shift = shift),
    fitted = FALSE,
    subclass = "outcome_box_cox_transform"
  )
}

new_outcome_transform <- function(type, params, fitted, subclass = character()) {
  structure(
    list(type = type, fitted = fitted, params = params),
    class = c(subclass, "outcome_transform")
  )
}

# Mark a transform as carrying frozen training-time parameters
mark_fitted <- function(transform, params) {
  transform$params <- params
  transform$fitted <- TRUE
  transform
}

#' Is `x` a fitted outcome transformation?
#'
#' `is_fitted_outcome_transform()` reports whether `x` is an
#' [outcome_transform] whose parameters have already been estimated from a
#' training outcome. Unfitted transforms must be passed through
#' [fit_outcome_transform()] before use.
#'
#' @param x An object.
#'
#' @return A single `TRUE` or `FALSE`.
#'
#' @export
#' @examples
#' is_fitted_outcome_transform(outcome_log_transform())
#'
#' trans <- fit_outcome_transform(outcome_log_transform(), 1:10)
#' is_fitted_outcome_transform(trans)
is_fitted_outcome_transform <- function(x) {
  inherits(x, "outcome_transform") && is_true(x$fitted)
}

check_outcome_transform <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_inherits(x, "outcome_transform", arg = arg, call = call)
}

check_fitted_outcome_transform <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_outcome_transform(x, arg = arg, call = call)
  if (!is_true(x$fitted)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has not been fitted to a training outcome yet.",
        i = "Call {.fn fit_outcome_transform} (or {.fn mold_outcomes}) first."
      ),
      call = call
    )
  }
  invisible(x)
}

# ------------------------------------------------------------------------------
# Printing

#' @export
print.outcome_transform <- function(x, ...) {
  cli::cli_text("<outcome transform: {x$type}>")
  cli::cli_text("Fitted: {x$fitted}")
  print_outcome_params(x)
  invisible(x)
}

print_outcome_params <- function(x) {
  params <- x$params
  if (x$type == "log") {
    cli::cli_text("Base: {params$base}")
    if (!is.null(params$shift)) {
      cli::cli_text("Shift: {params$shift}")
    }
  } else if (x$type == "box_cox") {
    lambda <- params$lambda
    if (is.null(lambda)) {
      cli::cli_text("Lambda: <to be estimated>")
    } else {
      cli::cli_text("Lambda: {format(lambda, digits = 4)}")
    }
    cli::cli_text("Search limits: [{params$limits[1]}, {params$limits[2]}]")
    if (!is.null(params$shift)) {
      cli::cli_text("Shift: {params$shift}")
    }
  }
  invisible(x)
}

# ------------------------------------------------------------------------------
# Fitting
#
# This is the *only* place parameters are estimated. The resulting transform is
# frozen and all later calls reuse these parameters verbatim.

#' Fit a reversible outcome transformation
#'
#' @description
#' `fit_outcome_transform()` estimates any data-dependent parameters of an
#' [outcome_transform] from the **training outcome only**, then freezes them.
#' After fitting, the transform can be applied to the training outcome with
#' [mold_outcomes()] and used to back-transform predictions with
#' [inverse_predictions()].
#'
#' Missing values in `outcome` are removed before estimating parameters; they
#' are re-introduced in the corresponding positions when the transform is
#' applied. Non-missing values outside the transformation's domain cause an
#' informative error.
#'
#' Calling this function on an already-fitted transform is an error: parameters
#' are never re-estimated from new data.
#'
#' @param transform An [outcome_transform] created by
#'   [outcome_log_transform()] or [outcome_box_cox_transform()].
#'
#' @param outcome A numeric vector, or a data frame / tibble with a single
#'   numeric column, containing the **training** outcome.
#'
#' @param ... Not used.
#'
#' @inheritParams rlang::args_error_context
#'
#' @return A fitted `outcome_transform` with frozen parameters.
#'
#' @export
#' @examples
#' trans <- outcome_box_cox_transform()
#'
#' # `lambda` is estimated from the training outcome
#' fitted <- fit_outcome_transform(trans, mtcars$mpg)
#' fitted
fit_outcome_transform <- function(
  transform,
  outcome,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  check_outcome_transform(transform, call = call)
  if (is_true(transform$fitted)) {
    cli::cli_abort(
      c(
        "The outcome transform has already been fitted.",
        i = "Transform parameters are estimated once, from the training
           outcome. They cannot be re-estimated from new data."
      ),
      call = call
    )
  }

  outcome <- prep_outcome_for_fit(outcome, call = call)

  UseMethod("fit_outcome_transform")
}

#' @export
fit_outcome_transform.outcome_log_transform <- function(
  transform,
  outcome,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  params <- transform$params
  shift <- validate_shift(
    shift = params$shift,
    outcome = outcome,
    type = "log",
    call = call
  )

  shifted <- outcome + shift
  validate_positive(shifted, type = "log", call = call)

  mark_fitted(transform, list(base = params$base, shift = shift))
}

#' @export
fit_outcome_transform.outcome_box_cox_transform <- function(
  transform,
  outcome,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  params <- transform$params
  shift <- validate_shift(
    shift = params$shift,
    outcome = outcome,
    type = "box_cox",
    call = call
  )

  shifted <- outcome + shift
  validate_positive(shifted, type = "box_cox", call = call)

  if (is.null(params$lambda)) {
    lambda <- estimate_box_cox_lambda(
      shifted,
      limits = params$limits,
      call = call
    )
  } else {
    lambda <- params$lambda
  }

  mark_fitted(
    transform,
    list(lambda = lambda, limits = params$limits, shift = shift)
  )
}

# ------------------------------------------------------------------------------
# Application (forward direction)

#' Apply a fitted outcome transformation
#'
#' @description
#' `apply_outcome_transform()` applies a fitted [outcome_transform] to values
#' using the parameters frozen during [fit_outcome_transform()]. It is a generic
#' with methods for numeric vectors and [quantile_pred] objects.
#'
#' No parameters are estimated here: the same frozen transform is applied to
#' every input. Missing values are preserved in place.
#'
#' @param transform A fitted [outcome_transform].
#'
#' @param x A numeric vector or a [quantile_pred].
#'
#' @param ... Not used.
#'
#' @inheritParams rlang::args_error_context
#'
#' @return An object of the same type and size as `x`, on the transformed
#'   scale.
#'
#' @export
#' @examples
#' fitted <- fit_outcome_transform(outcome_log_transform(), mtcars$mpg)
#'
#' apply_outcome_transform(fitted, c(10, 20, 30))
apply_outcome_transform <- function(transform, x, ..., call = caller_env()) {
  check_dots_empty0(...)
  check_fitted_outcome_transform(transform, call = call)
  UseMethod("apply_outcome_transform", x)
}

#' @export
apply_outcome_transform.default <- function(transform, x, ..., call = caller_env()) {
  check_dots_empty0(...)
  stop_input_type(
    x,
    what = "a numeric vector or a <quantile_pred>",
    arg = "x",
    call = call
  )
}

#' @export
apply_outcome_transform.numeric <- function(transform, x, ..., call = caller_env()) {
  check_dots_empty0(...)
  transform_values(transform, x, call = call)
}

#' @export
apply_outcome_transform.quantile_pred <- function(
  transform,
  x,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  levels <- extract_quantile_levels(x)
  values <- transform_values(transform, as.matrix(x), call = call)
  quantile_pred(values, quantile_levels = levels)
}

# ------------------------------------------------------------------------------
# Inverse application (back to the business scale)

#' Back-transform predictions to the original outcome scale
#'
#' @description
#' `inverse_outcome_transform()` applies the inverse of a fitted
#' [outcome_transform] to predictions made on the transformed scale. It is a
#' generic with methods for numeric vectors and [quantile_pred] objects.
#'
#' The inverse is evaluated with the parameters frozen during
#' [fit_outcome_transform()]; nothing is estimated from `x`. For a
#' [quantile_pred], the quantile levels are preserved exactly, and each
#' quantile value is back-transformed individually. This is valid because a
#' monotonic transformation maps quantiles to quantiles.
#'
#' @section Mean prediction semantics:
#'
#' Back-transforming a point prediction returns the inverse transformation of
#' *that number*. For a log transform this is the geometric mean of the
#' response (the median, under symmetric log-scale errors), **not** the
#' expected value on the original scale. In general,
#' `g^{-1}(E[g(Y)]) != E[Y]` for nonlinear `g()`. This function never claims
#' the back-transformed point estimate is an expectation on the original
#' scale; obtaining that expectation requires a bias correction informed by
#' the model's predictive distribution.
#'
#' @param transform A fitted [outcome_transform].
#'
#' @param x A numeric vector or a [quantile_pred] of predictions on the
#'   transformed scale.
#'
#' @param ... Not used.
#'
#' @inheritParams rlang::args_error_context
#'
#' @return An object of the same type and size as `x`, on the original
#'   outcome scale.
#'
#' @export
#' @examples
#' fitted <- fit_outcome_transform(outcome_log_transform(), mtcars$mpg)
#'
#' transformed_pred <- c(2.5, 3.0)
#' inverse_outcome_transform(fitted, transformed_pred)
#'
#' # Quantile predictions keep their levels
#' qp <- quantile_pred(matrix(c(2.5, 3.0, 3.5), nrow = 1), c(.1, .5, .9))
#' inverse_outcome_transform(fitted, qp)
inverse_outcome_transform <- function(transform, x, ..., call = caller_env()) {
  check_dots_empty0(...)
  check_fitted_outcome_transform(transform, call = call)
  UseMethod("inverse_outcome_transform", x)
}

#' @export
inverse_outcome_transform.default <- function(
  transform,
  x,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  stop_input_type(
    x,
    what = "a numeric vector or a <quantile_pred>",
    arg = "x",
    call = call
  )
}

#' @export
inverse_outcome_transform.numeric <- function(
  transform,
  x,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  inverse_transform_values(transform, x, call = call)
}

#' @export
inverse_outcome_transform.quantile_pred <- function(
  transform,
  x,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  levels <- extract_quantile_levels(x)
  values <- inverse_transform_values(transform, as.matrix(x), call = call)
  quantile_pred(values, quantile_levels = levels)
}

# ------------------------------------------------------------------------------
# Core forward / inverse math (works elementwise on vectors and matrices)

transform_values <- function(transform, x, call = caller_env()) {
  if (transform$type == "log") {
    shifted <- x + transform$params$shift
    validate_positive(shifted, type = "log", call = call)
    log(shifted, base = transform$params$base)
  } else if (transform$type == "box_cox") {
    shifted <- x + transform$params$shift
    validate_positive(shifted, type = "box_cox", call = call)
    box_cox_forward(shifted, lambda = transform$params$lambda)
  }
}

inverse_transform_values <- function(transform, x, call = caller_env()) {
  if (transform$type == "log") {
    base <- transform$params$base
    out <- base^x - transform$params$shift
  } else if (transform$type == "box_cox") {
    lambda <- transform$params$lambda
    if (!box_cox_is_log(lambda)) {
      radicands <- lambda * x + 1
      bad <- !is.na(radicands) & radicands <= 0
      if (any(bad)) {
        n_bad <- sum(bad)
        cli::cli_abort(
          c(
            "The Box-Cox inverse transform requires {.code lambda * x + 1 > 0}.",
            i = "{n_bad} non-missing predicted value{?s} {?was/were} outside the
                 domain of the fitted transform (lambda = {format(lambda,
                 digits = 4)}). These predictions cannot correspond to any
                 real-valued outcome on the original scale."
          ),
          call = call
        )
      }
    }
    out <- box_cox_inverse(x, lambda = lambda) -
      transform$params$shift
  }
  # The forward domain is (0, Inf) after shifting, so the inverse must return
  # strictly positive shifted values. `exp()` cannot fail, but a finite
  # transformed value may back-transform beyond what doubles can represent.
  if (any(is.infinite(out) & !is.infinite(x), na.rm = TRUE)) {
    cli::cli_warn(
      "Some back-transformed values are infinite (outside the range of
       double precision)."
    )
  }
  out
}

# Box and Cox (1964), using the one-parameter continuous-at-zero form.
# An explicit tolerance keeps the lambda = 0 branch deterministic regardless
# of the machine's default `all.equal()` scaling.
box_cox_is_log <- function(lambda) {
  abs(lambda) < 1e-6
}

box_cox_forward <- function(x, lambda) {
  if (box_cox_is_log(lambda)) {
    log(x)
  } else {
    (x^lambda - 1) / lambda
  }
}

box_cox_inverse <- function(x, lambda) {
  if (box_cox_is_log(lambda)) {
    exp(x)
  } else {
    (lambda * x + 1)^(1 / lambda)
  }
}

# Profile log-likelihood maximization over `lambda` (Box & Cox, 1964)
estimate_box_cox_lambda <- function(x, limits, call = caller_env()) {
  n <- length(x)
  log_x <- log(x)

  profile_loglik <- function(lambda) {
    transformed <- box_cox_forward(x, lambda)
    var_est <- suppressWarnings(stats::var(transformed))
    if (!is.finite(var_est) || var_est <= 0) {
      return(-Inf)
    }
    # The Jacobian term makes the profiles comparable across lambda.
    -n / 2 * log(var_est) + (lambda - 1) * sum(log_x)
  }

  # `optimize()` maximizes on the interior of the limits; grid-evaluate the
  # boundary candidates as well so a boundary optimum is not missed.
  interior <- tryCatch(
    suppressWarnings(stats::optimize(
      profile_loglik,
      interval = limits,
      maximum = TRUE,
      tol = .Machine$double.eps^0.25
    )),
    error = function(e) {
      cli::cli_abort(
        c(
          "The Box-Cox {.arg lambda} profile could not be optimized.",
          i = "The transformed outcome appears to have (near) zero variance."
        ),
        parent = e,
        call = call
      )
    }
  )

  candidates <- c(interior$maximum, limits[[1]], limits[[2]])
  objectives <- vapply(candidates, profile_loglik, numeric(1))

  if (!any(is.finite(objectives))) {
    cli::cli_abort(
      "The Box-Cox {.arg lambda} could not be estimated because the outcome
       has (near) zero variance after transformation.",
      call = call
    )
  }

  best <- which.max(objectives)
  lambda <- candidates[[best]]
  unname(lambda)
}

# ------------------------------------------------------------------------------
# Domain validation and shift handling

validate_positive <- function(x, type, call = caller_env()) {
  # NAs are allowed and simply propagate through the math; non-NA values must
  # lie inside the domain.
  bad <- !is.na(x) & x <= 0
  if (any(bad)) {
    n_bad <- sum(bad)
    example <- utils::head(x[bad], n = 3)
    if (type == "log") {
      msg <- c(
        "The log outcome transform requires strictly positive values.",
        i = "{n_bad} non-missing value{?s} {?was/were} outside the domain.
             Example: {.val {example}}.",
        i = "Supply a positive {.arg shift} so the outcome is strictly positive
             before logging."
      )
    } else {
      msg <- c(
        "The Box-Cox outcome transform requires strictly positive values.",
        i = "{n_bad} non-missing value{?s} {?was/were} outside the domain.
             Example: {.val {example}}.",
        i = "Supply a positive {.arg shift} so the outcome is strictly positive
             before the Box-Cox transformation."
      )
    }
    cli::cli_abort(msg, call = call)
  }
  invisible(x)
}

# Validate the user-supplied additive shift against the data domain. Shifts
# are never inferred silently: out-of-domain data without an explicit shift is
# an error, so the contract stays explicit and reproducible.
validate_shift <- function(shift, outcome, type, call = caller_env()) {
  observed <- outcome[!is.na(outcome)]
  min_observed <- min(observed)
  needs_shift <- min_observed <= 0

  if (is.null(shift)) {
    if (needs_shift) {
      if (type == "log") {
        cli::cli_abort(
          c(
            "The log outcome transform requires strictly positive values, but
             the smallest non-missing outcome is {min_observed}.",
            i = "Supply a {.arg shift} strictly greater than {-min_observed} so
                 that {.code outcome + shift} is strictly positive before
                 logging."
          ),
          call = call
        )
      } else {
        cli::cli_abort(
          c(
            "The Box-Cox outcome transform requires strictly positive values,
             but the smallest non-missing outcome is {min_observed}.",
            i = "Supply a {.arg shift} strictly greater than {-min_observed} so
                 that {.code outcome + shift} is strictly positive before the
                 Box-Cox transformation."
          ),
          call = call
        )
      }
    }
    return(0)
  }

  if (!is.numeric(shift) || length(shift) != 1L || is.na(shift) || shift < 0) {
    cli::cli_abort(
      "{.arg shift} must be a single non-negative number.",
      call = call
    )
  }

  if (needs_shift && shift <= -min_observed) {
    cli::cli_abort(
      c(
        "The supplied {.arg shift} ({shift}) does not make the outcome
         strictly positive.",
        i = "The smallest outcome value is {min_observed}, so the shift must be
             strictly greater than {-min_observed}."
      ),
      call = call
    )
  }

  shift
}

# ------------------------------------------------------------------------------
# Input preparation

# Accept vectors and 1-column data frames; reject everything else explicitly.
prep_outcome_for_fit <- function(outcome, call = caller_env()) {
  if (is.data.frame(outcome)) {
    if (ncol(outcome) != 1L) {
      cli::cli_abort(
        "Outcome transformations require a univariate outcome, but
         {ncol(outcome)} outcome columns were found.",
        call = call
      )
    }
    outcome <- outcome[[1]]
  } else if (is.matrix(outcome)) {
    if (NCOL(outcome) != 1L) {
      cli::cli_abort(
        "Outcome transformations require a univariate outcome, but
         {NCOL(outcome)} outcome columns were found.",
        call = call
      )
    }
    dim(outcome) <- NULL
  }

  if (!is.numeric(outcome)) {
    stop_input_type(
      outcome,
      what = "a numeric outcome",
      arg = "outcome",
      call = call
    )
  }

  complete <- outcome[!is.na(outcome)]
  if (length(complete) == 0L) {
    cli::cli_abort(
      "Cannot fit an outcome transform when every outcome value is missing.",
      call = call
    )
  }

  outcome
}

check_length_2_numeric <- function(x, arg, call = caller_env()) {
  if (!is.numeric(x) || length(x) != 2L || anyNA(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a length-2 numeric vector.",
      call = call
    )
  }
  invisible(x)
}

# ------------------------------------------------------------------------------
# Workflow bridges

#' Mold training outcomes with a reversible transform
#'
#' @description
#' `mold_outcomes()` is the training-time bridge between an
#' [outcome_transform] and a model fit. It fits the transform to the training
#' outcome (if not already fitted), returns the transformed target to train on,
#' and returns the frozen transform for later use in [inverse_predictions()].
#'
#' Parameters are estimated once, from `outcome`, and then frozen. Applying the
#' returned transform to other data never re-estimates anything.
#'
#' @param transform An [outcome_transform], fitted or unfitted.
#'
#' @param outcome A numeric vector, or a data frame / tibble with a single
#'   numeric column: the **training** outcome.
#'
#' @param ... Not used.
#'
#' @inheritParams rlang::args_error_context
#'
#' @return A named list with two elements:
#'
#' * `outcomes`: a tibble with a single `.outcome` column containing the
#'   transformed training target (missing values preserved in place).
#'
#' * `transform`: the fitted, frozen `outcome_transform`.
#'
#' @export
#' @examples
#' result <- mold_outcomes(outcome_log_transform(), mtcars$mpg)
#'
#' # The target the model actually sees
#' head(result$outcomes)
#'
#' # The frozen transform, to pass on to `inverse_predictions()`
#' result$transform
mold_outcomes <- function(
  transform,
  outcome,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  check_outcome_transform(transform, call = call)

  original <- prep_outcome_for_fit(outcome, call = call)

  if (!is_true(transform$fitted)) {
    transform <- fit_outcome_transform(transform, original, call = call)
  }

  transformed <- apply_outcome_transform(transform, original, call = call)

  list(
    outcomes = tibble(.outcome = transformed),
    transform = transform
  )
}

#' Back-transform model predictions to the business scale
#'
#' @description
#' `inverse_predictions()` is the prediction-time bridge between a model fit on
#' transformed outcomes and predictions on the original outcome scale. It takes
#' the frozen transform produced by [mold_outcomes()] (or
#' [fit_outcome_transform()]) and applies its inverse to the model's
#' predictions.
#'
#' The transform must already be fitted: this function never estimates
#' parameters from the predictions themselves.
#'
#' * Numeric `predictions` are back-transformed elementwise and returned as a
#'   tibble with a `.pred` column (the same convention as
#'   [spruce_numeric()]).
#' * [quantile_pred] `predictions` are back-transformed value-by-value while
#'   the quantile levels are preserved, and returned as a tibble with a
#'   `.pred_quantile` column.
#'
#' @section What the back-transformed estimate is (and is not):
#'
#' A back-transformed point prediction is the inverse transform of the model's
#' prediction on the transformed scale. For a log transform of the outcome
#' `Y`, the model predicts `E[log(Y)]`, and `exp(E[log(Y)])` is the geometric
#' mean (median under log-symmetric errors), not `E[Y]`. The function does not
#' label the result as an original-scale expectation: obtaining `E[Y]` itself
#' requires a bias correction informed by the model's predictive distribution,
#' which is outside the scope of this reversible transform contract.
#'
#' @param transform A fitted [outcome_transform].
#'
#' @param predictions A numeric vector or a [quantile_pred] of predictions on
#'   the transformed scale.
#'
#' @param ... Not used.
#'
#' @inheritParams rlang::args_error_context
#'
#' @return A tibble:
#'
#' * `.pred` (numeric) for numeric predictions, or
#' * `.pred_quantile` (a [quantile_pred]) for quantile predictions, with the
#'   same quantile levels as the input.
#'
#' @export
#' @examples
#' molded <- mold_outcomes(outcome_log_transform(), mtcars$mpg)
#'
#' # Pretend the model produced these predictions on the log scale
#' log_scale_preds <- c(2.9, 3.0, 3.1)
#'
#' # Back to miles-per-gallon
#' inverse_predictions(molded$transform, log_scale_preds)
#'
#' # Quantile predictions keep their levels
#' qp <- quantile_pred(matrix(c(2.7, 3.0, 3.3), nrow = 1), c(.1, .5, .9))
#' inverse_predictions(molded$transform, qp)
inverse_predictions <- function(
  transform,
  predictions,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  check_fitted_outcome_transform(transform, call = call)

  n_pred <- vec_size(predictions)

  if (is_quantile_pred(predictions)) {
    back <- inverse_outcome_transform(
      transform,
      predictions,
      call = call
    )
    out <- tibble(.pred_quantile = back)
  } else if (is.numeric(predictions) && is.null(dim(predictions))) {
    back <- inverse_outcome_transform(
      transform,
      predictions,
      call = call
    )
    out <- tibble(.pred = back)
  } else {
    stop_input_type(
      predictions,
      what = "a numeric vector or a <quantile_pred>",
      arg = "predictions",
      call = call
    )
  }

  validate_prediction_size(out, data.frame(dummy = seq_len(n_pred)))
  out
}
