test_that("log transform: construction and print", {
  trans <- outcome_log_transform()
  expect_s3_class(trans, "outcome_log_transform")
  expect_s3_class(trans, "outcome_transform")
  expect_false(is_fitted_outcome_transform(trans))
  expect_snapshot(print(trans))

  expect_snapshot(error = TRUE, outcome_log_transform(base = 1))
  expect_snapshot(error = TRUE, outcome_log_transform(base = -2))
  expect_snapshot(error = TRUE, outcome_log_transform(shift = -1))
})

test_that("box-cox transform: construction and print", {
  trans <- outcome_box_cox_transform()
  expect_s3_class(trans, "outcome_box_cox_transform")
  expect_false(is_fitted_outcome_transform(trans))
  expect_snapshot(print(trans))

  expect_snapshot(error = TRUE, outcome_box_cox_transform(limits = c(2, -2)))
  expect_snapshot(error = TRUE, outcome_box_cox_transform(limits = c(-2, -2)))
  expect_snapshot(error = TRUE, outcome_box_cox_transform(limits = 1))
})

test_that("log transform: fit, apply, inverse round trip", {
  y <- mtcars$mpg
  fitted <- fit_outcome_transform(outcome_log_transform(), y)

  expect_true(is_fitted_outcome_transform(fitted))
  expect_equal(fitted$params$base, exp(1))
  expect_equal(fitted$params$shift, 0)

  molded <- mold_outcomes(outcome_log_transform(), y)
  expect_named(molded, c("outcomes", "transform"))
  expect_named(molded$outcomes, ".outcome")
  expect_equal(molded$outcomes$.outcome, log(y))

  back <- inverse_predictions(fitted, log(y))
  expect_named(back, ".pred")
  expect_equal(back$.pred, y)

  # apply / inverse generics agree with the bridge
  expect_equal(
    apply_outcome_transform(fitted, y),
    molded$outcomes$.outcome
  )
  expect_equal(
    inverse_outcome_transform(fitted, log(y)),
    y
  )
})

test_that("log transform: arbitrary base round trips", {
  y <- c(1, 5, 10, 100)
  for (base in c(2, 10, 7.5)) {
    fitted <- fit_outcome_transform(outcome_log_transform(base = base), y)
    expect_equal(apply_outcome_transform(fitted, y), log(y, base = base))
    expect_equal(
      inverse_outcome_transform(fitted, log(y, base = base)),
      y
    )
  }
})

test_that("box-cox: lambda is estimated and matches a grid reference", {
  set.seed(1)
  y <- rexp(80) + 0.5

  fitted <- fit_outcome_transform(outcome_box_cox_transform(), y)
  expect_true(is_fitted_outcome_transform(fitted))
  expect_length(fitted$params$lambda, 1)
  expect_true(is.finite(fitted$params$lambda))
  expect_gt(fitted$params$lambda, -2)
  expect_lt(fitted$params$lambda, 2)

  # grid-search profile log-likelihood reference
  ref_lambda <- function(x, limits = c(-2, 2)) {
    n <- length(x)
    lx <- log(x)
    ll <- function(lam) {
      z <- if (abs(lam) < 1e-8) log(x) else (x^lam - 1) / lam
      -n / 2 * log(stats::var(z)) + (lam - 1) * sum(lx)
    }
    grid <- seq(limits[1], limits[2], length.out = 2001)
    grid[which.max(vapply(grid, ll, numeric(1)))]
  }
  expect_equal(fitted$params$lambda, ref_lambda(y), tolerance = 0.01)

  # round trip
  molded <- mold_outcomes(outcome_box_cox_transform(), y)
  back <- inverse_predictions(molded$transform, molded$outcomes$.outcome)
  expect_equal(back$.pred, y, tolerance = 1e-8)
})

test_that("box-cox: fixed lambda is respected and lambda = 0 equals log", {
  y <- c(1, 2, 3, 4, 5)

  bc0 <- fit_outcome_transform(outcome_box_cox_transform(lambda = 0), y)
  expect_equal(bc0$params$lambda, 0)
  expect_equal(apply_outcome_transform(bc0, y), log(y))

  bc_half <- fit_outcome_transform(outcome_box_cox_transform(lambda = 0.5), y)
  expect_equal(
    apply_outcome_transform(bc_half, y),
    (sqrt(y) - 1) / 0.5
  )
  expect_equal(
    inverse_outcome_transform(bc_half, (sqrt(y) - 1) / 0.5),
    y
  )
})

test_that("box-cox: boundary lambda candidates are considered", {
  # A data set whose optimum sits at the search boundary
  y <- c(1, 2, 4, 8, 16, 32)^3
  tight <- outcome_box_cox_transform(limits = c(0, 0.1))
  fitted <- fit_outcome_transform(tight, y)
  expect_true(fitted$params$lambda >= 0 - 1e-8)
  expect_true(fitted$params$lambda <= 0.1 + 1e-8)
})

test_that("missing outcome values are handled explicitly and preserved", {
  y <- c(1, 2, NA, 4, 5)
  fitted <- fit_outcome_transform(outcome_log_transform(), y)

  molded <- mold_outcomes(fitted, y)
  expect_equal(is.na(molded$outcomes$.outcome), is.na(y))
  expect_equal(molded$outcomes$.outcome[1:2], log(y[1:2]))

  back <- inverse_predictions(fitted, molded$outcomes$.outcome)
  expect_equal(is.na(back$.pred), is.na(y))
  expect_equal(back$.pred, y)

  # parameters come from the complete cases only, without error
  full <- fit_outcome_transform(outcome_log_transform(), c(1, 2, 4, 5))
  expect_equal(fitted$params, full$params)
})

test_that("all-missing outcomes cannot be fit", {
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(outcome_log_transform(), c(NA_real_, NA_real_))
  )
})

test_that("out-of-domain outcomes raise explicit errors", {
  # zero without shift (log)
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(outcome_log_transform(), c(1, 0, 2))
  )
  # negatives without shift (box-cox)
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(outcome_box_cox_transform(), c(1, -3, 2))
  )
})

test_that("shift must be supplied explicitly and be large enough", {
  y <- c(0, 1, 2, 3)

  # an explicit, sufficient shift works and round trips
  fitted <- fit_outcome_transform(outcome_log_transform(shift = 1), y)
  expect_equal(fitted$params$shift, 1)
  molded <- mold_outcomes(fitted, y)
  expect_equal(molded$outcomes$.outcome, log(y + 1))
  expect_equal(
    inverse_predictions(fitted, molded$outcomes$.outcome)$.pred,
    y
  )

  # insufficient shift is an error
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(outcome_log_transform(shift = 1), c(-5, 2, 3))
  )

  # negative shift is an error
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(outcome_log_transform(shift = -2), y)
  )

  # box-cox with shift round trips
  bc <- fit_outcome_transform(outcome_box_cox_transform(shift = 1), y)
  molded_bc <- mold_outcomes(bc, y)
  expect_equal(
    inverse_predictions(bc, molded_bc$outcomes$.outcome)$.pred,
    y,
    tolerance = 1e-8
  )
})

test_that("fitted transforms are frozen and never re-estimated", {
  fitted <- fit_outcome_transform(outcome_box_cox_transform(), mtcars$mpg)
  lambda <- fitted$params$lambda

  # refitting a fitted transform is rejected
  expect_snapshot(error = TRUE, {
    fit_outcome_transform(fitted, mtcars$disp)
  })

  # mold_outcomes() on a fitted transform reuses the frozen parameters
  molded <- mold_outcomes(fitted, mtcars$disp)
  expect_identical(molded$transform$params$lambda, lambda)
  expect_identical(molded$transform, fitted)

  # unfitted transforms cannot be applied or inverted
  unfitted <- outcome_log_transform()
  expect_snapshot(error = TRUE, apply_outcome_transform(unfitted, 1:3))
  expect_snapshot(error = TRUE, inverse_outcome_transform(unfitted, 1:3))
  expect_snapshot(error = TRUE, inverse_predictions(unfitted, 1:3))
})

test_that("quantile predictions keep their levels and back-transform values", {
  fitted <- fit_outcome_transform(outcome_log_transform(), c(10, 20, 30, 40))
  levels <- c(.1, .25, .5, .75, .9)
  raw <- matrix(log(c(10, 20, 30, 40, 50, 12, 22, 32, 42, 52)), nrow = 2)
  qp <- quantile_pred(raw, levels)

  back <- inverse_outcome_transform(fitted, qp)
  expect_true(is_quantile_pred(back))
  expect_identical(extract_quantile_levels(back), levels)
  expect_equal(as.matrix(back), exp(raw))

  # the bridge returns the standard spruce-style column
  out <- inverse_predictions(fitted, qp)
  expect_named(out, ".pred_quantile")
  expect_identical(
    extract_quantile_levels(out$.pred_quantile),
    levels
  )

  # forward direction also preserves levels
  fwd <- apply_outcome_transform(fitted, quantile_pred(exp(raw), levels))
  expect_s3_class(fwd, "quantile_pred")
  expect_identical(extract_quantile_levels(fwd), levels)
  expect_equal(as.matrix(fwd), raw)
})

test_that("quantile back-transform is elementwise and NAs stay in place", {
  fitted <- fit_outcome_transform(outcome_log_transform(), 1:10 + 1)
  levels <- c(.2, .5, .8)
  raw <- matrix(c(log(2), NA, log(4), log(3), log(4), log(5)), nrow = 2)
  qp <- quantile_pred(raw, levels)

  back <- inverse_outcome_transform(fitted, qp)
  expect_equal(is.na(as.matrix(back)), is.na(raw))
  expect_equal(as.matrix(back)[!is.na(raw)], exp(raw)[!is.na(raw)])
  expect_identical(extract_quantile_levels(back), levels)
})

test_that("box-cox inverse domain is validated, including in quantile_pred", {
  bc <- fit_outcome_transform(
    outcome_box_cox_transform(lambda = 0.5),
    c(1, 2, 3, 4)
  )

  expect_snapshot(error = TRUE, inverse_outcome_transform(bc, -3))

  qp <- quantile_pred(matrix(c(-3, 0, 1), nrow = 1), c(.1, .5, .9))
  expect_snapshot(error = TRUE, inverse_outcome_transform(bc, qp))

  # in-range values still work, including NA mixed in
  v <- inverse_outcome_transform(bc, c(NA_real_, 0, 1))
  expect_true(is.na(v[1]))
  expect_equal(v[2], 1)
})

test_that("zero-variance outcomes cannot select a box-cox lambda", {
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(outcome_box_cox_transform(), rep(5, 8))
  )
  # a fixed lambda needs no estimation, so it still works
  fixed <- fit_outcome_transform(
    outcome_box_cox_transform(lambda = 1),
    rep(5, 8)
  )
  expect_equal(fixed$params$lambda, 1)
})

test_that("non-numeric and multivariate outcomes are rejected", {
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(outcome_log_transform(), factor(c("a", "b")))
  )
  expect_snapshot(
    error = TRUE,
    fit_outcome_transform(
      outcome_log_transform(),
      mtcars[1:5, c("mpg", "disp")]
    )
  )
  expect_snapshot(
    error = TRUE,
    inverse_predictions(
      fit_outcome_transform(outcome_log_transform(), mtcars$mpg),
      matrix(1:4, ncol = 2)
    )
  )
})

test_that("transforms survive saveRDS/loadRDS and behave identically", {
  fitted <- fit_outcome_transform(outcome_box_cox_transform(), mtcars$mpg)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  saveRDS(fitted, path)
  loaded <- readRDS(path)

  expect_identical(loaded, fitted)
  preds <- c(2.5, 3, 3.5)
  expect_identical(
    inverse_predictions(loaded, preds),
    inverse_predictions(fitted, preds)
  )

  # quantile version survives reload as well
  qp <- quantile_pred(matrix(preds, nrow = 1), c(.1, .5, .9))
  expect_identical(
    inverse_predictions(loaded, qp),
    inverse_predictions(fitted, qp)
  )
})

test_that("data frame and 1-column matrix outcomes are accepted", {
  y_df <- data.frame(y = c(1, 2, 3, 4))
  fitted <- fit_outcome_transform(outcome_log_transform(), y_df)
  expect_equal(apply_outcome_transform(fitted, y_df$y), log(y_df$y))

  y_mat <- matrix(c(1, 2, 3, 4), ncol = 1)
  fitted2 <- fit_outcome_transform(outcome_log_transform(), y_mat)
  expect_equal(apply_outcome_transform(fitted2, c(1, 2, 3, 4)), log(c(1, 2, 3, 4)))
})

test_that("mold_outcomes output is the transformed real training target", {
  y <- c(2, 4, 8)
  result <- mold_outcomes(outcome_log_transform(base = 2), y)

  expect_true(tibble::is_tibble(result$outcomes))
  expect_equal(nrow(result$outcomes), length(y))
  expect_equal(result$outcomes$.outcome, log(y, base = 2))
  expect_true(is_fitted_outcome_transform(result$transform))
})

test_that("inverse_predictions validates prediction size", {
  fitted <- fit_outcome_transform(outcome_log_transform(), mtcars$mpg)
  out <- inverse_predictions(fitted, 1:5)
  expect_equal(nrow(out), 5)
  expect_invisible(validate_prediction_size(out, data.frame(x = 1:5)))
})
