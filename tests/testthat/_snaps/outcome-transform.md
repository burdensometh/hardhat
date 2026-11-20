# log transform: construction and print

    Code
      print(trans)
    Message
      <outcome transform: log>
      Fitted: FALSE
      Base: 2.71828182845905

---

    Code
      outcome_log_transform(base = 1)
    Condition
      Error in `outcome_log_transform()`:
      ! `base` must not be 1.

---

    Code
      outcome_log_transform(base = -2)
    Condition
      Error in `outcome_log_transform()`:
      ! `base` must be a number larger than or equal to 2.2250738585072e-308, not the number -2.

---

    Code
      outcome_log_transform(shift = -1)
    Condition
      Error in `outcome_log_transform()`:
      ! `shift` must be a number larger than or equal to 0 or `NULL`, not the number -1.

# box-cox transform: construction and print

    Code
      print(trans)
    Message
      <outcome transform: box_cox>
      Fitted: FALSE
      Lambda: <to be estimated>
      Search limits: [-2, 2]

---

    Code
      outcome_box_cox_transform(limits = c(2, -2))
    Condition
      Error in `outcome_box_cox_transform()`:
      ! `limits` must be strictly increasing, not 2 and -2.

---

    Code
      outcome_box_cox_transform(limits = c(-2, -2))
    Condition
      Error in `outcome_box_cox_transform()`:
      ! `limits` must be strictly increasing, not -2 and -2.

---

    Code
      outcome_box_cox_transform(limits = 1)
    Condition
      Error in `outcome_box_cox_transform()`:
      ! `limits` must be a length-2 numeric vector.

# all-missing outcomes cannot be fit

    Code
      fit_outcome_transform(outcome_log_transform(), c(NA_real_, NA_real_))
    Condition
      Error:
      ! Cannot fit an outcome transform when every outcome value is missing.

# out-of-domain outcomes raise explicit errors

    Code
      fit_outcome_transform(outcome_log_transform(), c(1, 0, 2))
    Condition
      Error:
      ! The log outcome transform requires strictly positive values, but the smallest non-missing outcome is 0.
      i Supply a `shift` strictly greater than 0 so that `outcome + shift` is strictly positive before logging.

---

    Code
      fit_outcome_transform(outcome_box_cox_transform(), c(1, -3, 2))
    Condition
      Error:
      ! The Box-Cox outcome transform requires strictly positive values, but the smallest non-missing outcome is -3.
      i Supply a `shift` strictly greater than 3 so that `outcome + shift` is strictly positive before the Box-Cox transformation.

# shift must be supplied explicitly and be large enough

    Code
      fit_outcome_transform(outcome_log_transform(shift = 1), c(-5, 2, 3))
    Condition
      Error:
      ! The supplied `shift` (1) does not make the outcome strictly positive.
      i The smallest outcome value is -5, so the shift must be strictly greater than 5.

---

    Code
      fit_outcome_transform(outcome_log_transform(shift = -2), y)
    Condition
      Error in `outcome_log_transform()`:
      ! `shift` must be a number larger than or equal to 0 or `NULL`, not the number -2.

# fitted transforms are frozen and never re-estimated

    Code
      fit_outcome_transform(fitted, mtcars$disp)
    Condition
      Error:
      ! The outcome transform has already been fitted.
      i Transform parameters are estimated once, from the training outcome. They cannot be re-estimated from new data.

---

    Code
      apply_outcome_transform(unfitted, 1:3)
    Condition
      Error:
      ! `transform` has not been fitted to a training outcome yet.
      i Call `fit_outcome_transform()` (or `mold_outcomes()`) first.

---

    Code
      inverse_outcome_transform(unfitted, 1:3)
    Condition
      Error:
      ! `transform` has not been fitted to a training outcome yet.
      i Call `fit_outcome_transform()` (or `mold_outcomes()`) first.

---

    Code
      inverse_predictions(unfitted, 1:3)
    Condition
      Error:
      ! `transform` has not been fitted to a training outcome yet.
      i Call `fit_outcome_transform()` (or `mold_outcomes()`) first.

# box-cox inverse domain is validated, including in quantile_pred

    Code
      inverse_outcome_transform(bc, -3)
    Condition
      Error:
      ! The Box-Cox inverse transform requires `lambda * x + 1 > 0`.
      i 1 non-missing predicted value was outside the domain of the fitted transform (lambda = 0.5). These predictions cannot correspond to any real-valued outcome on the original scale.

---

    Code
      inverse_outcome_transform(bc, qp)
    Condition
      Error:
      ! The Box-Cox inverse transform requires `lambda * x + 1 > 0`.
      i 1 non-missing predicted value was outside the domain of the fitted transform (lambda = 0.5). These predictions cannot correspond to any real-valued outcome on the original scale.

# zero-variance outcomes cannot select a box-cox lambda

    Code
      fit_outcome_transform(outcome_box_cox_transform(), rep(5, 8))
    Condition
      Error:
      ! The Box-Cox `lambda` could not be estimated because the outcome has (near) zero variance after transformation.

# non-numeric and multivariate outcomes are rejected

    Code
      fit_outcome_transform(outcome_log_transform(), factor(c("a", "b")))
    Condition
      Error:
      ! `outcome` must be a numeric outcome, not a <factor> object.

---

    Code
      fit_outcome_transform(outcome_log_transform(), mtcars[1:5, c("mpg", "disp")])
    Condition
      Error:
      ! Outcome transformations require a univariate outcome, but 2 outcome columns were found.

---

    Code
      inverse_predictions(fit_outcome_transform(outcome_log_transform(), mtcars$mpg),
      matrix(1:4, ncol = 2))
    Condition
      Error:
      ! `predictions` must be a numeric vector or a <quantile_pred>, not an integer matrix.

