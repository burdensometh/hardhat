# The modeling package templates only contain `{{model}}` placeholders, so a
# fixed `gsub()` renders them exactly like `use_modeling_files()` does with
# whisker. Rendering + evaluating them here lets us test the generated Poisson
# regression bridge without requiring usethis or a full package build.
source_hardhat_templates <- function(model, env) {
  templates <- c("constructor.R", "fit.R", "predict.R")

  for (template in templates) {
    path <- system.file("templates", "R", template, package = "hardhat")
    code <- readChar(path, nchars = file.size(path))
    code <- gsub("{{model}}", model, code, fixed = TRUE)
    eval(parse(text = code), envir = env)
  }

  invisible(env)
}

test_that("generated model fits a poisson regression with `glm.fit()`", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(100, lambda = 2),
    x = runif(100),
    exposure = runif(100, min = 0.5, max = 2)
  )

  fit <- poisson_regression(y ~ x + offset(log(exposure)), df)

  expect_s3_class(fit, "poisson_regression")
  expect_s3_class(fit, "hardhat_model")

  # The engine is the mature GLM fitter, fed with the molded predictors and
  # the offset from the `extras`
  processed <- mold(y ~ x + offset(log(exposure)), df)
  glm_fit <- glm.fit(
    x = as.matrix(processed$predictors),
    y = df$y,
    family = poisson(),
    offset = log(df$exposure)
  )

  expect_identical(fit$coefs, glm_fit$coefficients)
})

test_that("generated model never learns the offset as a coefficient", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(100, lambda = 2),
    x = runif(100),
    exposure = runif(100, min = 0.5, max = 2)
  )

  fit <- poisson_regression(y ~ x + offset(log(exposure)), df)
  processed <- mold(y ~ x + offset(log(exposure)), df)

  # One coefficient per molded predictor column, and nothing for the offset
  expect_length(fit$coefs, ncol(processed$predictors))
  expect_identical(names(fit$coefs), colnames(as.matrix(processed$predictors)))
  expect_false(any(grepl("offset", names(fit$coefs), fixed = TRUE)))
})

test_that("generated model sums multiple offsets along `mold()` semantics", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(100, lambda = 2),
    x = runif(100),
    a = runif(100),
    b = runif(100)
  )

  fit <- poisson_regression(y ~ x + offset(a) + offset(b), df)

  processed <- mold(y ~ x + offset(a) + offset(b), df)
  glm_fit <- glm.fit(
    x = as.matrix(processed$predictors),
    y = df$y,
    family = poisson(),
    offset = df$a + df$b
  )

  expect_identical(fit$coefs, glm_fit$coefficients)
})

test_that("generated model restores the response scale with the new data's own offset", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(100, lambda = 2),
    x = runif(100),
    exposure = runif(100, min = 0.5, max = 2)
  )

  fit <- poisson_regression(y ~ x + offset(log(exposure)), df)

  new_data <- data.frame(x = c(0.1, 0.9), exposure = c(1, 4))
  pred <- predict(fit, new_data)

  # Linear predictor from the new data, response scale restored with the
  # new samples' own exposure
  forged <- forge(new_data, fit$blueprint)
  eta <- as.vector(as.matrix(forged$predictors) %*% fit$coefs) +
    log(new_data$exposure)

  expect_identical(pred$.pred, exp(eta))

  # The offset is used in the computation, not tacked on next to the result
  expect_named(pred, ".pred")

  # With a log link, doubling the exposure doubles the expected count
  pred_double <- predict(fit, transform(new_data, exposure = exposure * 2))
  expect_equal(pred_double$.pred, pred$.pred * 2)

  # The training offset is not reused at prediction time
  pred_train_offset <- exp(
    as.vector(as.matrix(forged$predictors) %*% fit$coefs) +
      log(df$exposure[seq_len(nrow(new_data))])
  )
  expect_false(isTRUE(all.equal(pred$.pred, pred_train_offset)))
})

test_that("generated model rejects missing or non-finite offsets at fit time", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(20, lambda = 2),
    x = runif(20),
    exposure = runif(20, min = 0.5, max = 2)
  )

  df_na <- df
  df_na$exposure[1] <- NA
  expect_error(
    poisson_regression(y ~ x + offset(exposure), df_na),
    "missing or non-finite"
  )

  df_nan <- df
  df_nan$exposure[1] <- NaN
  expect_error(
    poisson_regression(y ~ x + offset(exposure), df_nan),
    "missing or non-finite"
  )

  df_inf <- df
  df_inf$exposure[1] <- Inf
  expect_error(
    poisson_regression(y ~ x + offset(exposure), df_inf),
    "missing or non-finite"
  )

  df_neg_inf <- df
  df_neg_inf$exposure[1] <- -Inf
  expect_error(
    poisson_regression(y ~ x + offset(exposure), df_neg_inf),
    "missing or non-finite"
  )
})

test_that("generated model rejects missing or non-finite offsets at predict time", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(20, lambda = 2),
    x = runif(20),
    exposure = runif(20, min = 0.5, max = 2)
  )

  fit <- poisson_regression(y ~ x + offset(exposure), df)

  new_data <- data.frame(x = 0.5, exposure = NA_real_)
  expect_error(predict(fit, new_data), "missing or non-finite")

  new_data <- data.frame(x = 0.5, exposure = Inf)
  expect_error(predict(fit, new_data), "missing or non-finite")

  new_data <- data.frame(x = 0.5, exposure = NaN)
  expect_error(predict(fit, new_data), "missing or non-finite")
})

test_that("generated model is consistent after saving and reloading", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(100, lambda = 2),
    x = runif(100),
    exposure = runif(100, min = 0.5, max = 2)
  )

  fit <- poisson_regression(y ~ x + offset(log(exposure)), df)

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fit, path)
  reloaded <- readRDS(path)

  new_data <- data.frame(x = c(0.1, 0.9), exposure = c(1, 4))

  expect_identical(
    predict(reloaded, new_data),
    predict(fit, new_data)
  )
})

test_that("generated model predicts on zero rows", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(20, lambda = 2),
    x = runif(20),
    exposure = runif(20, min = 0.5, max = 2)
  )

  fit <- poisson_regression(y ~ x + offset(log(exposure)), df)

  pred <- predict(fit, df[0, ])

  expect_s3_class(pred, "tbl_df")
  expect_named(pred, ".pred")
  expect_equal(nrow(pred), 0)
  expect_identical(pred$.pred, numeric())
})

test_that("generated model works without an offset", {
  source_hardhat_templates("poisson_regression", environment())

  set.seed(123)
  df <- data.frame(
    y = rpois(100, lambda = 2),
    x = runif(100)
  )

  # XY interface
  fit_xy <- poisson_regression(df["x"], df$y)

  processed <- mold(df["x"], df$y)
  glm_fit <- glm.fit(
    x = as.matrix(processed$predictors),
    y = df$y,
    family = poisson()
  )

  expect_identical(fit_xy$coefs, glm_fit$coefficients)

  pred <- predict(fit_xy, df["x"])
  expect_equal(nrow(pred), nrow(df))

  # Formula interface without an offset
  fit_formula <- poisson_regression(y ~ x, df)
  expect_identical(fit_formula$coefs, glm_fit$coefficients)
})
