test_that("default text blueprint has sensible defaults", {
  bp <- default_text_blueprint()

  expect_s3_class(bp, "default_text_blueprint")
  expect_s3_class(bp, "text_blueprint")
  expect_s3_class(bp, "xy_blueprint")
  expect_s3_class(bp, "hardhat_blueprint")

  expect_equal(bp$composition, "dgCMatrix")
  expect_true(is.function(bp$tokenizer))
  expect_null(bp$text_columns)
  expect_equal(bp$numeric_columns, character())
  expect_equal(bp$min_times, 1L)
  expect_null(bp$max_tokens)
  expect_true(bp$smooth_idf)
  expect_equal(bp$prefix, "tfidf")
  expect_equal(bp$sep, "__")
  expect_null(bp$text_results)
})

test_that("text blueprint validates its arguments", {
  expect_snapshot(error = TRUE, default_text_blueprint(min_times = -1L))
  expect_snapshot(error = TRUE, default_text_blueprint(max_tokens = 0L))
  expect_snapshot(error = TRUE, default_text_blueprint(tokenizer = "not a function"))
  expect_snapshot(error = TRUE, default_text_blueprint(prefix = ""))
  expect_snapshot(error = TRUE, default_text_blueprint(sep = ""))
  expect_snapshot(error = TRUE, default_text_blueprint(composition = "nope"))
})

test_that("blueprint refresh keeps all text fields", {
  bp <- default_text_blueprint(min_times = 2L, smooth_idf = FALSE)
  refreshed <- refresh_blueprint(bp)

  expect_s3_class(refreshed, "text_blueprint")
  expect_equal(refreshed$min_times, 2L)
  expect_false(refreshed$smooth_idf)
  expect_true(is.function(refreshed$tokenizer))

  updated <- update_blueprint(bp, smooth_idf = FALSE)
  expect_false(updated$smooth_idf)
  expect_equal(updated$min_times, 2L)
})

test_that("mold with text blueprint produces sparse TF-IDF predictors", {
  train <- data.frame(
    text = c("the cat sat", "the dog ran", NA, ""),
    x = c(1, 2, 3, 4)
  )
  y <- c(1, 0, 1, 0)

  out <- mold(train, y, blueprint = default_text_blueprint())

  expect_s4_class(out$predictors, "dgCMatrix")
  expect_equal(nrow(out$predictors), 4L)

  cnames <- colnames(out$predictors)
  # Numeric auxiliary column comes first and is preserved
  expect_equal(cnames[[1]], "x")
  # Source boundary is kept in feature names
  expect_true(all(grepl("^tfidf__text__", cnames[-1])))
  expect_true("tfidf__text__the" %in% cnames)
  expect_true("tfidf__text__cat" %in% cnames)
  expect_snapshot(print(out$blueprint))
})

test_that("multiple text columns keep their source boundary", {
  train <- data.frame(
    text1 = c("shared word", "only first"),
    text2 = c("shared word", "only second"),
    n = c(1, 2)
  )

  out <- mold(train, y = c(0, 1), blueprint = default_text_blueprint())
  cnames <- colnames(out$predictors)

  expect_true("tfidf__text1__shared" %in% cnames)
  expect_true("tfidf__text2__shared" %in% cnames)
  expect_equal(anyDuplicated(cnames), 0L)
  expect_equal(out$blueprint$text_columns, c("text1", "text2"))
  expect_equal(out$blueprint$numeric_columns, "n")
})

test_that("TF-IDF values match the documented formula", {
  d <- data.frame(
    txt = c("a a b", "b c", "c a a a"),
    n = c(1, 2, 3)
  )

  out <- mold(d, y = c(0, 1, 0), blueprint = default_text_blueprint())
  mat <- unname(as.matrix(out$predictors))
  cols <- colnames(out$predictors)
  col <- function(name) which(cols == name)

  idf <- log(4 / 3) + 1 # df = 2 for each of a, b, c; n = 3

  expect_equal(mat[1, col("tfidf__txt__a")], 2 * idf)
  expect_equal(mat[1, col("tfidf__txt__b")], 1 * idf)
  expect_equal(mat[2, col("tfidf__txt__c")], 1 * idf)
  expect_equal(mat[3, col("tfidf__txt__a")], 3 * idf)
  expect_equal(mat[, col("n")], c(1, 2, 3))

  # Classic (unsmoothed) IDF
  out_classic <- mold(
    d,
    y = c(0, 1, 0),
    blueprint = default_text_blueprint(smooth_idf = FALSE)
  )
  mat_classic <- unname(as.matrix(out_classic$predictors))
  expect_equal(
    mat_classic[1, col("tfidf__txt__a")],
    2 * log(3 / 2)
  )
})

test_that("missing and empty text are distinct but both yield zero rows", {
  train <- data.frame(
    text = c("a b", "", NA, "   "),
    n = c(1, 2, 3, 4)
  )

  out <- mold(train, y = c(0, 1, 0, 1), blueprint = default_text_blueprint())

  expect_equal(
    out$extras$missing_text$text,
    c(FALSE, FALSE, TRUE, FALSE)
  )

  mat <- as.matrix(out$predictors)
  tfidf_cols <- grepl("tfidf__", colnames(mat))

  # Empty string, whitespace-only, and missing all produce zero TF-IDF rows
  expect_true(all(mat[2, tfidf_cols] == 0))
  expect_true(all(mat[3, tfidf_cols] == 0))
  expect_true(all(mat[4, tfidf_cols] == 0))

  # Numeric auxiliary values survive regardless of text state
  expect_equal(mat[, "n"], c(1, 2, 3, 4))
})

test_that("forge reuses frozen vocabulary and never expands columns", {
  train <- data.frame(
    text = c("the cat", "the dog"),
    n = c(1, 2)
  )

  out <- mold(train, y = c(0, 1), blueprint = default_text_blueprint())

  new_data <- data.frame(
    text = c("the cat", "brand new unseen words", NA, ""),
    n = c(3, 4, 5, 6)
  )

  forged <- forge(new_data, out$blueprint)

  expect_s4_class(forged$predictors, "dgCMatrix")
  expect_equal(
    colnames(forged$predictors),
    colnames(out$predictors)
  )
  expect_equal(nrow(forged$predictors), 4L)
  expect_equal(
    forged$extras$missing_text$text,
    c(FALSE, FALSE, TRUE, FALSE)
  )

  # The all-unseen row has zero TF-IDF values but retains its numeric value
  mat <- as.matrix(forged$predictors)
  unseen_row <- mat[2, grepl("tfidf__", colnames(mat))]
  expect_true(all(unseen_row == 0))
  expect_equal(unname(mat[2, "n"]), 4)
})

test_that("forge on identical data reproduces mold output", {
  train <- data.frame(
    text = c("a a b", "b c", "c a"),
    n = c(1, 2, 3)
  )

  out <- mold(train, y = c(0, 1, 0), blueprint = default_text_blueprint())
  forged <- forge(train, out$blueprint)

  expect_equal(
    unname(as.matrix(forged$predictors)),
    unname(as.matrix(out$predictors))
  )
  expect_equal(
    colnames(forged$predictors),
    colnames(out$predictors)
  )
})

test_that("saved and reloaded blueprint gives identical results", {
  train <- data.frame(
    text = c("the cat sat", "the dog ran"),
    n = c(1, 2)
  )

  out <- mold(train, y = c(0, 1), blueprint = default_text_blueprint())

  path <- tempfile(fileext = ".rds")
  saveRDS(out$blueprint, path)
  reloaded <- readRDS(path)

  new_data <- data.frame(
    text = c("the cat", "unseen terms"),
    n = c(3, 4)
  )

  f1 <- forge(new_data, out$blueprint)$predictors
  f2 <- forge(new_data, reloaded)$predictors

  expect_equal(colnames(f2), colnames(f1))
  expect_equal(unname(as.matrix(f2)), unname(as.matrix(f1)))
})

test_that("min_times filters rare terms", {
  train <- data.frame(text = c("common rare", "common", "common other"))

  out <- mold(
    train,
    y = c(0, 1, 0),
    blueprint = default_text_blueprint(min_times = 2L)
  )

  cnames <- colnames(out$predictors)
  expect_true("tfidf__text__common" %in% cnames)
  expect_false("tfidf__text__rare" %in% cnames)
  expect_false("tfidf__text__other" %in% cnames)
})

test_that("max_tokens caps vocabulary by document frequency", {
  train <- data.frame(text = c("a b c", "a b", "a"))

  out <- mold(
    train,
    y = c(0, 1, 0),
    blueprint = default_text_blueprint(max_tokens = 2L)
  )

  tfidf_cols <- grep("tfidf__", colnames(out$predictors), value = TRUE)
  expect_length(tfidf_cols, 2L)
  # `a` (df 3) and `b` (df 2) outrank `c` (df 1)
  expect_true("tfidf__text__a" %in% tfidf_cols)
  expect_true("tfidf__text__b" %in% tfidf_cols)
})

test_that("custom tokenizer is honored and frozen", {
  hyphen_tokenizer <- function(x) {
    strsplit(tolower(x), "-", fixed = TRUE)
  }

  d <- data.frame(txt = c("a-b", "b-c", "a-c"), n = 1:3)

  out <- mold(
    d,
    y = c(0, 1, 0),
    blueprint = default_text_blueprint(tokenizer = hyphen_tokenizer)
  )

  cnames <- colnames(out$predictors)
  expect_true("tfidf__txt__a" %in% cnames)
  expect_true("tfidf__txt__b" %in% cnames)
  expect_true("tfidf__txt__c" %in% cnames)

  forged <- forge(d, out$blueprint)
  expect_equal(
    colnames(forged$predictors),
    cnames
  )
})

test_that("a failing tokenizer errors with the column named", {
  bad_tokenizer <- function(x) {
    stop("boom")
  }

  d <- data.frame(txt = c("a b"), n = 1)

  expect_snapshot(error = TRUE, {
    mold(d, y = 0, blueprint = default_text_blueprint(tokenizer = bad_tokenizer))
  })
})

test_that("tokenizer returning wrong shape errors", {
  bad_tokenizer <- function(x) {
    c("a", "b")
  }

  d <- data.frame(txt = c("a b", "c d"))

  expect_snapshot(error = TRUE, {
    mold(d, y = c(0, 1), blueprint = default_text_blueprint(tokenizer = bad_tokenizer))
  })
})

test_that("explicit text_columns are respected", {
  train <- data.frame(
    text = c("a b", "b c"),
    n = c(1, 2)
  )

  out <- mold(
    train,
    y = c(0, 1),
    blueprint = default_text_blueprint(text_columns = "text")
  )

  expect_equal(out$blueprint$text_columns, "text")
  expect_equal(out$blueprint$numeric_columns, "n")
})

test_that("undeclared and non-existent text columns error clearly", {
  train <- data.frame(
    text = c("a", "b"),
    other = c("c", "d"),
    n = c(1, 2)
  )

  expect_snapshot(error = TRUE, {
    mold(
      train,
      y = c(0, 1),
      blueprint = default_text_blueprint(text_columns = "text")
    )
  })

  expect_snapshot(error = TRUE, {
    mold(
      train,
      y = c(0, 1),
      blueprint = default_text_blueprint(text_columns = "missing")
    )
  })
})

test_that("non-numeric non-text predictor columns error", {
  train <- data.frame(
    text = c("a", "b"),
    f = factor(c("x", "y"))
  )

  expect_snapshot(error = TRUE, {
    mold(train, y = c(0, 1), blueprint = default_text_blueprint())
  })
})

test_that("all three compositions produce consistent columns", {
  train <- data.frame(text = c("a b", "b c"), n = c(1, 2))

  sparse <- mold(
    train,
    y = c(0, 1),
    blueprint = default_text_blueprint(composition = "dgCMatrix")
  )
  mat <- mold(
    train,
    y = c(0, 1),
    blueprint = default_text_blueprint(composition = "matrix")
  )
  tbl <- mold(
    train,
    y = c(0, 1),
    blueprint = default_text_blueprint(composition = "tibble")
  )

  expect_s4_class(sparse$predictors, "dgCMatrix")
  expect_true(is.matrix(mat$predictors))
  expect_s3_class(tbl$predictors, "tbl_df")

  expected <- colnames(sparse$predictors)
  expect_equal(colnames(mat$predictors), expected)
  expect_equal(colnames(tbl$predictors), expected)
})

test_that("intercept is added first for every composition", {
  train <- data.frame(text = c("a b", "b c"), n = c(1, 2))

  out <- mold(
    train,
    y = c(0, 1),
    blueprint = default_text_blueprint(intercept = TRUE)
  )

  expect_equal(colnames(out$predictors)[[1]], "(Intercept)")

  forged <- forge(train, out$blueprint)
  expect_equal(colnames(forged$predictors)[[1]], "(Intercept)")
  expect_equal(ncol(forged$predictors), ncol(out$predictors))
})

test_that("data with no text or no numeric columns is handled", {
  text_only <- data.frame(t1 = c("a b", "b c"), t2 = c("x", "y x"))
  out_text <- mold(text_only, y = c(0, 1), blueprint = default_text_blueprint())
  expect_equal(out_text$blueprint$numeric_columns, character())
  expect_length(out_text$blueprint$text_columns, 2L)
  forged_text <- forge(text_only, out_text$blueprint)$predictors
  expect_equal(ncol(forged_text), ncol(out_text$predictors))

  numeric_only <- data.frame(x = 1:3, z = 4:6)
  out_num <- mold(numeric_only, y = c(0, 1, 0), blueprint = default_text_blueprint())
  expect_equal(out_num$blueprint$text_columns, character())
  expect_equal(colnames(out_num$predictors), c("x", "z"))
  forged_num <- forge(numeric_only, out_num$blueprint)$predictors
  expect_equal(colnames(forged_num), c("x", "z"))
})

test_that("forge validates missing columns", {
  train <- data.frame(text = c("a b", "b c"), n = c(1, 2))
  out <- mold(train, y = c(0, 1), blueprint = default_text_blueprint())

  expect_snapshot(error = TRUE, {
    forge(data.frame(n = c(1, 2)), out$blueprint)
  })
})

test_that("feature names colliding with numeric columns error clearly", {
  train <- data.frame(
    `tfidf__text__a` = c(1, 2),
    text = c("a b", "a c"),
    check.names = FALSE
  )

  expect_snapshot(error = TRUE, {
    mold(train, y = c(0, 1), blueprint = default_text_blueprint())
  })

  # A different prefix avoids the collision
  out <- mold(
    train,
    y = c(0, 1),
    blueprint = default_text_blueprint(prefix = "txtfeat")
  )
  expect_equal(anyDuplicated(colnames(out$predictors)), 0L)
})

test_that("outcomes are supported through the text blueprint", {
  train <- data.frame(text = c("a b", "b c"), n = c(1, 2), y = c(0, 1))

  out <- mold(
    train[c("text", "n")],
    train["y"],
    blueprint = default_text_blueprint()
  )

  forged <- forge(train, out$blueprint, outcomes = TRUE)
  expect_s3_class(forged$outcomes, "tbl_df")
  expect_equal(colnames(forged$outcomes), "y")
})

test_that("a vector outcome uses the standardized .outcome name", {
  train <- data.frame(text = c("a b", "b c"), n = c(1, 2))

  out <- mold(train, c(0, 1), blueprint = default_text_blueprint())
  expect_equal(colnames(out$outcomes), ".outcome")
})
