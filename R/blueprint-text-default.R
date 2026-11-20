# Default text blueprint: mold() and forge() implementations
#
# This blueprint subclasses the XY blueprint, so outcomes are handled exactly
# like `default_xy_blueprint()`. The predictor side adds TF-IDF generation for
# character columns and combines the result with numeric auxiliary columns.

#' @rdname run-mold
#' @export
run_mold.default_text_blueprint <- function(
  blueprint,
  ...,
  x,
  y,
  call = caller_env()
) {
  check_dots_empty0(...)

  cleaned <- mold_text_default_clean(
    blueprint = blueprint,
    x = x,
    y = y,
    call = call
  )

  blueprint <- cleaned$blueprint
  x <- cleaned$x
  y <- cleaned$y

  mold_text_default_process(blueprint = blueprint, x = x, y = y, call = call)
}

# ------------------------------------------------------------------------------
# mold - clean

mold_text_default_clean <- function(blueprint, x, y, ..., call = caller_env()) {
  check_dots_empty0(...)

  check_data_frame_or_matrix(x, call = call)
  x <- coerce_to_tibble(x)
  check_unique_column_names(x)

  blueprint <- resolve_text_columns(blueprint, x, call = call)

  if (is.null(y)) {
    # Special case `y = NULL` as a 0 column variation on `x`
    y <- x[0L]
  }

  y <- standardize(y)

  list(blueprint = blueprint, x = x, y = y)
}

# Determine which predictor columns are text columns and validate the split
# against the blueprint declaration. Every non-text predictor column must be
# numeric, since the final predictor set is a numeric (sparse) matrix.
resolve_text_columns <- function(blueprint, x, ..., call = caller_env()) {
  check_dots_empty0(...)

  declared <- blueprint$text_columns
  all_cols <- names(x)

  if (is.null(declared)) {
    text_cols <- all_cols[map_lgl(x, is.character)]
  } else {
    missing_cols <- setdiff(declared, all_cols)
    if (length(missing_cols) > 0L) {
      cli::cli_abort(
        c(
          "The following text column{?s} {?was/were} not found in {.arg x}:
           {.val {missing_cols}}."
        ),
        call = call
      )
    }

    non_character <- declared[!map_lgl(x[declared], is.character)]
    if (length(non_character) > 0L) {
      cli::cli_abort(
        c(
          "Every text column must be a character column.",
          i = "The following {?is/are} not character: {.val {non_character}}."
        ),
        call = call
      )
    }

    text_cols <- declared
  }

  undeclared_character <- setdiff(
    all_cols[map_lgl(x, is.character)],
    text_cols
  )
  if (length(undeclared_character) > 0L) {
    cli::cli_abort(
      c(
        "All character columns must either be declared as text columns or
         removed from {.arg x}.",
        i = "The following character column{?s} {?was/were} not declared in
             {.arg text_columns}: {.val {undeclared_character}}.",
        i = "Set {.arg text_columns = NULL} to treat every character column as
             text."
      ),
      call = call
    )
  }

  numeric_cols <- setdiff(all_cols, text_cols)
  non_numeric <- numeric_cols[!map_lgl(x[numeric_cols], is.numeric)]
  if (length(non_numeric) > 0L) {
    cli::cli_abort(
      c(
        "Non-text predictor columns must be numeric.",
        i = "The following {?is/are} neither text nor numeric:
             {.val {non_numeric}}.",
        i = "Declare character columns as text via {.arg text_columns}."
      ),
      call = call
    )
  }

  blueprint <- update_blueprint0(
    blueprint,
    text_columns = text_cols,
    numeric_columns = numeric_cols
  )

  blueprint
}

# ------------------------------------------------------------------------------
# mold - process

mold_text_default_process <- function(blueprint, x, y, ..., call = caller_env()) {
  check_dots_empty0(...)

  processed <- mold_text_default_process_predictors(blueprint, x, call = call)

  blueprint <- processed$blueprint
  predictors <- processed$data
  predictors_ptype <- processed$ptype
  predictors_extras <- processed$extras

  processed <- mold_xy_default_process_outcomes(blueprint, y, call = call)

  blueprint <- processed$blueprint
  outcomes <- processed$data
  outcomes_ptype <- processed$ptype
  outcomes_extras <- processed$extras

  ptypes <- new_ptypes(predictors_ptype, outcomes_ptype)
  extras <- new_extras(predictors_extras, outcomes_extras)

  blueprint <- update_blueprint0(blueprint, ptypes = ptypes)

  new_mold_process(predictors, outcomes, blueprint, extras)
}

mold_text_default_process_predictors <- function(
  blueprint,
  x,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  # Important! Collect ptype before adding intercept! The ptype validates the
  # raw input columns at forge time; the generated TF-IDF columns are instead
  # guaranteed by the frozen `text_results`.
  ptype <- extract_ptype(x, call = call)

  text_cols <- blueprint$text_columns
  numeric_cols <- blueprint$numeric_columns

  missing_text <- list()
  text_results <- list()
  text_mats <- list()

  n_docs <- nrow(x)

  for (column in text_cols) {
    tokens <- apply_tokenizer(
      blueprint$tokenizer,
      x[[column]],
      column = column,
      call = call
    )

    missing_text[[column]] <- map_lgl(tokens, is.null)

    info <- fit_text_column(
      tokens = tokens,
      column = column,
      n_docs = n_docs,
      min_times = blueprint$min_times,
      max_tokens = blueprint$max_tokens,
      smooth_idf = blueprint$smooth_idf,
      prefix = blueprint$prefix,
      sep = blueprint$sep,
      call = call
    )

    text_results[[column]] <- info
    text_mats[[column]] <- transform_text_column(tokens, info)
  }

  blueprint <- update_blueprint0(blueprint, text_results = text_results)

  predictors <- combine_predictor_parts(
    x = x,
    numeric_cols = numeric_cols,
    text_mats = text_mats,
    composition = blueprint$composition,
    intercept = blueprint$intercept,
    call = call
  )

  extras <- list(missing_text = missing_text)

  new_mold_process_terms(
    blueprint = blueprint,
    data = predictors,
    ptype = ptype,
    extras = extras
  )
}

# Combine numeric auxiliary columns with sparse TF-IDF matrices, honoring the
# requested composition. The column order is always: numeric auxiliary columns
# first (in source order), then TF-IDF columns grouped by source text column.
combine_predictor_parts <- function(
  x,
  numeric_cols,
  text_mats,
  composition,
  intercept,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  n_docs <- nrow(x)

  if (length(numeric_cols) > 0L) {
    numeric_mat <- as.matrix(x[numeric_cols])
  } else {
    numeric_mat <- matrix(numeric(), nrow = n_docs, ncol = 0L)
  }

  if (length(text_mats) > 0L) {
    text_mat <- do.call(cbind, text_mats)
  } else {
    text_mat <- Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      x = numeric(),
      dims = c(n_docs, 0L)
    )
  }

  sparse <- cbind(
    Matrix::Matrix(numeric_mat, sparse = TRUE),
    text_mat
  )

  final_names <- colnames(sparse)
  duplicated_names <- unique(final_names[duplicated(final_names)])
  if (length(duplicated_names) > 0L) {
    cli::cli_abort(
      c(
        "The generated TF-IDF feature names must be unique and must not
         collide with numeric auxiliary column names.",
        i = "The following name{?s} {?is/are} duplicated:
             {.val {duplicated_names}}.",
        i = "Consider changing {.arg prefix} or {.arg sep}."
      ),
      call = call
    )
  }

  if (intercept) {
    intercept_mat <- Matrix::sparseMatrix(
      i = seq_len(n_docs),
      j = rep.int(1L, n_docs),
      x = rep.int(1, n_docs),
      dims = c(n_docs, 1L),
      dimnames = list(NULL, "(Intercept)")
    )
    sparse <- cbind(intercept_mat, sparse)
  }

  switch(
    composition,
    dgCMatrix = sparse,
    matrix = as.matrix(sparse),
    tibble = sparsevctrs::coerce_to_sparse_tibble(sparse),
    cli::cli_abort("Unsupported composition {.val {composition}}.", call = call)
  )
}

# ------------------------------------------------------------------------------

#' @rdname run-forge
#' @export
run_forge.default_text_blueprint <- function(
  blueprint,
  new_data,
  ...,
  outcomes = FALSE,
  call = caller_env()
) {
  check_dots_empty0(...)

  cleaned <- forge_text_default_clean(
    blueprint = blueprint,
    new_data = new_data,
    outcomes = outcomes,
    call = call
  )

  blueprint <- cleaned$blueprint
  predictors <- cleaned$predictors
  outcomes <- cleaned$outcomes
  extras <- cleaned$extras

  forge_text_default_process(
    blueprint = blueprint,
    predictors = predictors,
    outcomes = outcomes,
    extras = extras,
    call = call
  )
}

# ------------------------------------------------------------------------------
# forge - clean

forge_text_default_clean <- function(
  blueprint,
  new_data,
  outcomes,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  check_data_frame_or_matrix(new_data, call = call)
  new_data <- coerce_to_tibble(new_data)
  check_unique_column_names(new_data)
  check_bool(outcomes)

  predictors <- shrink(
    new_data,
    blueprint$ptypes$predictors,
    call = call
  )

  predictors <- scream(
    predictors,
    blueprint$ptypes$predictors,
    allow_novel_levels = blueprint$allow_novel_levels,
    call = call
  )

  if (outcomes) {
    outcomes <- shrink(new_data, blueprint$ptypes$outcomes, call = call)
    # Never allow novel levels for outcomes
    outcomes <- scream(outcomes, blueprint$ptypes$outcomes, call = call)
  } else {
    outcomes <- NULL
  }

  new_forge_clean(blueprint, predictors, outcomes)
}

# ------------------------------------------------------------------------------
# forge - process

forge_text_default_process <- function(
  blueprint,
  predictors,
  outcomes,
  extras,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  processed <- forge_text_default_process_predictors(
    blueprint,
    predictors,
    call = call
  )

  blueprint <- processed$blueprint
  predictors <- processed$data
  predictors_extras <- processed$extras

  processed <- forge_xy_default_process_outcomes(blueprint, outcomes)

  blueprint <- processed$blueprint
  outcomes <- processed$data
  outcomes_extras <- processed$extras

  extras <- c(
    extras,
    new_extras(predictors_extras, outcomes_extras)
  )

  new_forge_process(predictors, outcomes, extras)
}

forge_text_default_process_predictors <- function(
  blueprint,
  predictors,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  text_cols <- blueprint$text_columns
  numeric_cols <- blueprint$numeric_columns

  missing_text <- list()
  text_mats <- list()

  for (column in text_cols) {
    tokens <- apply_tokenizer(
      blueprint$tokenizer,
      predictors[[column]],
      column = column,
      call = call
    )

    missing_text[[column]] <- map_lgl(tokens, is.null)

    # Frozen vocabulary and IDF: unseen terms are dropped inside
    # `transform_text_column()`, never adding columns.
    info <- blueprint$text_results[[column]]
    text_mats[[column]] <- transform_text_column(tokens, info)
  }

  data <- combine_predictor_parts(
    x = predictors,
    numeric_cols = numeric_cols,
    text_mats = text_mats,
    composition = blueprint$composition,
    intercept = blueprint$intercept,
    call = call
  )

  extras <- list(missing_text = missing_text)

  new_forge_process_terms(
    blueprint = blueprint,
    data = data,
    extras = extras
  )
}
