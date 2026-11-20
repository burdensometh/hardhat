# Text tokenization and TF-IDF engine for the text blueprint
#
# The text blueprint turns character columns into sparse TF-IDF features. At
# `mold()` time a vocabulary and document frequencies are learned from the
# training data. Everything learned is stored on the blueprint so `forge()`
# can reproduce identical columns without ever expanding them.

# ------------------------------------------------------------------------------
# Tokenization

#' Tokenize character vectors for the text blueprint
#'
#' @description
#'
#' `tokenize_text()` is the default tokenizer used by
#' [default_text_blueprint()]. It extracts "word" tokens using regular
#' expression matches of consecutive letters or digits (see the `pattern`
#' argument). It deliberately has no hard dependencies beyond base R.
#'
#' Custom tokenizers can be supplied through the `tokenizer` argument of
#' [default_text_blueprint()]. A tokenizer is any function taking a single
#' character vector as its only argument and returning a list the same length
#' as that vector, where each element is a character vector of tokens. This
#' makes it straightforward to plug in tokenizers from other packages (e.g. for
#' stemming or Chinese word segmentation).
#'
#' @param x A character vector. Missing values (`NA`) are allowed and are
#'   preserved: the corresponding element of the returned list is `NULL`,
#'   which distinguishes missing text from empty text (which produces
#'   `character(0)`).
#'
#' @param pattern A single regular expression defining a token. Defaults to
#'   `"[[:alnum:]]+"`, which matches runs of letters or digits.
#'
#' @param lowercase A single logical. Should input be lowercased before
#'   tokenizing? Defaults to `TRUE`.
#'
#' @return
#'
#' A list the same length as `x`. Each element is a character vector of
#' tokens, with these special cases:
#'
#' - `NA_character_` input returns `NULL` (missing text).
#'
#' - Empty strings (`""`) or strings with no token matches return
#'   `character(0)` (empty text).
#'
#' @keywords internal
#' @export
#'
#' @examples
#' x <- c("Hello, world!", "foo-bar 123", NA, "")
#' tokenize_text(x)
tokenize_text <- function(x, ..., pattern = "[[:alnum:]]+", lowercase = TRUE) {
  check_dots_empty0(...)
  check_character(x)
  check_string(pattern, allow_empty = FALSE)
  check_bool(lowercase)

  if (lowercase) {
    x <- tolower(x)
  }

  # `regmatches()` with `gregexpr()` keeps `character(0)` for non-matching
  # strings. Normalize the `NA` case to `NULL` so missing and empty text stay
  # distinguishable downstream.
  matches <- gregexpr(pattern, x, perl = TRUE)
  tokens <- regmatches(x, matches)

  na_loc <- which(is.na(x))
  tokens[na_loc] <- rep(list(NULL), length(na_loc))

  tokens
}

# Build the default tokenizer as a self-contained closure so `pattern` and
# `lowercase` are frozen on the blueprint. A custom `tokenizer` supplied by the
# user is used as-is and must itself be a one-argument function.
make_default_tokenizer <- function(pattern, lowercase) {
  force(pattern)
  force(lowercase)

  function(x) {
    tokenize_text(x, pattern = pattern, lowercase = lowercase)
  }
}

check_tokenizer <- function(x, ..., call = caller_env()) {
  if (is_function(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x = x,
    what = "a function",
    arg = "tokenizer",
    call = call
  )
}

# Apply a tokenizer, validating that its output has the required shape.
apply_tokenizer <- function(tokenizer, x, column, ..., call = caller_env()) {
  check_dots_empty0(...)

  tokens <- try_fetch(
    tokenizer(x),
    error = function(cnd) {
      cli::cli_abort(
        "The {.arg tokenizer} failed while tokenizing column {.val {column}}.",
        parent = cnd,
        call = call
      )
    }
  )

  if (!is.list(tokens) || length(tokens) != length(x)) {
    cli::cli_abort(
      c(
        "The {.arg tokenizer} must return a list the same length as its input.",
        i = "Column {.val {column}} has length {length(x)} but the tokenizer
             returned length {length(tokens)}."
      ),
      call = call
    )
  }

  bad <- which(!map_lgl(tokens, function(elt) is.null(elt) || is.character(elt)))
  if (length(bad) > 0L) {
    cli::cli_abort(
      c(
        "Every element of the {.arg tokenizer} result must be a character
         vector or {.code NULL}.",
        i = "Column {.val {column}} has invalid output at row {.val {bad}}."
      ),
      call = call
    )
  }

  tokens
}

# ------------------------------------------------------------------------------
# Vocabulary and document frequency learning

# A fitted text column. Everything in here is required at `forge()` time and
# nothing else is kept.
#
# - `column`: name of the source character column.
# - `vocabulary`: character vector of learned terms, in first-seen order. This
#   order fixes the column order of the TF-IDF matrix.
# - `idf`: numeric vector of IDF weights aligned with `vocabulary`.
# - `feature_names`: column names emitted for this source column, prefixed so
#   multiple text columns keep their source boundary.
new_text_term_info <- function(column, vocabulary, idf, feature_names) {
  list(
    column = column,
    vocabulary = vocabulary,
    idf = idf,
    feature_names = feature_names
  )
}

# Learn the vocabulary and IDF weights for one character column.
#
# `smooth_idf = TRUE` uses the scikit-learn style smoothing
# `idf = log((1 + n) / (1 + df)) + 1`, which is always finite. With
# `smooth_idf = FALSE` the classic `log(n / df)` form is used.
fit_text_column <- function(
  tokens,
  column,
  n_docs,
  min_times,
  max_tokens,
  smooth_idf,
  prefix,
  sep,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)

  # Document frequency: number of non-missing documents each term appears in.
  # Missing documents (`NULL`) are excluded; empty documents contribute no
  # terms. Taking `unique()` per document ensures a term counts at most once
  # per document, which is the definition of document frequency.
  doc_terms <- tokens[!map_lgl(tokens, is.null)]

  if (length(doc_terms) == 0L) {
    return(new_text_term_info(
      column = column,
      vocabulary = character(),
      idf = numeric(),
      feature_names = character()
    ))
  }

  unique_sets <- lapply(doc_terms, unique)

  # Document frequency per term, counting each non-missing document at most
  # once. `table()` returns alphabetical order, which we reindex below.
  df_table <- table(unlist(unique_sets, use.names = FALSE))

  # First-seen order (rather than alphabetical) so the learned column order
  # tracks the training data and stays stable across save/reload.
  vocabulary <- unique(unlist(unique_sets, use.names = FALSE))
  document_frequency <- as.integer(df_table[vocabulary])

  keep <- document_frequency >= min_times
  vocabulary <- vocabulary[keep]
  document_frequency <- document_frequency[keep]

  if (!is.null(max_tokens) && length(vocabulary) > max_tokens) {
    # Keep the most frequent terms; ties break by first-seen order.
    ord <- order(-document_frequency, seq_along(document_frequency))
    ord <- sort(ord[seq_len(max_tokens)])
    vocabulary <- vocabulary[ord]
    document_frequency <- document_frequency[ord]
  }

  if (smooth_idf) {
    idf <- log((1 + n_docs) / (1 + document_frequency)) + 1
  } else {
    idf <- log(n_docs / document_frequency)
  }

  # Include the source column in every feature name so multiple text columns
  # keep their source boundary even when they share identical terms.
  feature_names <- paste0(prefix, sep, column, sep, vocabulary)

  new_text_term_info(
    column = column,
    vocabulary = vocabulary,
    idf = idf,
    feature_names = feature_names
  )
}

# ------------------------------------------------------------------------------
# TF-IDF transformation

# Build a sparse TF-IDF dgCMatrix for one column using fitted term info.
#
# The vocabulary and IDF weights are frozen in `info`. Terms not present in
# `info$vocabulary` are ignored, so unseen words never create new columns.
#
# Rows align with the input: missing text (`NULL` tokens) and empty text
# (`character(0)` tokens) both produce all-zero rows in the TF-IDF matrix.
transform_text_column <- function(tokens, info) {
  n_docs <- length(tokens)
  n_terms <- length(info$vocabulary)

  if (n_terms == 0L) {
    return(Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      x = numeric(),
      dims = c(n_docs, 0L),
      dimnames = list(NULL, character())
    ))
  }

  term_index <- seq_len(n_terms)
  names(term_index) <- info$vocabulary

  # Accumulate triplet components per document in lists, then flatten once.
  # Growing vectors inside the loop would be quadratic in the number of
  # non-zero entries.
  row_parts <- list()
  col_parts <- list()
  tf_parts <- list()

  for (i in seq_along(tokens)) {
    doc_tokens <- tokens[[i]]
    if (is.null(doc_tokens) || length(doc_tokens) == 0L) {
      next
    }

    counts <- table(doc_tokens)
    terms <- names(counts)

    mapped <- term_index[terms]
    hit <- which(!is.na(mapped))

    if (length(hit) == 0L) {
      next
    }

    row_parts[[i]] <- rep.int(i, length(hit))
    col_parts[[i]] <- mapped[hit]
    tf_parts[[i]] <- as.numeric(counts)[hit]
  }

  row_loc <- unlist(row_parts, use.names = FALSE)
  col_loc <- unlist(col_parts, use.names = FALSE)
  tf_values <- unlist(tf_parts, use.names = FALSE)

  if (length(tf_values) == 0L) {
    return(Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      x = numeric(),
      dims = c(n_docs, n_terms),
      dimnames = list(NULL, info$feature_names)
    ))
  }

  tf <- Matrix::sparseMatrix(
    i = row_loc,
    j = col_loc,
    x = tf_values,
    dims = c(n_docs, n_terms),
    dimnames = list(NULL, info$feature_names)
  )

  # Multiply each column by its frozen IDF weight.
  out <- tf %*% Matrix::Diagonal(n = n_terms, x = info$idf)
  colnames(out) <- info$feature_names
  out
}
