#' Text preprocessing blueprint
#'
#' @description
#'
#' The text blueprint extends the XY blueprint with TF-IDF feature generation
#' for character columns. Rather than treating an entire piece of text as a
#' single categorical level (which would generate a one-off level per
#' document), it tokenizes the text, learns a vocabulary and document
#' frequencies at [mold()] time, and emits sparse TF-IDF columns that can be
#' fed to a model alongside numeric auxiliary columns.
#'
#' @inheritParams new-blueprint
#'
#' @param pattern A single regular expression defining a token. Only used by
#'   the default tokenizer. Defaults to `"[[:alnum:]]+"`.
#'
#' @param lowercase A single logical. Should text be lowercased before
#'   tokenizing? Only used by the default tokenizer. Defaults to `TRUE`.
#'
#' @return
#'
#' For `default_text_blueprint()`, a text blueprint.
#'
#' @section Mold:
#'
#' When [mold()] is used with the text blueprint:
#'
#' - Character columns selected as text are tokenized using `tokenizer`.
#'
#' - A vocabulary and document frequencies are learned from the training
#'   tokens, honoring `min_times` and `max_tokens`. IDF weights are computed
#'   from the document frequencies.
#'
#' - Each text column is converted into a sparse TF-IDF matrix with one column
#'   per retained term, named to preserve the source column.
#'
#' - Numeric auxiliary columns are kept as-is and combined row-wise with the
#'   TF-IDF matrix.
#'
#' - Missing text (`NA`) and empty text (`""`, or no tokens) are different
#'   inputs but both contribute all-zero TF-IDF rows. Missing text is tracked
#'   separately in the `extras$missing_text` slot.
#'
#' @section Forge:
#'
#' When [forge()] is used with the text blueprint:
#'
#' - The frozen tokenizer, vocabulary, and IDF weights are reused. Unseen
#'   terms are ignored and never create new columns, so the forged predictor
#'   matrix always has exactly the columns learned at `mold()` time.
#'
#' - Missing and empty text again produce all-zero TF-IDF rows, with missing
#'   text reported in `extras$missing_text`.
#'
#' - Numeric auxiliary columns are validated against the prototype and combined
#'   with the TF-IDF matrix in the same column order as at `mold()` time.
#'
#' @examples
#' train <- data.frame(
#'   text = c("the cat sat", "the dog ran", NA, ""),
#'   x = c(1, 2, 3, 4)
#' )
#' y <- c(1, 0, 1, 0)
#'
#' bp <- default_text_blueprint()
#' processed <- mold(train, y, blueprint = bp)
#'
#' # Sparse TF-IDF columns plus the numeric auxiliary column
#' processed$predictors
#'
#' # The vocabulary and IDF weights are frozen on the blueprint
#' processed$blueprint$text_results
#'
#' test <- data.frame(
#'   text = c("the cat", "an unseen word here", NA, ""),
#'   x = c(5, 6, 7, 8)
#' )
#'
#' # Unseen terms do not expand the columns
#' forge(test, processed$blueprint)
#'
#' @name text-blueprint
#' @export
default_text_blueprint <- function(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  composition = "dgCMatrix",
  tokenizer = NULL,
  pattern = "[[:alnum:]]+",
  lowercase = TRUE,
  text_columns = NULL,
  min_times = 1L,
  max_tokens = NULL,
  smooth_idf = TRUE,
  prefix = "tfidf",
  sep = "__"
) {
  if (is.null(tokenizer)) {
    tokenizer <- make_default_tokenizer(pattern = pattern, lowercase = lowercase)
  }

  new_default_text_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    composition = composition,
    tokenizer = tokenizer,
    text_columns = text_columns,
    numeric_columns = character(),
    min_times = min_times,
    max_tokens = max_tokens,
    smooth_idf = smooth_idf,
    prefix = prefix,
    sep = sep
  )
}

#' @inheritParams new-blueprint
#'
#' @param tokenizer A function taking a single character vector and returning a
#'   list of character token vectors, one element per input. The default is
#'   [tokenize_text()]. The tokenizer (and its options) are frozen on the
#'   blueprint and reused unchanged at [forge()] time.
#'
#' @param text_columns Either `NULL` (the default) or a character vector of
#'   predictor column names to treat as text. When `NULL`, every character
#'   column in `x` is treated as text. Numeric columns are always treated as
#'   auxiliary numeric columns and passed through.
#'
#' @param numeric_columns A character vector of auxiliary numeric predictor
#'   column names. This is determined automatically at [mold()] time and should
#'   generally not be set directly.
#'
#' @param min_times A single non-negative integer. A term must appear in at
#'   least this many training documents to be kept in the vocabulary. Defaults
#'   to `1L`.
#'
#' @param max_tokens Either `NULL` or a single positive integer. The maximum
#'   number of terms retained per text column, keeping the terms with the
#'   highest document frequency (ties broken by first appearance). Defaults to
#'   `NULL`, meaning no cap.
#'
#' @param smooth_idf A single logical. If `TRUE` (the default), IDF weights use
#'   the smoothed form `log((1 + n) / (1 + df)) + 1`, which is always finite.
#'   If `FALSE`, the classic `log(n / df)` form is used.
#'
#' @param prefix A single string used as a prefix for the TF-IDF column names
#'   generated from each text column. Defaults to `"tfidf"`. Feature columns
#'   are named `"<prefix><sep><source column><sep><term>"`, preserving the
#'   source-column boundary for multiple text columns.
#'
#' @param sep A single string separating the prefix, source column, and term
#'   parts of generated feature names. Defaults to `"__"`.
#'
#' @param text_results A named list of fitted term information, one element per
#'   text column. This is generated automatically at [mold()] time and should
#'   generally not be set directly.
#'
#' @rdname new-blueprint
#' @export
new_text_blueprint <- function(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  composition = "dgCMatrix",
  ptypes = NULL,
  tokenizer = NULL,
  text_columns = NULL,
  numeric_columns = character(),
  min_times = 1L,
  max_tokens = NULL,
  smooth_idf = TRUE,
  prefix = "tfidf",
  sep = "__",
  text_results = NULL,
  ...,
  subclass = character()
) {
  if (is.null(tokenizer)) {
    tokenizer <- make_default_tokenizer(
      pattern = "[[:alnum:]]+",
      lowercase = TRUE
    )
  }

  check_tokenizer(tokenizer)
  check_character(text_columns, allow_null = TRUE)
  check_character(numeric_columns)
  check_number_whole(min_times, min = 0)
  check_number_whole(max_tokens, min = 1, allow_null = TRUE)
  check_bool(smooth_idf)
  check_string(prefix, allow_empty = FALSE)
  check_string(sep, allow_empty = FALSE)
  check_list(text_results, allow_null = TRUE)

  new_xy_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    composition = composition,
    ptypes = ptypes,
    tokenizer = tokenizer,
    text_columns = text_columns,
    numeric_columns = numeric_columns,
    min_times = as.integer(min_times),
    max_tokens = if (is.null(max_tokens)) NULL else as.integer(max_tokens),
    smooth_idf = smooth_idf,
    prefix = prefix,
    sep = sep,
    text_results = text_results,
    ...,
    subclass = c(subclass, "text_blueprint")
  )
}

#' @rdname new-default-blueprint
#' @export
new_default_text_blueprint <- function(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  composition = "dgCMatrix",
  ptypes = NULL,
  tokenizer = NULL,
  text_columns = NULL,
  numeric_columns = character(),
  min_times = 1L,
  max_tokens = NULL,
  smooth_idf = TRUE,
  prefix = "tfidf",
  sep = "__",
  text_results = NULL,
  ...,
  subclass = character()
) {
  new_text_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    composition = composition,
    ptypes = ptypes,
    tokenizer = tokenizer,
    text_columns = text_columns,
    numeric_columns = numeric_columns,
    min_times = min_times,
    max_tokens = max_tokens,
    smooth_idf = smooth_idf,
    prefix = prefix,
    sep = sep,
    text_results = text_results,
    ...,
    subclass = c(subclass, "default_text_blueprint")
  )
}

#' @export
refresh_blueprint.text_blueprint <- function(blueprint) {
  do.call(new_text_blueprint, as.list(blueprint))
}

#' @export
refresh_blueprint.default_text_blueprint <- function(blueprint) {
  do.call(new_default_text_blueprint, as.list(blueprint))
}

check_text_blueprint <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_inherits(x, "text_blueprint", arg = arg, call = call)
}
