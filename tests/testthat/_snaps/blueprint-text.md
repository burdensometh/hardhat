# text blueprint validates its arguments

    Code
      default_text_blueprint(min_times = -1L)
    Condition
      Error in `new_text_blueprint()`:
      ! `min_times` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      default_text_blueprint(max_tokens = 0L)
    Condition
      Error in `new_text_blueprint()`:
      ! `max_tokens` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      default_text_blueprint(tokenizer = "not a function")
    Condition
      Error in `new_text_blueprint()`:
      ! `tokenizer` must be a function, not the string "not a function".

---

    Code
      default_text_blueprint(prefix = "")
    Condition
      Error in `new_text_blueprint()`:
      ! `prefix` must be a single string, not the empty string "".

---

    Code
      default_text_blueprint(sep = "")
    Condition
      Error in `new_text_blueprint()`:
      ! `sep` must be a single string, not the empty string "".

---

    Code
      default_text_blueprint(composition = "nope")
    Condition
      Error in `new_blueprint()`:
      ! `composition` must be one of "tibble", "matrix", or "dgCMatrix", not "nope".

# mold with text blueprint produces sparse TF-IDF predictors

    Code
      print(out$blueprint)
    Message
      Text blueprint:
      # Predictors: 2
      # Outcomes: 1
      Intercept: FALSE
      Novel Levels: FALSE
      Composition: dgCMatrix
      

# a failing tokenizer errors with the column named

    Code
      mold(d, y = 0, blueprint = default_text_blueprint(tokenizer = bad_tokenizer))
    Condition
      Error in `mold()`:
      ! The `tokenizer` failed while tokenizing column "txt".
      Caused by error in `tokenizer()`:
      ! boom

# tokenizer returning wrong shape errors

    Code
      mold(d, y = c(0, 1), blueprint = default_text_blueprint(tokenizer = bad_tokenizer))
    Condition
      Error in `mold()`:
      ! The `tokenizer` must return a list the same length as its input.
      i Column "txt" has length 2 but the tokenizer returned length 2.

# undeclared and non-existent text columns error clearly

    Code
      mold(train, y = c(0, 1), blueprint = default_text_blueprint(text_columns = "text"))
    Condition
      Error in `mold()`:
      ! All character columns must either be declared as text columns or removed from `x`.
      i The following character column was not declared in `text_columns`: "other".
      i Set `text_columns = NULL` to treat every character column as text.

---

    Code
      mold(train, y = c(0, 1), blueprint = default_text_blueprint(text_columns = "missing"))
    Condition
      Error in `mold()`:
      ! The following text column was not found in `x`: "missing".

# non-numeric non-text predictor columns error

    Code
      mold(train, y = c(0, 1), blueprint = default_text_blueprint())
    Condition
      Error in `mold()`:
      ! Non-text predictor columns must be numeric.
      i The following is neither text nor numeric: "f".
      i Declare character columns as text via `text_columns`.

# forge validates missing columns

    Code
      forge(data.frame(n = c(1, 2)), out$blueprint)
    Condition
      Error in `forge()`:
      ! The required column "text" is missing.

# feature names colliding with numeric columns error clearly

    Code
      mold(train, y = c(0, 1), blueprint = default_text_blueprint())
    Condition
      Error in `mold()`:
      ! The generated TF-IDF feature names must be unique and must not collide with numeric auxiliary column names.
      i The following name is duplicated: "tfidf__text__a".
      i Consider changing `prefix` or `sep`.

