library(testthat)
library(glue)
library(rlang)

# generic information message
msg <- function(q, ...) glue("Problem in question {q}: ", ..., ".")

# expect specific numeric or text value
expect_value <- function(q, var, value = NULL, expr = identity, dec = 3) {
  var_quo <- enquo(var)
  var_name <- quo_text(var_quo)
  expect_true(
    exists(var_name),
    info = msg(q, glue("'{var_name}' object should exist but does not"))
  )
  if (!is.null(value)) {
    var <- expr(eval_tidy(var_quo))
    if (is.numeric(value)) {
      var <- as.numeric(round(var, dec))
      value <- round(value, dec)
    }
    expect_equal(
      var, value,
      info = msg(q, glue("'{var_name}' does not have the correct value"))
    )
  }
}

# expect functoin value
expect_func_value <- function(q, var, value = NULL, params = list(), dec = 3) {
  var_quo <- enquo(var)
  var_name <- quo_text(var_quo)
  expect_true(
    exists(var_name),
    info = msg(q, glue("'{var_name}' function should exist but does not"))
  )
  if (!is.null(value)) {
    var <- do.call(var, args = params)
    if (is.numeric(value)) {
      var <- as.numeric(round(var, dec))
      value <- round(value, dec)
    }
    expect_equal(
      var, value,
      info = msg(q, glue("'{var_name}' does not have the correct value"))
    )
  }
}

# expect columns to be present
expect_names <- function(q, var, cols, expr = identity) {
  var_quo <- enquo(var)
  var_name <- quo_text(var_quo)
  expect_true(
    exists(var_name),
    info = msg(q, glue("'{var_name}' object should exist but does not"))
  )
  var_cols <- names(expr(eval_tidy(var_quo)))
  expect_true(
    all(cols %in% var_cols),
    info = msg(q, glue("'{var_name}' does not have all expected names: ",
                       "{collapse(cols, sep=', ')}"))
  )
}

# expect a specific class
expect_class <- function(q, var, class) {
  var_quo <- enquo(var)
  var_name <- quo_text(var_quo)
  expect_true(
    exists(var_name),
    info = msg(q, glue("'{var_name}' object should exist but does not"))
  )
  var <- eval_tidy(var_quo)
  expect_is(
    var, class,
    info = msg(q, glue("'{var_name}' does not have the expected class: {class}"))
  )
}