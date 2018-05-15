

#' Build a SQL IN statement.
#'
#' Returns SQL IN statement as a string. Supports multi-variable value matching
#' with tuples.
#'
#' Concatenates values in a vector and returns them in a SQL conditional format.
#' This is necessary since Friday Institute researchers are given read-only
#' access to the database.  Otherwise, you could use dplyr join commands with
#' copy=TRUE, which would create a temporary table in the database, so the
#' database can handle the calculations.  If this breaks, you can always pull
#' the entire table and do subsetting locally, but that's obviously expensive.
#'
#' @keywords internal
#' @param var_names A character vector specifying the names of the database
#'   columns to which the condition applies. If \code{values} is a vector,
#'   \code{names} should have a length of 1. If \code{values} is a data frame,
#'   \code{names} should have a length of \code{ncol(values)}.
#' @param values A vector or data frame containing values to match on. Factors
#'   will be converted to strings. Rows of a data frame are treated as tuples.
#' @return A string of format \code{"name IN (value1, value2, ...)"} for vector
#'   inputs and \code{"(name1, name2) IN ((name1value1, name2value1), ...)"} for
#'   data frames.
#' @examples
#' \dontrun{
#' courseids <- c(1, 2, 3)
#' build_sql_in("courseid", courseids)
#'
#' # tuple
#' tuple <- tribble(
#'   ~courseid, ~userid,
#'   1,         1,
#'   1,         2,
#'   2,         3,
#'   2,         4
#' )
#' build_sql_in(c("courseid", "userid"), tuple)
#' }
build_sql_in <- function(var_names, values) {
  if (is.vector(values)) {
    return(build_sql_in.vector(var_names, values))
  } else if (is.data.frame(values)) {
    return(build_sql_in.data.frame(var_names, values))
  } else {
    stop(
      paste0(
        "Unsupported type/class input to build_sql_in. Only vector ",
        "and data.frame supported."
      )
    )
  }
}



#' @keywords internal
build_sql_in_helper <- function(v) {
  # Treat categories as text, not numbers.
  if (is.factor(v)) {
    v <- as.character(v)
  }

  # Strings need quotes in SQL statements.
  if (is.character(v)) {
    v <- paste0("\"", v, "\"")
  } else {
    v <- as.character(v)
  }

  return(v)
}


#' @keywords internal
build_sql_in.vector <- function(var_name, v) {
  # SQL statement will need to be a string.
  values <- build_sql_in_helper(v) %>%
    paste(collapse = ", ")

  # SQL IN conditions have this format
  q <- sprintf(
    "%s IN (%s)",
    var_name,
    values
  )
  return(q)
}


#' @keywords internal
build_sql_in.data.frame = function(var_names, df) {
  # SQL statement will need to be a string.
  tuples <- lapply(df, build_sql_in_helper) %>%
    do.call(cbind, .) %>%
    apply(1, paste, collapse = ", ") %>%
    sprintf("(%s)", .) %>%
    paste(collapse = ", ")

  # SQL IN conditions have this format
  q = sprintf(
    "(%s) IN (%s)",
    paste(var_names, collapse = ', '),
    tuples
  )
  return(q)
}

