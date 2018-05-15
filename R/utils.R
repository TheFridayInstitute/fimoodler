

# JSON --------------------------------------------------------------------

#' Convert to/from JSON
#'
#' These functions wrap \code{jsonlite::fromJSON} and \code{jsonlite::toJSON}
#' and make it easier to work with JSON inside of a data set.
#'
#' A common use case is where all selected answers to a multi-select checkbox
#' survey question are stored in a single column of a wide-formatted data set.
#' JSON encoding is a simply way to ensure answer text doesn't interfere with
#' the process of parsing the different answers (e.g. if there was a semi-colon
#' in the answer and your separator was a semi-colon).
#'
#' \code{jsonlite} does not support the preservation of \code{NA} values that
#' would be needed for this use case, such as when someone skipped the
#' multi-select question.  \code{jsonlite} throws an error reading an \code{NA}
#' value and writes \code{NA} to a JSON object containing an \code{NA} value,
#' instead of simply returning NA.  To make the functions more friendly for data
#' sets, \code{NA} values are returned as \code{NA} during both reading and
#' writing.
#'
#' When storing JSON in data frames, JSON objects will be stored as a character
#' vector and be stripped of their \code{"json"} class that \code{jsonlite}
#' attaches.  Losing the \code{"json"} class does not have any detrimental
#' effects, but unneccessary warnings may be thrown if data frame functions
#' (e.g. \code{dplyr::mutate}) have to remove the class for you.  \code{to_JSON}
#' strips the \code{"json"} class by default to prevent warning spam.
#'
#' @param x An object to be converted to/from JSON.
#' @param keep_na A single logical value indicating whether to return \code{NA}
#'   values when received (\code{TRUE}) or not (\code{FALSE}).
#' @param rm_class A single logical value indicating whether to remove the
#'   \code{"json"} class from JSON objects (\code{TRUE}) or not (\code{FALSE}).
#' @param ... Additional arguments to be passed to \code{jsonlite} functions.
#' @name json
#' @seealso \code{\link[jsonlite]{fromJSON}}, \code{\link[jsonlite]{toJSON}}
NULL

#' @rdname json
#' @export
from_JSON <- function(x, keep_na = TRUE, ...) {
  if (keep_na) {
    if (length(x) == 1 && is.na(x)) return(NA)
  }
  return(jsonlite::fromJSON(x, ...))
}

#' @rdname json
#' @export
to_JSON <- function(x, keep_na = TRUE, rm_class = TRUE, ...) {
  if (keep_na) {
    if (length(x) == 1 && is.na(x)) return(NA)
  }
  j <- jsonlite::toJSON(x, ...)
  if (rm_class) class(j) <- class(j)[class(j) != "json"]
  return(j)
}



# Survey ------------------------------------------------------------------



#' Cast multi-select responses into wide format
#'
#' @keywords internal
#' @param resp A list of character vectors, where each character vector is a
#'   response to a multi-select question.  Each response can be a vector of
#'   strings, where each value represents a selected option, or a single string
#'   of all selected options that still needs to be split (must also use
#'   \code{sep} param).
#' @param opt A character vector listing all possible response options to the
#'   multi-select question.
#' @param resp_sep A single string containing a regular expression.  Each string
#'   in \code{resp} will be split where this expression matches.  A value of
#'   NULL indicates the responses are already split.
#' @param opt_sep A single string containing a regular expression.  Each string
#'   in \code{opt} will be split where this expression matches.  A value of NULL
#'   indicates the options are already split.
#' @param prefix A single string to prefix each column name in the output.
#' @return A tibble with one column for each possible option and one row for
#'   each response.  Logical values indicate whether each option was selected in
#'   each response.
spread_ms <- function(resp, opt, resp_sep = NULL, opt_sep = NULL, prefix = "") {
  if (!is.null(resp_sep)) resp <- stringr::str_split(resp, resp_sep)
  if (!is.null(opt_sep)) opt <- stringr::str_split(opt, opt_sep)[[1]]
  dplyr::bind_rows(lapply(resp, spread_ms_helper, opt, prefix)) %>%
    return()
}

#' @keywords internal
spread_ms_helper <- function(x, opt, prefix) {
  res <- opt %in% x %>%
    setNames(paste0(prefix, opt)) %>%
    as.list() %>%
    dplyr::as_tibble()
}
