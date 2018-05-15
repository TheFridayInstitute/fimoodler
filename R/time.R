
#' Count periods
#'
#' Counts the number of complete periods between two time points.  Period length
#' must be specified.  Timezone of \code{start[1]} is used as timezone.
#'
#' Counting from zero yields the number of complete periods between \code{start}
#' and \code{end}. Counting from +1/-1 yields which period the value of
#' \code{end} is in.
#'
#' @param start A vector of dates or datetimes.  Lubridate, POSIX, and text
#'   (%Y-%m-%d or %Y-%m-%d %H:%M:%S) formats accepted.  Length must be either 1
#'   or length(end).
#' @param end A vector of dates or datetimes.  Lubridate, POSIX, and text
#'   (%Y-%m-%d or %Y-%m-%d %H:%M:%S) formats accepted.  Length must be either 1
#'   or length(start).
#' @param period A single lubridate period.
#' @param from_zero A single logical value specifying whether to start counting
#'   from zero (TRUE) or +1/-1 (FALSE).
#' @return A numerical vector with a value for each value of \code{start}.
#' @export
count_periods <- function(start, end, period, from_zero = FALSE) {
  i <- lubridate::interval(start, end)
  ps <- i %/% period
  if (from_zero) return(ps)
  adj <- sign(ps)
  adj <- ifelse(adj == 0 & end >= start, 1, adj)
  adj <- ifelse(adj == 0 & end < start, -1, adj)
  return(ps + adj)
}
