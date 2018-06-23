

#' Fetch quiz grades
#'
#' Returns the most recent final quiz grades and the maximum possible grade for
#' the specified quiz and users.
#'
#' Missing grades are given a value of \code{NA}.
#'
#' Note that the final grades reported here are scaled, if scaling was
#' specified in the quiz.  After summing the individual quiz question grades,
#' Moodle is capable of scaling the sum.  For example, a quiz may be scored on a
#' 5 point scale (grade sums ranging 0-5), but scaled to a range of 0-20 for
#' contribution to final grade in a class.  If 100 points can be earned
#' according to they syllabus, then a range of 0-20 would mean the quiz is 20%
#' of the final course grade.
#'
#' @param quiz_cm_id A single integer corresponding to a quiz course module id
#'   (see \code{list_cms}).
#' @param user_ids A vector of integers corresponding to userids.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble with quiz grade information for each input userid.
#' @seealso \code{\link{list_cms}}
#' @export
fetch_quiz_grades <- function(quiz_cm_id, user_ids, ...,
                              con = get_session_con()) {
  quiz_id <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::filter(id == quiz_cm_id) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(instance)
  max_grade <- dplyr::tbl(con, "mdl_quiz") %>%
    dplyr::filter(id == quiz_id) %>%
    dplyr::select(maxgrade = grade) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(maxgrade)
  grades <- dplyr::tbl(con, "mdl_quiz_grades") %>%
    dplyr::filter(quiz == quiz_id, userid %in% user_ids) %>%
    dplyr::select(userid, grade, gradetm = timemodified) %>%
    dplyr::collect(n = Inf)

  # Result needs one row per input user
  res <- dplyr::tibble(userid = user_ids) %>%
    dplyr::left_join(grades, by = "userid") %>%
    dplyr::mutate(maxgrade = max_grade) %>%
    dplyr::select(-userid)
  return(res)
}
