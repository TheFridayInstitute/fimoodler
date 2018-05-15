
#' Fetch users enrolled in a course
#'
#' Returns the first enrollment record for each user currently enrolled in the
#' specified course.
#'
#' This will not return users who were unenrolled (usually by request) from the
#' course.
#'
#' If a user has multiple enrollments for the same course, the earlier
#' enrollment is returned.  An example case of duplicates is LD Fall 2017 where
#' the enrollment engine was used to batch assign groups, resulting in multiple
#' enrollment methods
#'
#' @param course_id A single integer corresponding to a courseid in the
#'   database.
#' @param methods Vector of strings specifying which enrollment methods to
#'   include.  A NULL value allows all enrollment methods.
#' @param after A single UNIX datetime in any format that can be cast to a
#'   lubridate datetime object.  Enrollments before this time will be excluded.
#'   A NULL value will not apply this filter.
#' @param before A single UNIX datetime in any format that can be cast to a
#'   lubridate datetime object.  Enrollments on and after this time will be
#'   excluded.  A NULL value will not apply this filter.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return Tibble of users enrolled in the course and enrollment details.
#' @seealso \code{\link{fetch_unenrolled_users}}
#' @export
fetch_enrolled_users <- function(course_id,
                             methods = NULL,
                             after = NULL,
                             before = NULL,
                             ...,
                             con = get_session_con()) {

  q <- sprintf(
    "SELECT e.courseid,
    u.id AS userid,
    e.id AS enrollid,
    e.enrol AS enrollmethod,
    ue.timecreated AS enrolltc
    FROM mdl_user u,
    mdl_user_enrolments ue,
    mdl_enrol e
    WHERE u.id = ue.userid
    AND ue.enrolid = e.id
    AND e.courseid = %s",
    course_id
  )
  enrollments <- dplyr::tbl(con, dplyr::sql(q)) %>%
    dplyr::collect(n = Inf)

  # Don't want duplicate enrollment entriess; keep earliest
  enrollments <- enrollments %>%
    dplyr::arrange(courseid, userid, enrolltc) %>%
    dplyr::distinct(courseid, userid, .keep_all = TRUE)

  # Filter late, so confident we base on earliest enrollment record
  if (!is.null(methods)) {
    enrollments <- enrollments %>%
      dplyr::filter(enrollmethod %in% methods)
  }
  if (!is.null(after)) {
    after <- as.integer(lubridate::as_datetime(after))
    enrollments <- enrollments %>%
      dplyr::filter(enrolltc >= after)
  }
  if (!is.null(before)) {
    before <- as.integer(lubridate::as_datetime(before))
    enrollments <- enrollments %>%
      dplyr::filter(enrolltc < before)
  }

  return(enrollments)
}

#' Fetch users who unenrolled from a course
#'
#' Unenrolled users were enrolled at one point in time, but they were later
#' unenrolled.  The function parameters below allow filtering based on the
#' initial enrollment record.  Some information is lost when users are
#' unenrolled (see Details below), so be sure to check that you can get the
#' information you need for your analysis.
#'
#' Unenrollment usually occurs at the request of the user and is usually
#' performed by an administrator.
#'
#' When a user is unenrolled, their entry in the enrollments table is removed. A
#' variety of other information is also removed, such as group assignments in
#' the groups_members table. Logstore events and enrollment survey responses are
#' the exception.  They are preserved in the database and can be traced to
#' recall lost information, such as enrollments and group assignments.  This
#' package supports tracing enrollment logs for the identification of unenrolled
#' users.  However, this package does not support tracing other types of logs
#' for unenrolled users, because they are not often studied. Unenrolled users
#' are identified by a combination of the presence of a logstore enrollment
#' event and the absence of a record in the enrollments table.
#'
#' In the logstore, the \code{relateduserid} column specifies the enrolled user
#' and the \code{userid} column specifies the actor, which could be the enrolled
#' user if enrolled by survey or an administrator if enrolled manually.  Similar
#' to the enrollments table, there may be multiple enrollments for a single user
#' in a course.  This function focuses on the first entry, similar to
#' \code{fetch_enrolled_users}.
#'
#' Examples in course 72 are user 16129 and 15931.
#'
#' @param course_id A single integer corresponding to a courseid in the
#'   database.
#' @param methods Vector of strings specifying which enrollment methods to
#'   include.  A NULL value allows all enrollment methods.
#' @param after A single UNIX datetime in any format that can be cast to a
#'   lubridate datetime object. Enrollments before this time will be excluded.
#'   A NULL value will not apply this filter.
#' @param before A single UNIX datetime in any format that can be cast to a
#'   lubridate datetime object. Enrollments on and after this time will be
#'   excluded.  A NULL value will not apply this filter.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return Tibble of users unenrolled from the course.
#' @seealso \code{\link{fetch_enrolled_users}}
#' @export
fetch_unenrolled_users <- function(course_id,
                                   methods = NULL,
                                   after = NULL,
                                   before = NULL,
                                   ...,
                                   con = get_session_con()) {
  # Need current enrollments for comparison
  enrolls <- fetch_enrolled_users(
    course_id = course_id,
    methods = methods,
    after = after,
    before = before,
    con = con
  )

  # Need enrollment logs for comparison
  logs <- fetch_logs(
    course_id = course_id,
    events = "\\core\\event\\user_enrolment_created"
  )
  grp_hlp <- function(string, pattern) {
    if (length(string) > 0) stringr::str_match(string, pattern)[,2]
    else character(0)
  }

  # Don't want duplicate enrollment entries; keep earliest
  logs <- logs %>%
    dplyr::arrange(timecreated) %>%
    dplyr::distinct(courseid, relateduserid, .keep_all = TRUE) %>%
    dplyr::mutate(enrollmethod = grp_hlp(other, "s:6:\"(\\w+)\";")) %>%
    dplyr::select(
      courseid,
      userid = relateduserid,
      enrollmethod,
      enrolltc = timecreated
    )

  # Filter late, so confident we base on earliest enrollment record
  if (!is.null(methods)) {
    logs <- logs %>%
      dplyr::filter(enrollmethod %in% methods)
  }
  if (!is.null(after)) {
    after <- as.integer(lubridate::as_datetime(after))
    logs <- logs %>%
      dplyr::filter(enrolltc >= after)
  }
  if (!is.null(before)) {
    before <- as.integer(lubridate::as_datetime(before))
    logs <- logs %>%
      dplyr::filter(enrolltc < before)
  }

  unenrolled <- logs %>%
    dplyr::anti_join(enrolls, by = c("courseid", "userid"))

  return(unenrolled)
}


