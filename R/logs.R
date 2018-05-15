
#' Fetch trace data logs
#'
#' Fetches entries from the Moodle logstore and adds some extra columns that are
#' often helpful for research purposes.  Use arguments to fetch only the data
#' you need, so execution time is minimized.
#'
#' Course module ids are included for each log corresponding to a course module
#' instance.
#'
#' Section (sometimes referred to as a "unit" in the course) info is included in
#' the output for each log corresponding to a module that is embedded in a
#' course section.  Section number is based on the position of the modules at
#' the time this function is run.
#'
#' Each contextlevel refers to a different db table and must be handled
#' separately.  There are some contextids in the logstore that no longer exist
#' in the mdl_course_context table because the corresponding modules were
#' deleted. Events refering to these contexts will get an NA for their section.
#' The only people that access those deleted modules are likely staff.
#'
#' @param course_id A single integer corresponding to a courseid in the
#'   database.
#' @param user_ids An integer vector corresponding to userids in the database.
#'   Logs will only be returned for these users.  A NULL value will not apply
#'   this filter.
#' @param events A character vector of eventnames by which to filter the logs. A
#'   NULL value will not apply this filter.
#' @param after A single UNIX datetime in any format that can be cast to a
#'   lubridate datetime object. Entries before this time will be excluded.  A
#'   NULL value will not apply this filter.
#' @param before A single UNIX datetime in any format that can be cast to a
#'   lubridate datetime object. Entries on and after this time will be excluded.
#'   A NULL value will not apply this filter.
#' @param cm_ids An integer vector corresponding to coursemoduleids in the
#'   database. Logs will only be returned for these course modules.  A NULL
#'   value will not apply this filter.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble with one row per log event.  If user_ids are passed in, the
#'   number of users in the return may be less than the number of users passed
#'   in, because some users may not have the specified logs.
#' @export
fetch_logs <- function(course_id,
                       user_ids = NULL,
                       events = NULL,
                       after = NULL,
                       before = NULL,
                       cm_ids = NULL,
                       ...,
                       con = get_session_con()) {
  logs <- dplyr::tbl(con, "mdl_logstore_standard_log") %>%
    dplyr::filter(courseid == course_id) %>%
    dplyr::select(-ip)  # for anonymity
  if (!is.null(user_ids)) {
    logs <- logs %>%
      dplyr::filter(userid %in% user_ids)
  }
  if (!is.null(events)) {
    # Backslashes must be escaped for query
    events <- stringr::str_replace_all(events, "\\\\", "\\\\\\\\")
    logs <- logs %>%
      dplyr::filter(eventname %in% events)
  }
  if (!is.null(after)) {
    after <- as.integer(lubridate::as_datetime(after))
    logs <- logs %>%
      dplyr::filter(timecreated >= after)
  }
  if (!is.null(before)) {
    before <- as.integer(lubridate::as_datetime(before))
    logs <- logs %>%
      dplyr::filter(timecreated < before)
  }
  if (!is.null(cm_ids)) {
    logs <- logs %>%
      dplyr::filter(contextlevel == 70, contextinstanceid %in% cm_ids)
  }
  logs <- logs %>%
    dplyr::collect(n = Inf)

  # cmid allows end-user to work w/ modules w/o knowing Moodle context cols
  logs <- logs %>%
    dplyr::mutate(cmid = ifelse(contextlevel == 70, contextinstanceid, NA))

  # Section and instance info make logs more readable
  logs <- logs %>%
    dplyr::bind_cols(fetch_cm_sections(logs$cmid)) %>%
    dplyr::bind_cols(fetch_cm_instances(logs$cmid))

  return(logs)
}
