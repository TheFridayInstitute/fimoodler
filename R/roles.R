
#' Fetch roles assigned to users in a course
#'
#' Returns roles assigned to users in the Moodle database.  Parameters allow
#' filtering and control over role inheritance.
#'
#' Roles apply to a specific context (e.g. course-specific or system-wide) and
#' can be inherited from a parent context. Data is returned in long format and
#' only includes users with roles in the specified conditions.  In the case of
#' duplicate roles (same role given to same user, potentially a different
#' context level), only the earlier role assignment is included.
#'
#' Currently, only system- and course-level roles are supported.  Currently
#' Friday Institute is not using "course category"-level and user-level roles,
#' which are between system- and course-levels, so they are not currently
#' supported.
#'
#' Duplicate role assignments may exist if a user was enrolled more than once,
#' e.g. course 72 used manual enrollment as a way to assign groups after survey
#' enrollment had already occurred.
#'
#' @param course_id A single integer corresponding to a courseid in the
#'   database.
#' @param user_ids A vector of integers corresponding to userids enrolled in the
#'   course specified above.
#' @param inherit A single logical value specifying whether to include roles
#'   from context levels greater than course-level (TRUE) or to only use
#'   course-level roles (FALSE).
#' @param shape A single string specifying what shape the returned data should
#'   have.  \code{"wide"} output has one row per input userid and a column for
#'   each possible role.  \code{"long"} output has one row per role assignment
#'   and multiple columns of information.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble in long format with one row per role assignment.
#' @export
fetch_course_roles <- function(course_id, user_ids,
                               inherit = TRUE,
                               shape = "wide",
                               ...,
                               con = get_session_con()) {
  ra <- dplyr::tbl(con, "mdl_role_assignments") %>%
    dplyr::filter(userid %in% user_ids) %>%
    dplyr::select(userid, roleid, contextid, roleassigntm = timemodified)
  cx <- dplyr::tbl(con, "mdl_context") %>%
    dplyr::filter(contextlevel == 50, instanceid == course_id) %>%
    dplyr::select(contextid = id, courseid = instanceid)
  r <- dplyr::tbl(con, "mdl_role") %>%
    dplyr::select(roleid = id, rolename = shortname)
  course_roles <- ra %>%
    dplyr::inner_join(cx, by = "contextid") %>%
    dplyr::left_join(r, by = "roleid") %>%
    dplyr::select(courseid, userid, roleid, rolename, roleassigntm) %>%
    dplyr::collect(n = Inf)

  # Roles in higher contexts affect lower contexts
  if (inherit) {
    # System, context level 10
    cx <- dplyr::tbl(con, "mdl_context") %>%
      dplyr::filter(contextlevel == 10) %>%
      dplyr::select(contextid = id)
    system_roles <- ra %>%
      dplyr::inner_join(cx, by = "contextid") %>%
      dplyr::left_join(r, by = "roleid") %>%
      dplyr::select(userid, roleid, rolename, roleassigntm) %>%
      dplyr::collect(n = Inf)
    system_roles <- system_roles %>%
      dplyr::mutate(courseid = course_id)

    long <- course_roles %>%
      dplyr::bind_rows(system_roles)
  }

  # Duplicates are unnecessary; keep earliest assignment
  long <- long %>%
    dplyr::arrange(roleassigntm) %>%
    dplyr::distinct(courseid, userid, roleid, rolename, .keep_all = TRUE)
  if (shape == "long") return(long)

  # Roles will not be included in long data if no assignments
  all <- dplyr::tbl(con, "mdl_role") %>%
    dplyr::select(rolename = shortname) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(rolename)
  assigned <- unique(long$rolename)
  missing <- paste0("rolename_", dplyr::setdiff(all, assigned))
  quosures <- lapply(missing, function(x){rlang::quo(FALSE)}) %>%
    setNames(missing)
  all_pref <- paste0("rolename_", all) %>%
    sort()

  # Wide format needs one row per input userid
  wide <- long %>%
    dplyr::group_by(userid) %>%
    dplyr::arrange(rolename) %>%
    dplyr::mutate(rolenames = paste(rolename, collapse = ";")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(member = TRUE) %>%
    dplyr::select(userid, rolenames, rolename, member) %>%
    tidyr::spread(rolename, member, fill = FALSE, sep = "_") %>%
    dplyr::mutate(!!!quosures) %>%
    dplyr::select(
      userid,
      rolenames,
      dplyr::one_of(all_pref),
      dplyr::everything()
    )
  res <- dplyr::tibble(userid = user_ids) %>%
    dplyr::left_join(wide, by = "userid") %>%
    dplyr::select(-userid)
  return(res)
}
