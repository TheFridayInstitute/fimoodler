
#' Fetch group memberships
#'
#' Returns a list of groups to which users belong in a course.
#'
#' @param course_id A single integer corresponding to a courseid in the
#'   database.
#' @param user_ids A vector of integers corresponding to userids enrolled in the
#'   course specified above.
#' @param shape A single string specifying what shape the returned data should
#'   have.  \code{"wide"} output has one row per input userid and a column for
#'   each possible group.  \code{"long"} output has one row per group assignment
#'   and multiple columns of information.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble with group assignment information.  Format depends on
#'   \code{shape} parameter.
#' @export
fetch_groups <- function(course_id,
                         user_ids,
                         shape = "wide",
                         ...,
                         con = get_session_con()) {
  gm <- dplyr::tbl(con, "mdl_groups_members") %>%
    dplyr::filter(userid %in% user_ids) %>%
    dplyr::select(userid, groupid, groupta = timeadded)
  g <- dplyr::tbl(con, "mdl_groups") %>%
    dplyr::filter(courseid == course_id) %>%
    dplyr::select(groupid = id, groupname = name)
  long <- gm %>%
    dplyr::inner_join(g, by = "groupid") %>%
    dplyr::collect(n = Inf)
  if (shape == "long") return(long)

  # Groups will not be included in long data if no assignments
  all <- dplyr::tbl(con, "mdl_groups") %>%
    dplyr::filter(courseid == course_id) %>%
    dplyr::select(groupname = name) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(groupname)
  assigned <- unique(long$groupname)
  missing <- paste0("groupname_", dplyr::setdiff(all, assigned))
  quosures <- lapply(missing, function(x){rlang::quo(FALSE)}) %>%
    setNames(missing)
  all_pref <- paste0("groupname_", all) %>%
    sort()

  # Wide format needs one row per input userid
  wide <- long %>%
    dplyr::group_by(userid) %>%
    dplyr::arrange(groupname) %>%
    dplyr::mutate(groupnames = to_JSON(groupname)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(member = TRUE) %>%
    dplyr::select(userid, groupnames, groupname, member) %>%
    tidyr::spread(groupname, member, fill = FALSE, sep = "_") %>%
    dplyr::mutate(!!!quosures) %>%
    dplyr::select(
      userid,
      groupnames,
      dplyr::one_of(all_pref),
      dplyr::everything()
    )
  res <- dplyr::tibble(userid = user_ids) %>%
    dplyr::left_join(wide, by = "userid") %>%
    dplyr::select(-userid)

  # No assigments means not in groups
  to_replace <- rep(FALSE, ncol(res)-1) %>%
    setNames(names(res)[names(res) != "groupnames"]) %>%
    as.list()
  res <- res %>%
    dplyr::mutate(groupnames = ifelse(is.na(groupnames), "[]", groupnames)) %>%
    tidyr::replace_na(to_replace)

  return(res)
}
