

#' Fetch user details
#'
#' Gets responses to standard questions asked during Moodle account creation.
#'
#' The timezone column has a mix of formats from new and legacy Moodle versions.
#' Newer Moodle versions specify the name of a city as a string.  Older versions
#' used a number.  A timezone of 99 signifies the Moodle server's timezone.
#' Other numbers signify the difference from UTC.
#'
#' @param user_ids A vector of integers corresponding to userids.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble containing user profile information.  Each row corresponds
#'   to a value in user_ids.
#' @seealso \code{\link{fetch_user_info}}
#' @export
fetch_user_details <- function(user_ids, ..., con = get_session_con()) {
  deets <- dplyr::tbl(con, "mdl_user") %>%
    dplyr::filter(id %in% user_ids) %>%
    dplyr::select(userid = id, city, state, country, lang, calendartype,
                  timezone, lastaccess) %>%
    dplyr::collect(n = Inf)

  # Missing values should be in consistent format
  deets$city[deets$city == ""] <- NA
  deets$state[deets$state == ""] <- NA
  deets$country[deets$country == ""] <- NA

  # Row order must match input userid order
  res <- dplyr::tibble("userid" = user_ids) %>%
    dplyr::left_join(deets, by = "userid") %>%
    dplyr::select(-userid)

  return(res)
}


#' Fetch user info
#'
#' Gets responses to custom questions asked during Moodle account creation.
#'
#' User info fields can have different response formats.  Currently,
#' \code{text}, \code{menu}, and \code{multiselect} are supported.
#'
#' Responses to multi-select fields are reported in two ways.  A column named
#' after the field has a single string for each response.  The string contains
#' all selected values.  Each possible option is also given its own column that
#' has logical values indicating whether the option was selected.
#'
#' Empty string values indicate no response and are changed to the R missing
#' value indicator \code{NA}. Do not be confused by response values that appear
#' similar, such as the text \code{"NA"} or \code{"N/A"}.  Non-empty strings
#' indicate the user selected or entered that value. Inputting a value of
#' \code{"NA"} is different from not responding at all, which would be coded
#' \code{NA}.
#'
#' At the Friday Institute, some user info fields collect data that could be
#' used to uncover a user's identity.  This function is hard coded to only
#' return approved "safe" fields from which it would be difficult to uncover a
#' user's identity.  If a new safe field is added on a future date, it needs to
#' be added to the hard coded list in this package before this function will
#' return it.
#'
#' @param user_ids A vector of integers corresponding to userids.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A wide-format tibble with user info, where each row corresponds to a
#'   value of \code{user_ids}.
#' @seealso \code{\link{fetch_user_details}}
#' @export
fetch_user_info <- function(user_ids, ..., con = get_session_con()) {
  idata <- dplyr::tbl(con, "mdl_user_info_data") %>%
    dplyr::filter(userid %in% user_ids) %>%
    dplyr::filter(fieldid %in% SAFE_UINFO_FIELDS) %>%  #protect anonymity
    dplyr::select(userid, fieldid, value = data)
  ifield <- dplyr::tbl(con, "mdl_user_info_field") %>%
    dplyr::select(fieldid = id, key = shortname)
  info <- idata %>%
    dplyr::left_join(ifield, by = "fieldid") %>%
    dplyr::select(-fieldid) %>%
    dplyr::collect(n = Inf)

  # Missing values should be in consistent format
  info$value[info$value == ""] <- NA

  # Return value should be wide format
  info <- info %>%
    tidyr::spread(key, value)

  # Spreading multiselects here prevents users from having to know how to dig
  # into userinfo tables to get response options
  ms <- dplyr::tbl(con, "mdl_user_info_field") %>%
    dplyr::filter(datatype == "multiselect") %>%
    dplyr::select(key = shortname, opts = param1) %>%
    dplyr::collect(n = Inf)
  for (r in 1:nrow(ms)) {
    var <- ms[[r, "key"]]
    opts <- ms[[r, "opts"]]
    resp <- info[[var]]
    wide_resp <- spread_ms(
      resp,
      opts,
      resp_sep = "[\\r\\n]+",
      opt_sep = "[\\r\\n]+",
      prefix = paste0(var, ":"))
    info <- info %>%
      dplyr::bind_cols(wide_resp)
  }

  # Row order must match input userid order
  res <- dplyr::tibble("userid" = user_ids) %>%
    dplyr::left_join(info , by = "userid") %>%
    dplyr::select(-userid)

  return(res)
}
