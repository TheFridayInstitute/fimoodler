

#' Fetch questionnaire responses
#'
#' Returns responses to the specified questionnaire in long format.  Incomplete
#' submissions can be excluded.
#'
#' Moodle does not store short names for questionnaire questions.  If you desire
#' wide format data, questions will have to be manually named and reformatted.
#'
#' @param qnr_cm_id A single integer corresponding to a questionnaire course
#'   module id (see \code{list_cms}).
#' @param user_ids A vector of integers corresponding to userids.
#' @param include_incomplete A single logical value indicating whether to
#'   include data from incomplete questionnaires (TRUE) or not (FALSE). Note
#'   that a questionnaire is considered complete when the entire questionnaire
#'   is submitted, even if a non-required question is left blank.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble with questionnaire responses for each input userid.
#' @seealso \code{\link{list_cms}}
#' @export
fetch_questionnaires <- function(qnr_cm_id, user_ids,
                                 include_incomplete = FALSE, ...,
                                 con = get_session_con()) {
  # Need questionnaire id (not cmid) to get responses
  qnr_id <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::filter(id == qnr_cm_id) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(instance)

  # Fetch response ids
  r <- dplyr::tbl(con, "mdl_questionnaire_response") %>%
    dplyr::select(
      userid = username,
      qnrid = survey_id,
      rid = id,
      rtc = submitted,
      rcomplete = complete) %>%
    dplyr::filter(
      qnrid == qnr_id,
      userid %in% user_ids) %>%
    dplyr::collect(n = Inf)
  r <- r %>%
    dplyr::mutate(
      rcomplete = ifelse(
        rcomplete == "y",
        TRUE,
        FALSE))
  if (!include_incomplete) {
    r <- r %>%
      dplyr::filter(
        rcomplete)
  }

  # If no responses, we are done
  if (nrow(r) == 0) {
    return(r)
  }

  # Question response types have different data structures
  rvals <- dplyr::tibble()
  choice <- dplyr::tbl(con, "mdl_questionnaire_quest_choice") %>%
    dplyr::select(
      qid = question_id,
      choiceid = id,
      rtxt = content,
      choiceval = value)

  # Multiple response: choice_id maps to choice table.
  rm <- dplyr::tbl(con, "mdl_questionnaire_resp_multiple") %>%
    dplyr::select(
      rid = response_id,
      qid = question_id,
      choiceid = choice_id) %>%
    dplyr::filter(
      rid %in% r$rid)
  rmc <- rm %>%
    dplyr::left_join(
      choice, by = c("qid", "choiceid")) %>%
    dplyr::collect(n = Inf)
  rmc <- rmc %>%
    dplyr::rename(
      multval = choiceval) %>%
    dplyr::filter(
      !stringr::str_detect(rtxt, "^!other"))  #handled in other section below
  rvals <- rvals %>%
    dplyr::bind_rows(rmc)

  # Single response: choice_id maps to choice table.
  rs <- dplyr::tbl(con, "mdl_questionnaire_resp_single") %>%
    dplyr::select(
      rid = response_id,
      qid = question_id,
      choiceid = choice_id) %>%
    dplyr::filter(
      rid %in% r$rid)
  rsc <- rs %>%
    dplyr::left_join(
      choice, by = c("qid", "choiceid")) %>%
    dplyr::collect(n = Inf)
  rsc <- rsc %>%
    dplyr::rename(
      singleval = choiceval) %>%
    dplyr::filter(
      !stringr::str_detect(rtxt, "^!other"))  #handled in other section below
  rvals <- rvals %>%
    dplyr::bind_rows(rsc)

  # Bool: choice_id is the actual choice text y/n.
  rb <- dplyr::tbl(con, "mdl_questionnaire_response_bool") %>%
    dplyr::select(
      rid = response_id,
      qid = question_id,
      rtxt = choice_id) %>%
    dplyr::filter(
      rid %in% r$rid) %>%
    dplyr::collect(n = Inf)
  rbc <- rb %>%
    dplyr::mutate(
      choiceid = NA,
      rtxt = ifelse(
        rtxt == "y",
        "Yes",
        "No"),
      boolval = ifelse(
        rtxt == "Yes",
        TRUE,
        FALSE))
  rvals <- rvals %>%
    dplyr::bind_rows(rbc)

  # Date: choice_id is replaced by "response" which has a date YYYY-MM-DD.
  rd <- dplyr::tbl(con, "mdl_questionnaire_response_date") %>%
    dplyr::select(
      rid = response_id,
      qid = question_id,
      rtxt = response) %>%
    dplyr::filter(
      rid %in% r$rid) %>%
    dplyr::collect(n = Inf)
  rdc <- rd %>%
    dplyr::mutate(
      choiceid = NA,
      dateval = as.integer(lubridate::as_datetime(rtxt)))
  rvals <- rvals %>%
    dplyr::bind_rows(rdc)

  # Other: Special type that exists within single and multiple response types.
  # choice_id maps to choice table to get the text of the other statement, e.g.
  # "Other (please specify):"  The text value the user inputs is in the
  # "response" field.
  ro <- dplyr::tbl(con, "mdl_questionnaire_response_other") %>%
    dplyr::select(
      rid = response_id,
      qid = question_id,
      rtxtother = response) %>%
    dplyr::filter(
      rid %in% r$rid) %>%
    dplyr::collect(n = Inf)
  roc <- ro %>%
    dplyr::mutate(
      rtxt = "Other",
      choiceid = NA)
  rvals <- rvals %>%
    dplyr::bind_rows(roc)

  # Rank: choice_id is item.  rank is the selected value.  if rank does not
  # match value in choices table (e.g. -999), then item not answered.
  rr <- dplyr::tbl(con, "mdl_questionnaire_response_rank") %>%
    dplyr::select(
      rid = response_id,
      qid = question_id,
      qitemid = choice_id,
      choiceval = rank) %>%
    dplyr::filter(
      rid %in% r$rid)
  rrc <- rr %>%
    dplyr::left_join(choice, by = c("qid", "choiceval")) %>%
    dplyr::collect(n = Inf)
  rrc <- rrc %>%
    dplyr::rename(
      rankval = choiceval) %>%
    dplyr::mutate(
      rankval = ifelse(
        is.na(choiceid),
        NA,
        rankval))
  rvals <- rvals %>%
    dplyr::bind_rows(rrc)

  # Text: Responses are in "response" col. Responses may be HTML or plain text.
  rt <- dplyr::tbl(con, "mdl_questionnaire_response_text") %>%
    dplyr::select(
      rid = response_id,
      qid = question_id,
      rtxt = response) %>%
    dplyr::filter(
      rid %in% r$rid) %>%
    dplyr::collect(n = Inf)
  rt <- rt %>%
    dplyr::mutate(
      choiceid = NA,
      rtxt = paste0("<html>", rtxt, "</html>"))
  rt$rtxt <- sapply(rt$rtxt, function(x) {
    xml2::read_html(x) %>%
      rvest::html_text()
  }, USE.NAMES = FALSE)
  rvals <- rvals %>%
    dplyr::bind_rows(rt)

  # Skipped qs and items should have a value, but only include users with resp.
  rvals <- rvals %>%
    dplyr::mutate(
      qnrcmid = qnr_cm_id,
      qnrid = qnr_id)
  all_qs <- view_questionnaire(qnr_cm_id, con) %>%
    dplyr::mutate(
      qnrcmid = qnr_cm_id) %>%
    dplyr::select(
      qnrcmid,
      qnrid,
      qid,
      qitemid,
      qtype,
      qchoices)
  long <- r %>%
    dplyr::mutate(
      qnrcmid = qnr_cm_id) %>%
    dplyr::left_join(
      all_qs, by = c("qnrcmid", "qnrid")) %>%
    dplyr::left_join(
      rvals, by = c("qnrcmid", "qnrid", "rid", "qid", "qitemid")) %>%
    dplyr::arrange(
      userid,
      rid,
      qid,
      qitemid) %>%
    dplyr::select(
      userid,
      qnrcmid,
      qnrid,
      qid,
      qtype,
      qitemid,
      rid,
      rcomplete,
      rtxt,
      rtc,
      qchoices,
      dplyr::everything(),
      -choiceid)
  return(long)
}
