


#' Fetch enrollment survey responses
#'
#' Get responses to survey questions asked during course enrollment.
#'
#' If there are multiple submissions of the same survey for the same course by
#' the same user, then the most recent is returned.  Not sure why there are
#' duplicates.  It might occur when a user edits a survey response from their
#' account settings, or when they click the submit button twice before the page
#' finishes loading.
#'
#' @param esurvey_id A single integer corresponding to an enrollment survey id
#'   in the database.
#' @param user_ids A vector of integers corresponding to userids.
#' @param shape A single string specifying what shape the returned data should
#'   have.  \code{"wide"} output has one row per input userid and a column for
#'   each question.  \code{"long"} output has one row per question response and
#'   multiple columns of information.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble with enrollment survey responses.
#' @seealso \code{\link{list_enroll_surveys}}
#' @export
fetch_enroll_survey <- function(esurvey_id, user_ids,
                                shape = "wide",
                                ...,
                                con = get_session_con()) {
  q <- dplyr::tbl(con, "mdl_enrol_survey_questions") %>%
    dplyr::select(qid = id, qtype = type, qname = name)
  long <- dplyr::tbl(con, "mdl_enrol_survey_answers") %>%
    dplyr::filter(enrolid == esurvey_id, userid %in% user_ids) %>%
    dplyr::select(
      esurveyid = enrolid,
      userid,
      qid = questionid,
      rid = id,
      rtxt = answertext,
      rtc = timecreated
    ) %>%
    dplyr::left_join(q, by = "qid") %>%
    dplyr::collect(n = Inf)

  # Skipped questions are recorded as empty string response
  long <- long %>%
    dplyr::mutate(rtxt = ifelse(rtxt == "", NA, rtxt))

  # Questions may have been deleted.  No way to recover them.
  long <- long %>%
    dplyr::filter(!is.na(qname))

  # Only most recent response is desired
  long <- long %>%
    dplyr::arrange(dplyr::desc(rid)) %>%
    dplyr::distinct(userid, qid, .keep_all = T) %>%
    dplyr::arrange(userid, esurveyid, qid) %>%
    dplyr::select(userid, esurveyid, qid, qtype, qname, dplyr::everything())

  # Choices allow user to know factor levels
  long_qids <- unique(long$qid)
  c <- dplyr::tbl(con, "mdl_enrol_survey_options") %>%
    dplyr::filter(questionid %in% long_qids) %>%
    dplyr::select(qid = questionid, choicetxt = label) %>%
    dplyr::collect(n = Inf)
  cj <- c %>%
    dplyr::group_by(qid) %>%
    dplyr::mutate(
      qchoices = to_JSON(choicetxt),
      choicetxt = NULL
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  long <- long %>%
    dplyr::left_join(cj, by = "qid")

  # Checkbox responses should be in same JSON format for consistency
  if (any(long$qtype == "checkboxes")) {
    chkbox <- long %>%
      dplyr::filter(qtype == "checkboxes") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rtxt = to_JSON(as.character(from_JSON(rtxt)))) %>%
      dplyr::ungroup()
    long <- long %>%
      dplyr::filter(qtype != "checkboxes") %>%
      dplyr::bind_rows(chkbox)
  }

  # "Other" responses should be identifiable
  if (any(long$qtype == "selectother")) {
    so <- long %>%
      dplyr::filter(qtype == "selectother") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(other = !any(rtxt == from_JSON(qchoices))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        rtxtother = ifelse(other, rtxt, NA),
        rtxt = ifelse(other, "Other", rtxt)
      ) %>%
      dplyr::select(-other)
    so <- so %>%
      dplyr::rowwise() %>%
      dplyr::mutate(qchoices = to_JSON(c(from_JSON(qchoices), "Other"))) %>%
      dplyr::ungroup()
    long <- long %>%
      dplyr::filter(qtype != "selectother") %>%
      dplyr::bind_rows(so)
  }

  # Done with long format
  long <- long %>%
    dplyr::arrange(userid, esurveyid, qid)
  if (shape == "long") return(long)

  # Conflicts occur if multiple questions have the same name
  name_cnt <- long %>%
    dplyr::distinct(qid, qname) %>%
    dplyr::count(qname)
  if (any(name_cnt$n > 1)) {
    stop("Multiple questions with the same name. Can't use wide format.")
  }

  # Wide format has a col for each question and response option if multi-select
  chkbox_wide <- long %>%
    dplyr::filter(qtype == "checkboxes") %>%
    dplyr::select(userid, qname, rtxt, qchoices) %>%
    apply(1, function(row) {
      choices <- from_JSON(row["qchoices"])
      selections <- from_JSON(row["rtxt"])
      separated <- choices %in% selections %>%
        setNames(choices) %>%
        c(row["qname"], row["userid"]) %>%
        as.list() %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(userid = as.integer(userid))
      lengthend <- separated %>%
        tidyr::gather("key", "value", -qname, -userid)
      return(lengthend)
    }) %>%
    dplyr::bind_rows() %>%
    tidyr::unite("key", qname, key) %>%
    tidyr::spread(key, value)
  wide <- long %>%
    dplyr::select(userid, qname, rtxt) %>%
    tidyr::spread(qname, rtxt) %>%
    dplyr::left_join(chkbox_wide, by = "userid")

  # Questions will not be included in long data if no responses
  all_non_group <- dplyr::tbl(con, "mdl_enrol_survey_questions") %>%
    dplyr::filter(enrolid == esurvey_id, type != "group") %>%
    dplyr::select(qname = name) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(qname)
  included <- unique(long$qname)
  missing <- dplyr::setdiff(all_non_group, included)
  quosures <- lapply(missing, function(x){rlang::quo(NA)}) %>%
    setNames(missing)
  wide <- wide %>%
    dplyr::mutate(!!!quosures)

  # All userids must be represented
  wide <- dplyr::tibble(userid = user_ids) %>%
    dplyr::left_join(wide, by = "userid") %>%
    dplyr::select(-userid)
  return(wide)
}



