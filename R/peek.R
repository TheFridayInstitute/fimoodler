



#' Peek at values in database tables
#'
#' Peek at values in database tables for the purpose of quickly scanning what's
#' present and getting id numbers needed for other functions.
#'
#' @param course_id A single integer corresponding to a courseid in the database
#'   (see \code{list_courses}).
#' @param qnr_cm_id A single integer corresponding to a questionnaire course
#'   module id (see \code{list_cms}).
#' @param con A database connection object, using the session connection by
#'   default.
#' @name peek
NULL

#' @describeIn peek Certificate instances
#' @export
list_certs <- function(course_id, con = get_session_con()) {
  certs <- dplyr::tbl(con, "mdl_certificate") %>%
    dplyr::filter(course == course_id) %>%
    dplyr::select(
      certid = id,
      certname = name,
      certdesc1 = secondline,
      certdesc2 = customtext
    ) %>%
    dplyr::collect(n = Inf)

  # User will need cmid for fetch function
  modid <- dplyr::tbl(con, "mdl_modules") %>%
    dplyr::filter(name == "certificate") %>%
    dplyr::select(modid = id) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(modid)
  cms <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::filter(module == modid, instance %in% certs$certid) %>%
    dplyr::select(certid = instance, cmid = id) %>%
    dplyr::collect(n = Inf)
  cms <- cms %>%
    dplyr::group_by(certid) %>%
    dplyr::mutate(cmids = to_JSON(cmid)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-cmid) %>%
    dplyr::distinct()
  res <- certs %>%
    dplyr::left_join(cms, by = "certid") %>%
    dplyr::select(certid, cmids, dplyr::everything())
  return(res)
}

#' @describeIn peek Course modules currently in a course
#' @export
list_cms <- function(course_id, con = get_session_con()) {
  cms <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::filter(course == course_id) %>%
    dplyr::select(cmid = id) %>%
    dplyr::collect(n = Inf)

  cms <- cms %>%
    dplyr::bind_cols(
      fetch_cm_instances(cms$cmid),
      fetch_cm_sections(cms$cmid)
    )
  cms <- cms %>%
    dplyr::select(-sectionid, -modid, -instanceid)

  # Easy to read when ordered same way as in course
  cms <- cms %>%
    dplyr::select(cmid, instancename, modname, dplyr::everything()) %>%
    dplyr::arrange(section, sectionpos)
  return(cms)
}

#' Column names in a Moodle database table
#'
#' @keywords internal
list_cols <- function(table, con = get_session_con()) {
  q = sprintf(
    "SELECT column_name AS colname
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE table_name = \"%s\"",
    table
  )
  dplyr::tbl(con, dplyr::sql(q)) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series('colname') %>%
    return()
}

#' @describeIn peek Courses
#' @export
list_courses <- function(con = get_session_con()) {
  dplyr::tbl(con, "mdl_course") %>%
    dplyr::select(
      courseid = id,
      coursefullname = fullname,
      courseshortname = shortname
    ) %>%
    dplyr::collect(n = Inf) %>%
    return()
}

#' @describeIn peek Events currently in the logstore
#' @export
list_events <- function(con = get_session_con()) {
  dplyr::tbl(con, "mdl_logstore_standard_log") %>%
    dplyr::select(eventname) %>%
    dplyr::distinct() %>%
    dplyr::arrange(eventname) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(eventname) %>%
    return()
}

#' @describeIn peek Groups currently existing in a course
#' @export
list_groups <- function(course_id, con = get_session_con()) {
  g <- dplyr::tbl(con, "mdl_groups") %>%
    dplyr::filter(courseid == course_id) %>%
    dplyr::select(groupid = id, groupname = name)
  return(g)
}

#' @describeIn peek Modules in this Moodle installation
#' @export
list_mods <- function(con = get_session_con()) {
  dplyr::tbl(con, "mdl_modules") %>%
    dplyr::select(modid = id, modname = name) %>%
    dplyr::collect(n = Inf) %>%
    return()
}

#' @describeIn peek Roles that can be given to users
#' @export
list_roles <- function(con = get_session_con()) {
  dplyr::tbl(con, "mdl_role") %>%
    dplyr::select(
      roleid = id,
      rolefullname = name,
      roleshortname = shortname,
      roledesc = description
    ) %>%
    dplyr::collect(n = Inf) %>%
    return()
}

#' @describeIn peek Sections (aka units) currently in a course
#' @export
list_sections <- function(course_id, con = get_session_con()) {
  dplyr::tbl(con, "mdl_course_sections") %>%
    dplyr::filter(course == course_id) %>%
    dplyr::select(
      courseid = course,
      sectionid = id,
      section,
      sectionname = name
    ) %>%
    dplyr::collect(n = Inf) %>%
    return()
}

#' Table names in a Moodle database
#'
#' @keywords internal
list_tables <- function(con = get_session_con()) {
  q <- "SELECT table_name, table_comment
  FROM INFORMATION_SCHEMA.TABLES
  WHERE table_schema = \"moodle\""
  dplyr::tbl(con, dplyr::sql(q)) %>%
    dplyr::collect(n = Inf) %>%
    return()
}

#' @describeIn peek Surveys filled out during enrollment
#' @export
view_enroll_survey <- function(course_id, con = get_session_con()) {
  q <- dplyr::tbl(con, "mdl_enrol_survey_questions") %>%
    dplyr::filter(courseid == course_id) %>%
    dplyr::select(
      esurveyid = enrolid,
      qid = id,
      qpos = sort_order,
      qrequired = required,
      qtype = type,
      qname = name,
      qtxt = label,
      qparentid = parentid
    ) %>%
    dplyr::collect(n = Inf)
  q <- q %>%
    dplyr::mutate(qrequired = as.logical(qrequired))

  # 1Q-to-1A is more intuitive
  grps <- q %>%
    dplyr::filter(qtype == "group") %>%
    dplyr::select(qid, qparenttxt = qtxt)
  non_grps <- q %>%
    dplyr::filter(qtype != "group") %>%
    dplyr::left_join(grps, by = c("qparentid" = "qid")) %>%
    dplyr::mutate(
      qtxt = ifelse(
        is.na(qparenttxt),
        qtxt,
        paste(qparenttxt, qtxt, sep = " >> ")
      )
    ) %>%
    dplyr::select(-qparenttxt, -qparentid)

  # Knowing response choices is essential
  non_grp_ids <- unique(non_grps$qid)
  c <- dplyr::tbl(con, "mdl_enrol_survey_options") %>%
    dplyr::filter(questionid %in% non_grp_ids) %>%
    dplyr::arrange(id) %>%
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
  non_grps <- non_grps %>%
    dplyr::left_join(cj, by = "qid")

  # "Other" response choices should be included
  if (any(non_grps$qtype == "selectother")) {
    so <- non_grps %>%
      dplyr::filter(qtype == "selectother") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(qchoices = to_JSON(c(from_JSON(qchoices), "Other"))) %>%
      dplyr::ungroup()
    non_grps <- non_grps %>%
      dplyr::filter(qtype != "selectother") %>%
      dplyr::bind_rows(so)
  }

  non_grps <- non_grps %>%
    dplyr::arrange(esurveyid, qpos)
  return(non_grps)
}

#' @describeIn peek Questionnaire questions
#' @export
view_questionnaire <- function(qnr_cm_id, con = get_session_con()) {
  qnr_id <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::filter(id == qnr_cm_id) %>%
    dplyr::collect(n = Inf) %>%
    magrittr::use_series(instance)
  q <- dplyr::tbl(con, "mdl_questionnaire_question") %>%
    dplyr::filter(
      survey_id == qnr_id,
      !(type_id %in% c(99, 100)),  #page break and section text
      deleted == "n") %>%
    dplyr::select(
      qnrid = survey_id,
      qid = id,
      qpos = position,
      qrequired = required,
      qtxt = content,
      typeid = type_id)
  qt <- dplyr::tbl(con, "mdl_questionnaire_question_type") %>%
    dplyr::filter(
      response_table != "") %>%
    dplyr::select(
      qtype = type,
      rtable = response_table,
      typeid)
  qnr <- q %>%
    dplyr::left_join(qt, by = "typeid") %>%
    dplyr::select(-typeid) %>%
    dplyr::collect(n = Inf)
  qnr <- qnr %>%
    dplyr::mutate(
      qrequired = ifelse(
        qrequired == "n",
        FALSE,
        TRUE))

  # Question text is in HTML format
  qnr <- qnr %>%
    dplyr::mutate(
      qtxt = paste0("<html>", qtxt, "</html>"))
  qnr$qtxt <- sapply(qnr$qtxt, function(x) {
    xml2::read_html(x) %>%
      rvest::html_text()
  }, USE.NAMES = FALSE)

  # Items and choices are stored separately in long format
  qc <- dplyr::tbl(con, "mdl_questionnaire_quest_choice") %>%
    dplyr::filter(
      question_id %in% qnr$qid) %>%
    dplyr::select(
      qid = question_id,
      qcid = id,
      content,
      value) %>%
    dplyr::collect(n = Inf)
  qc <- qc %>%
    dplyr::left_join(
      dplyr::select(qnr, qid, rtable),
      by = "qid")
  items <- qc %>%
    dplyr::filter(
      rtable == "response_rank",
      is.na(value)) %>%
    dplyr::select(
      qid,
      qitemid = qcid,
      itemtxt = content)
  choices <- qc %>%
    dplyr::filter(
      !(rtable == "response_rank" & is.na(value))) %>%
    dplyr::arrange(
      value) %>%
    dplyr::select(
      qid,
      choicetxt = content) %>%
    dplyr::mutate(
      choicetxt = ifelse(
        stringr::str_detect(choicetxt, "^!other"),
        "Other",
        choicetxt))

  # 1q-to-1a makes most sense
  qnr <- qnr %>%
    dplyr::left_join(items, by = "qid") %>%
    dplyr::mutate(
      qtxt = ifelse(
        is.na(itemtxt),
        qtxt,
        paste(qtxt, itemtxt, sep = " >> "))) %>%
    dplyr::select(-itemtxt, -rtable)

  # Collapsed choices are easier to read
  collapsed <- choices %>%
    dplyr::group_by(qid) %>%
    dplyr::mutate(qchoices = to_JSON(choicetxt)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(qid, qchoices)
  qnr <- qnr %>%
    dplyr::left_join(collapsed, by = "qid")

  # Yes/No questions don't have choices in db table
  yn <- dplyr::tibble(qtype = "Yes/No", qchoices = to_JSON(c("Yes", "No")))
  qnr <- qnr %>%
    dplyr::left_join(yn, by = "qtype") %>%
    dplyr::mutate(qchoices = dplyr::coalesce(qchoices.x, qchoices.y)) %>%
    dplyr::select(-qchoices.x, -qchoices.y)

  # Column order should be consistent across view functions
  qnr <- qnr %>%
    dplyr::select(qnrid, qid, qitemid, qpos, qrequired, qtype, qtxt, qchoices,
                  dplyr::everything()) %>%
    dplyr::arrange(qpos, qitemid)

  return(qnr)
}

