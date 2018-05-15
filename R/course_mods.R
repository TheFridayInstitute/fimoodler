

#' Fetch course module info
#'
#' These functions get information about course modules from a Moodle database.
#'
#' @param cm_ids An integer vector containing course module ids in the database.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @seealso \code{\link{list_cms}}
#' @name fetch_cm
NULL


#' @keywords internal
#' @rdname fetch_cm
#' @return \code{fetch_cm_sections} returns a tibble with section information,
#'   one row for each element of \code{cm_ids}.
fetch_cm_sections <- function(cm_ids, ..., con = get_session_con()) {
  # Joining requires tbl format
  cm_ids <- dplyr::tibble("cmid" = cm_ids, "order" = 1:length(cm_ids))

  cm <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::select(cmid = id, sectionid = section)
  cs <- dplyr::tbl(con, "mdl_course_sections") %>%
    dplyr::select(sectionid = id, section, sectionname = name, sequence)
  res <- cm_ids %>%
    dplyr::left_join(cm, by = "cmid", copy = TRUE) %>%
    dplyr::left_join(cs, by = "sectionid", copy = TRUE) %>%
    dplyr::collect(n = Inf)
  seqs <- seq_to_pos(res$sequence)
  res <- res %>%
    dplyr::left_join(seqs, by = "cmid")

  # cm table isn't updated when sections are deleted through UI
  res <- res %>%
    dplyr::mutate(sectionid = ifelse(is.na(section), NA, sectionid))

  # Output row order needs to correspond to input's cmids
  res <- res %>%
    dplyr::arrange(order) %>%
    dplyr::select(-cmid, -order, -sequence)
  return(res)
}

#' @keywords internal
#' @rdname fetch_cm
#' @return \code{fetch_cm_instances} returns a tibble with module instance
#'   information, one row for each element of \code{cm_ids}.
fetch_cm_instances <- function(cm_ids, ..., con = get_session_con()) {
  # Joining requires tbl format
  cm_ids <- dplyr::tibble("cmid" = cm_ids, "order" = 1:length(cm_ids))

  cm <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::select(cmid = id, modid = module, instanceid = instance)
  mod <- dplyr::tbl(con, "mdl_modules") %>%
    dplyr::select(modid = id, modname = name)
  res <- cm_ids %>%
    dplyr::left_join(cm, by = "cmid", copy = TRUE) %>%
    dplyr::left_join(mod, by = "modid", copy = TRUE) %>%
    dplyr::collect(n = Inf)

  # Each mod type has its own data tables
  res <- suppressWarnings(
    dplyr::bind_rows(lapply(unique(res$modname), fci_helper, res, con))
  )

  # Output row order needs to correspond to input's cmids
  res <- res %>%
    dplyr::arrange(order) %>%
    dplyr::select(-cmid, -order)
  return(res)
}

#' @keywords internal
fci_helper <- function(mod_name, x, con) {
  # No instances if module NA
  if (is.na(mod_name)) {
    xf <- dplyr::filter(x, is.na(modname))
    return(xf)
  }

  xf <- dplyr::filter(x, modname == mod_name)
  mod_tbl <- paste0("mdl_", dplyr::first(xf$modname))
  instances <- dplyr::tbl(con, mod_tbl) %>%
    dplyr::select(instanceid = id, instancename = name)
  xf %>%
    dplyr::left_join(instances, by = "instanceid", copy = TRUE) %>%
    dplyr::collect(n = Inf) %>%
    return()
}
