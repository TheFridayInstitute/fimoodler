#' Extract order from sequence
#'
#' Splits a sequence string and determines the position of each cmid in the
#' sequence.  Returns a mapping of unique cmids to positions.
#'
#' @keywords internal
#' @param seqs A vector of strings, where each string specifies an ordered
#'   sequence of cmids belonging to a course section, as found in the
#'   course_sections database table.
#' @return A tibble mapping cmids to their ordered position in their parent
#'   course section.  Duplicates are removed.
seq_to_pos <- function(seqs) {
  # Final mapping shouldn't have duplicates
  seqs <- unique(seqs)

  dplyr::bind_rows(lapply(seqs, seq_to_pos_helper)) %>%
    return()
}

#' @keywords internal
seq_to_pos_helper <- function(x) {
  cmid <- stringr::str_split(x, ",") %>%
    dplyr::first() %>%
    as.integer()
  sectionpos <- ifelse(is.na(cmid), NA, 1:length(cmid))
  return(dplyr::tibble(cmid, sectionpos))
}
