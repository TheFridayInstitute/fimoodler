
# All of the code in this script needs to be reworked to follow the same style
# as the rest of the package.  It's also extremely wasteful pulling data for all
# users and courses, instead of just what's needed.  This code was originally
# written outside the package and ported in with minimal changes just to get it
# running.




#' Fetch certificate information
#'
#' Returns information about whether users earned and viewed a course
#' certificate, in addition to what datetime those events occurred.
#'
#' Only certificate views are neatly tracked in database tables.  Whether or not
#' a certificate is earned has to be determined at the time this function is run
#' by comparing the certificate's availability conditions to trace data.  Thus,
#' completion results may differ between runs of this function if a course
#' editor changes the availability conditions of the certificate or a student
#' meets a requirement they had not previously met.
#'
#' @param cert_cm_id A single integer corresponding to a certificate course
#'   module id (see \code{list_certs} or \code{list_cms}).
#' @param user_ids A vector of integers corresponding to userids.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return A tibble with certificate information for each input userid.
#' @seealso \code{\link{list_certs}}
#' @export
fetch_certs <- function(cert_cm_id, user_ids, ..., con = get_session_con()) {
  # The coursemodule has the availability conditions
  cm <- dplyr::tbl(con, "mdl_course_modules") %>%
    dplyr::filter(id == cert_cm_id) %>%
    dplyr::select(
      courseid = course,
      modid = module,
      cmid = id,
      cmavail = availability,
      certid = instance
    ) %>%
    dplyr::collect(n = Inf)
  course_id <- cm$courseid[1]
  cert_id <- cm$certid[1]
  avail <- from_JSON(cm$cmavail[1], simplifyVector = FALSE)

  # Aggregating conditions and requirements reduces number of db queries
  pointer = new.env(parent = globalenv())
  pointer$reqs <- list()
  extract_avail(avail, pointer)
  pointer$vals <- list()
  fetch_avail_vals(pointer)

  # Enrollment times needed to determine when some reqs are met
  pointer$enroll <- fetch_enrolled_users(course_id, con = con)

  # Conditions and requirements must be met to earn cert
  certs_earned <- data.frame()
  for (i in 1:length(user_ids)) {
    user_id = user_ids[i]
    cert_status = eval_condition(avail, pointer, user_id, course_id)
    r = nrow(certs_earned) + 1
    certs_earned[r, "certid"] = cert_id
    certs_earned[r, "userid"] = user_id
    certs_earned[r, "certearned"] = cert_status$met
    certs_earned[r, "certearnedat"] = cert_status$metOn
  }

  # Viewing an earned certificate is different from earning one
  certs_viewed <- dplyr::tbl(con, "mdl_certificate_issues") %>%
    dplyr::filter(certificateid == cert_id) %>%
    dplyr::select(
      userid,
      certid = certificateid,
      certviewedat = timecreated
    ) %>%
    dplyr::collect(n = Inf)
  certs_viewed <- certs_viewed %>%
    dplyr::mutate(certviewed = TRUE) %>%
    dplyr::select(userid, certid, certviewed, dplyr::everything())

  # All input userids should be represented
  res <- dplyr::tibble(userid = user_ids) %>%
    dplyr::left_join(certs_earned, by = "userid") %>%
    dplyr::left_join(certs_viewed, by = c("userid", "certid")) %>%
    dplyr::select(-certid, -userid)

  # No view entry means not viewed
  res <- res %>%
    tidyr::replace_na(list("certviewed" = FALSE))

  return(res)
}




