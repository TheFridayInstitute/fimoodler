
# All of the code in this script needs to be reworked to follow the same style
# as the rest of the package.  It's also extremely wasteful pulling data for all
# users and courses, instead of just what's needed.  This code was originally
# written outside the package and ported in with minimal changes just to get it
# running.






#' Extract availability conditions and requirements
#'
#' Extract a list of all operators, types, and keys in an availability condition
#' via recursive search.
#'
#' @keywords internal
#' @param avail An availability condition in list format.
#' @param pointer An environment that will hold the information.  The
#'   environment must have a symbol \code{reqs} that is a list.
#' @return No return value.  Changes are made to \code{pointer}.
extract_avail <- function(avail, pointer) {
  if ("type" %in% names(avail)) {
    # avail is a leaf and has requirements
    if (!(avail$type %in% names(pointer$reqs))) {
      pointer$reqs[[avail$type]] <- data.frame()
    }
    row.num <- nrow(pointer$reqs[[avail$type]]) + 1
    for (key in names(avail)[names(avail) != "type"]) {
      pointer$reqs[[avail$type]][row.num, key] = avail[[key]]
    }
  } else {
    # avail is a parent
    pointer$reqs$operator <- append(pointer$reqs$operator, avail$op)
    for (i in 1:length(avail$c)) {
      extract_avail(avail$c[[i]], pointer)
    }
  }
  return()
}


#' Fetch user values for availability requirements
#'
#' Given a set of availability conditions, lookup users' values that can be
#' later used to evaluate whether those conditions were met.
#'
#' Each Moodle component has its own types of requirements for completion.
#' Evaluating availability conditions requires support for each type of
#' component and a variety of condition operators.  The following condition
#' operators are supported:  \code{&}, \code{|}, \code{!&}, \code{!|}.  The
#' following components are supported:  completion, grade, group, grouping,
#' profile, and role.  The date component still needs to be incorporated.
#'
#' @keywords internal
#' @param pointer An environment that will hold the information.  The
#'   environment must have a symbol \code{reqs} that is a list of availability
#'   requirements from \code{extract_avail}.
#' @param ... Potential future args.
#' @param con A database connection object, using the session connection by
#'   default.
#' @return No return value.  Changes are made to \code{pointer}.
fetch_avail_vals <- function(pointer, ..., con = get_session_con()) {
  # Retrieve completion values
  if (!is.null(pointer$reqs$completion)) {
    include <- pointer$reqs$completion %>%
      dplyr::select(coursemoduleid = cm) %>%
      dplyr::distinct()
    comp <- dplyr::tbl(con, "mdl_course_modules_completion") %>%
      dplyr::select(
        userid,
        coursemoduleid,
        completionstate,
        timestamp = timemodified
      ) %>%
      dplyr::collect(n = Inf)
    comp <- comp %>%
      dplyr::mutate(
        timestamp = as.POSIXct(
          timestamp,
          tz="US/Eastern",
          origin = "1970-01-01"
        )
      )
    comp <- include %>%
      dplyr::left_join(comp, by = "coursemoduleid")
    pointer$vals[["completion"]] <- comp
  }

  # Retrieve date values
  # This isn't necessary b/c date is irrelevant for earning a certificate

  # Retrieve grade values
  if (!is.null(pointer$reqs$grade)) {
    include <- pointer$reqs$grade %>%
      dplyr::select(itemid = id) %>%
      dplyr::distinct()
    grades <- dplyr::tbl(con, "mdl_grade_grades") %>%
      dplyr::select(
        userid,
        itemid,
        finalgrade,
        timecreated,
        timemodified  # can have tc, tm, or tc and tm
      ) %>%
      dplyr::collect(n = Inf)
    grades <- grades %>%
      dplyr::mutate(
        timestamp = as.POSIXct(
          ifelse(!is.na(timemodified), timemodified, timecreated),
          tz="US/Eastern",
          origin = "1970-01-01"
        )
      ) %>%
      dplyr::select(-timecreated, -timemodified)
    grades <- include %>%
      dplyr::left_join(grades, by = "itemid")
    pointer$vals[["grade"]] <- grades
  }

  # Retrieve group values
  if (!is.null(pointer$reqs$group)) {
    include <- pointer$reqs$group %>%
      dplyr::select(groupid = id) %>%
      dplyr::distinct()
    groups <- dplyr::tbl(con, "mdl_groups_members") %>%
      dplyr::select(userid, groupid, timestamp = timeadded) %>%
      dplyr::collect(n = Inf)
    groups <- groups %>%
      dplyr::mutate(
        timestamp = as.POSIXct(
          timestamp,
          tz="US/Eastern",
          origin = "1970-01-01"
        )
      )
    groups <- groups %>%
      dplyr::left_join(include, by = "groupid")
    pointer$vals[["group"]] <- groups
  }

  # Retrieve grouping values
  if (!is.null(pointer$reqs$grouping)) {
    include <- pointer$reqs$grouping %>%
      dplyr::select(groupingid = id) %>%
      dplyr::distinct()
    gm <- dplyr::tbl(con, "mdl_groups_members") %>%
      dplyr::select(userid, groupid, timestamp = timeadded)
    gg <- dplyr::tbl(con, "mdl_groupings_groups") %>%
      dplyr::select(groupingid, groupid)
    groupings <- gm %>%
      dplyr::left_join(gg, by = "groupid") %>%
      dplyr::select(userid, groupingid, timestamp = timeadded) %>%
      dplyr::collect(n = Inf)
    groupings <- groupings %>%
      dplyr::mutate(
        timestamp = as.POSIXct(
          timestamp,
          tz = "US/Eastern",
          origin = "1970-01-01"
        )
      )
    groupings <- include %>%
      dplyr::left_join(groupings, by = "groupingid")
    pointer$vals[["grouping"]] <- groupings
  }

  # Retreive profile values
  if (!is.null(pointer$reqs$profile)) {
    profiles <- data.frame()

    # Custom fields (cf) and standard fields (sf) are recorded separately
    if (!is.null(pointer$reqs$profile$cf)) {
      include <- pointer$reqs$profile %>%
        dplyr::filter(!is.na(cf)) %>%
        dplyr::select(shortname = cf) %>%
        dplyr::distinct()
      idata <- dplyr::tbl(con, "mdl_user_info_data") %>%
        dplyr::select(userid, fieldid, value = data)
      ifield <- dplyr::tbl(con, "mdl_user_info_field") %>%
        dplyr::select(fieldid = id, field = shortname)
      cfs <- idata %>%
        dplyr::left_join(ifield, by = "fieldid") %>%
        dplyr::collect(n = Inf)
      cfs <- include %>%
        dplyr::left_join(cfs, by = "shortname") %>%
        dplyr::select(userid, field, value)
      profiles <- rbind(profiles, cfs)
    }
    if (!is.null(pointer$reqs$profile$sf)) {
      include <- pointer$reqs$profile %>%  #for some reason this is never used
        dplyr::filter(!is.na(sf)) %>%
        dplyr::select(sf) %>%
        dplyr::distinct()
      sfs <- dplyr::tbl(con, "mdl_user") %>%
        dplyr::select(userid = id, dplyr::everything()) %>%
        dplyr::collect(n = Inf)
      sfs <- sfs %>%
        tidyr::gather("field", "value", -userid) %>%
        dplyr::mutate(field = as.character(field))
      profiles <- rbind(profiles, sfs)
    }

    pointer$vals[["profile"]] <- profiles
  }

  # Retrieve role values
  if (!is.null(pointer$reqs$role)) {
    include <- pointer$reqs$role %>%
      dplyr::select(roleid = id) %>%
      magrittr::use_series(roleid) %>%
      unique()
    ra <- dplyr::tbl(con, "mdl_role_assignments") %>%
      dplyr::select(userid, roleid, contextid, timestamp = timemodified)
    cx <- dplyr::tbl(con, "mdl_context") %>%
      dplyr::filter(contextlevel == 50) %>%
      dplyr::select(contextid = id, courseid = instanceid)
    r <- dplyr::tbl(con, "mdl_role") %>%
      dplyr::filter(id %in% include) %>%
      dplyr::select(roleid = id)
    course_roles <- ra %>%
      dplyr::inner_join(cx, by = "contextid") %>%
      dplyr::left_join(r, by = "roleid") %>%
      dplyr::select(courseid, userid, roleid, timestamp) %>%
      dplyr::collect(n = Inf)
    course_roles <- course_roles %>%
      dplyr::mutate(
        timestamp = as.POSIXct(
          timestamp,
          tz = "US/Eastern",
          origin = "1970-01-01"
        )
      )
    pointer$vals[["role"]] <- course_roles
  }

  return()
}



#' Interprets an operator from an availability condition and applies the proper
#' logic to the operands in that condition.
#'
#' @keywords internal
apply_operator <- function(operator, operands) {
  if (operator == "&") {
    return(all(operands))
  } else if (operator == "|") {
    return(any(operands))
  } else if (operator == "!&") {
    return(!all(operands))
  } else if (operator == "!|") {
    return(!any(operands))
  } else {
    stop(paste0("Unexpected operator ", operator))
  }
}




#' If x is a terminating branch (has only leaves as children), then evaluate
#' those children, apply operator, and return value.  Otherwise, recursively
#' evaluate any children that are branches, then evaluate children that are
#' leaves, apply operator, and return value.
#'
#' @keywords internal
eval_condition <- function(x, pointer, user.id, course.id) {
  # Check if x has children and act accordingly
  if ("type" %in% names(x)) {
    # X is a leaf (i.e. has no children) and is a requirement.  Evaluate it.
    return(eval_req(x, pointer, user.id, course.id))
  } else {
    # X is a parent. Evaluate all children and apply operator.
    status <- list()
    for (i in 1:length(x$c)) {
      childStatus <- eval_condition(x$c[[i]], pointer, user.id, course.id)
      status$met <- append(status$met, childStatus$met)
      status$metOn <- append(status$metOn, childStatus$metOn)
    }
    thisConditionMet <- apply_operator(x$op, status$met)
    thisConditionStatus <- list(
      met = thisConditionMet,
      metOn = ifelse(
        thisConditionMet & any(!is.na(status$metOn)),
        max(status$metOn, na.rm = TRUE),
        NA
      )
    )
    return(thisConditionStatus)
  }
}



#' Takes a list-form availability requirement as input and forwards it to the
#' appropriate evaluation function based on the requirement's type.
#'
#' @keywords internal
eval_req <- function(req, pointer, user.id, course.id) {
  res <- switch(
    req$type,
    'completion' = eval_req_completion(req, pointer, user.id, course.id),
    'date' = eval_req_date(req, pointer, user.id, course.id),
    'grade' = eval_req_grade(req, pointer, user.id, course.id),
    'group' = eval_req_group(req, pointer, user.id, course.id),
    'grouping' = eval_req_grouping(req, pointer, user.id, course.id),
    'profile' = eval_req_profile(req, pointer, user.id, course.id),
    'role' = eval_req_role(req, pointer, user.id, course.id)
  )
  return(res)
}



#' Evaluates a list-form completion availability requirement.
#'
#' @keywords internal
eval_req_completion <- function(req, pointer, user.id, course.id) {
  # Get users actual value
  row <- pointer$vals[["completion"]] %>%
    dplyr::filter(userid == user.id, coursemoduleid == req$cm)
  actual.completion.state <- row$completionstate[1]

  # Verify user only has one entry
  if (nrow(row) > 1) {
    # There is more than one entry for this user, when there should only be 1
    stop(paste('There is more than one completion value for',
               'courseid', course.id,
               'userid', user.id,
               'coursemoduleid', req$cm))
  }

  # If there is no value, the requirement is not met.
  if (nrow(row) == 0) {
    return(list(met=F, metOn=NA))
  }

  # If there is a value, it must be compared to requirement
  res <- list(
    met = actual.completion.state == req$e,
    metOn = row$timestamp[1]
  )
  return(res)
}



#' Evaluates a list-form date availability requirement.
#'
#' @keywords internal
eval_req_date <- function(req, pointer, user.id, course.id) {
  # Date doesn't matter for earning a certificate, thus req is always met
  return(list(met=T))
}

# Evaluates a list-form grade availability requirement.
eval_req_grade <- function(req, pointer, user.id, course.id) {
  # Get users actual value
  row <- pointer$vals[["grade"]] %>%
    dplyr::filter(userid == user.id, itemid == req$id)
  actual.grade <- row$finalgrade[1]

  # Verify user has at most one entry
  if (nrow(row) > 1) {
    # There is more than one entry for this user, when there should only be 1
    stop(paste('There is more than one finalgrade value for',
               'courseid', course.id,
               'userid', user.id,
               'itemid', req$id))
  }

  # If there is no grade or the grade is NA, which can happen oddly, the
  # requirement is not met.
  if (nrow(row) == 0 | is.na(actual.grade)) {
    return(list(met=F, metOn=NA))
  }

  # If there is a grade, compare to requirements.  There could be a min, max, or
  # (min and max).
  met <- TRUE
  if (!is.null(req$min)) {
    met <- met && (actual.grade >= req$min)
  }
  if (!is.null(req$max)) {
    met <- met && (actual.grade <= req$max)
  }

  # Return
  res <- list(
    met = met,
    metOn = row$timestamp[1]
  )
  return(res)
}



#' Evaluates a list-form group availability requirement.
#'
#' @keywords internal
eval_req_group <- function(req, pointer, user.id, course.id) {
  # Get users actual value
  row <- pointer$vals[["group"]] %>%
    dplyr::filter(userid == user.id, groupid == req$id)

  # Verify user has at most one entry
  if (nrow(row) > 1) {
    # There is more than one entry for this user, when there should only be 1
    stop(paste('There is more than one group value for',
               'courseid', course.id,
               'userid', user.id,
               'groupid', req$id))
  }

  # If there is no value, the requirement is not met.
  if (nrow(row) == 0) {
    return(list(met=F, metOn=NA))
  }

  # If there is 1 entry, it must meet the requirement
  return(list(met=T, metOn=row$timestamp[1]))
}



#' Evaluates a list-form grouping availability requirement.
#'
#' @keywords internal
eval_req_grouping <- function(req, pointer, user.id, course.id) {
  # Get users actual value
  row <- pointer$vals[["grouping"]] %>%
    dplyr::filter(userid == user.id, groupingid == req$id)

  # Verify user has at most one entry
  if (nrow(row) > 1) {
    # There is more than one entry for this user, when there should only be 1
    stop(paste('There is more than one grouping value for',
               'courseid', course.id,
               'userid', user.id,
               'groupingid', req$id))
  }

  # If there is no value, the requirement is not met.
  if (nrow(row) == 0) {
    return(list(met=F, metOn=NA))
  }

  # If there is 1 entry, it must meet the requirement
  return(list(met=T, metOn=row$timestamp[1]))
}



#' Evaluates a list-form profile availability requirement.
#'
#' @keywords internal
eval_req_profile <- function(req, pointer, user.id, course.id) {
  # Get users actual value
  field.name <- ''
  if (!is.null(req$sf)) {
    field.name <- req$sf
  } else if (!is.null(req$cf)) {
    field.name <- req$cf
  } else {
    stop('Problem with profile req field name')
  }
  rows <- pointer$vals[["profile"]] %>%
    dplyr::filter(userid == user.id, field == field.name)
  value <- rows$value[1]

  # Verify user has at most one entry
  if (nrow(rows) > 1) {
    # There is more than one entry for this user, when there should only be 1
    stop(paste('There is more than one profile', field.name, 'value for',
               'courseid', course.id,
               'userid', user.id))
  }

  # If there is no value, the requirement is not met.
  if (nrow(rows) == 0) {
    return(list(met=F, metOn=NA))
  }

  # If there is 1 entry, compare it to the expected value
  met <- F
  if (req$op == 'isequalto') {
    met <- (value == req$v)
  } else if (req$op == 'contains') {
    met <- grepl(req$v, value, fixed = TRUE)
  } else if(req$op == 'doesnotcontain') {
    met <- !grepl(req$v, value, fixed = TRUE)
  } else if(req$op == 'startswith') {
    met <- grepl(paste0('^', req$v), value, fixed = TRUE)
  } else if (req$op == 'endswith') {
    met <- grepl(paste0(req$v, '$'), value, fixed = TRUE)
  } else if (req$op == 'isnotempty') {
    met <- !(value == '')
  } else if (req$op == 'isempty') {
    met <- (value == '')
  } else {
    stop(paste0('Unexpected operator in profile for user ', user.id))
  }

  # Return
  return(list(met=met, metOn=NA))
}



#' Evaluates a list-form role availability requirement.
#'
#' @keywords internal
eval_req_role <- function(req, pointer, user.id, course.id) {
  # Get users actual value
  row <- pointer$vals[["role"]] %>%
    dplyr::filter(userid == user.id, courseid == course.id, roleid == req$id)

  # Verify user has at most one entry
  if (nrow(row) > 1) {
    # There is more than one entry for this user, when there should only be 1
    stop(paste('There is more than one role value for',
               'courseid', course.id,
               'userid', user.id,
               'roleid', req$id))
  }

  # If there is no value, the requirement is not met.
  if (nrow(row) == 0) {
    return(list(met=F, metOn=NA))
  }

  # If there is 1 entry, it must meet the requirement
  return(list(met=T, metOn=row$timestamp[1]))
}
