
#' Connect to Moodle database
#'
#' These are functions for opening and closing database connections.  A
#' \strong{session} is a convenience that keeps track of a connection for you,
#' so you don't have to pass it to function calls.  However, you can create a
#' connection object outside of a session if you wish.
#'
#' These functions are wrappers around dbplyr and pool objects.  These
#' underlying tools neatly open and close individual connections as needed and
#' translate queries into a variety of different database lanaguages, so the
#' code is database-independent.
#'
#' You do not need to open and close connections constantly.  You can leave a
#' connection open as long as you want (unless the database kicks you off), but
#' it's best to close it when you are done for the day.
#'
#' @param host The address of the database host.
#' @param port The port on which to access the database.
#' @param dbname The name of the database to access.
#' @param username Your username to access the database.
#' @param password Your password to access the database. Generally, passwords
#'   should not be saved in plain text code files. Thus, the default is to
#'   prompt the user interactively.
#' @param driver Driver specifying communication protocol for the type of
#'   database being used. See dbplyr for supported databases.
#' @name dbcon
#' @examples
#' \dontrun{
#'
#' ## Start a session that will pass connection object for you
#'
#' # You will be prompted for password
#' start_db_session(
#'   host = "localhost",
#'   port = 3306,
#'   dbname = "moodle",
#'   username = "moodle-ro"
#' )
#'
#' # Do a bunch of stuff
#' fetch_enrolled_users(72)
#'
#' # End the session when you are done for the day
#' end_db_session()
#'
#' ## Create pool connection object you will pass yourself
#'
#' # You will be prompted for password
#' con <- create_db_con(
#'   host = "localhost",
#'   port = 3306,
#'   dbname = "moodle",
#'   username = "moodle-ro"
#' )
#'
#' # Do a bunch of stuff
#' fetch_enrolled_users(72, con = con)
#'
#' # Close the connection when you are done for the day
#' pool::poolClose(con)
#' }
NULL


#' @rdname dbcon
#' @return \code{create_db_con} returns a dbplyr pool connection object.
#' @export
create_db_con <- function(host, port, dbname, username,
                       password = getPass::getPass(msg="Database password:"),
                       driver = RMySQL::MySQL()) {
  pool::dbPool(
    drv = driver,
    host = host,
    port = port,
    dbname = dbname,
    username = username,
    password = password
  ) %>%
    return()
}

#' @rdname dbcon
#' @return \code{start_db_session} silently returns the old, closed session
#'   connection, or NULL if there was not a previous connection.
#' @export
start_db_session <- function(host, port, dbname, username,
                        password = getPass::getPass(msg="Database password:"),
                        driver = RMySQL::MySQL()) {
  set_session_con(
    create_db_con(
      host = host,
      port = port,
      dbname = dbname,
      username = username,
      password = password,
      driver = driver
    )
  ) %>%
    invisible()
}

#' @rdname dbcon
#' @return \code{end_db_session} silently returns the session connection after
#'   closing it, or NULL if there was no connection.
#' @export
end_db_session <- function() {
  set_session_con(NULL) %>%
    invisible()
}

#' @rdname dbcon
#' @return \code{get_session_con} returns the current session's connection
#'   object.
#' @export
get_session_con <- function() {
  return(db_session$con)
}

#' @rdname dbcon
#' @param con A database connection object.
#' @return \code{set_session_con} returns the previous session's connection
#'   object after replacing the session connection.
#' @export
set_session_con <- function(con) {
  if (!is.null(db_session$con))
    pool::poolClose(db_session$con)
  old <- db_session$con
  db_session$con <- con
  invisible(old)
}

