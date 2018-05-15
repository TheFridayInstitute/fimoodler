

# Constant ----------------------------------------------------------------


CONTEXT_LEVELS <- dplyr::tribble(
  ~contextlevel, ~contextname,
  10, "CONTEXT_SYSTEM",
  30, "CONTEXT_USER",
  40, "CONTEXT_COURSECAT",
  50, "CONTEXT_COURSE",
  70, "CONTEXT_MODULE",
  80, "CONTEXT_BLOCK"
)

SAFE_UINFO_FIELDS <- c(8, 17, 18, 19, 20, 21, 22, 23)


# Mutable --------------------------------------------------------------

db_session <- new.env(parent = emptyenv())
db_session$con <- NULL




