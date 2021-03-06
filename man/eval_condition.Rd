% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/availability.R
\name{eval_condition}
\alias{eval_condition}
\title{If x is a terminating branch (has only leaves as children), then evaluate
those children, apply operator, and return value.  Otherwise, recursively
evaluate any children that are branches, then evaluate children that are
leaves, apply operator, and return value.}
\usage{
eval_condition(x, pointer, user.id, course.id)
}
\description{
If x is a terminating branch (has only leaves as children), then evaluate
those children, apply operator, and return value.  Otherwise, recursively
evaluate any children that are branches, then evaluate children that are
leaves, apply operator, and return value.
}
\keyword{internal}
