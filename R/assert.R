#' Build intuitive assertion calls
#'
#' @param message (character) The error message to throw if the assertion fails
#' @param ... (boolean) Statements that evaluate to TRUE or FALSE
#'
#' @return None
#'
#' @export
assert <- function(message, ...) {
  if(!all(...)) stop(message, call. = FALSE)
}


#' Build intuitive warning messages
#'
#' @param message (character) The error message to throw if the assertion fails
#' @param ... (boolean) Statements that evaluate to TRUE or FALSE
#'
#' @return None
#'
#' @export
warn <- function(message, ...) {
  if(!all(...)) stop(message, call. = FALSE)
}
