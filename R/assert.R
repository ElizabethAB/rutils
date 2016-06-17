#' Build intuitive assertion calls
#'
#' @param message (character) The error message to throw if the assertion fails
#' @param ... (boolean) Statements that evaluate to TRUE or FALSE
#'
#' @return None
#'
#' @export
assert <- function(message, ...) {
  try(if(!all(...)) stop(message, call. = FALSE))
}
