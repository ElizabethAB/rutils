#' Named and numbered list of values
#'
#' Function to keep a named list of recorded values; useful when performing
# data validations (i.e., "X observations dropped due to variable Y missing,"
# "the range of X before and after a modification," etc.)
#'
#' @return (function) A function that receives two arguments, `value` and
#'    `val_name`. Calling the function with arguments populates a named list,
#'    where the name of the object is `val_name` and the stored value is `val`.
#'    This function can be called without arguments to return the named list.
#'
#' @export
tracker <- function() {
  .record <- list()
  function(value = NULL, val_name = NULL) {
    curr <- get(".record")
    if (is.null(value)) return(curr)
    curr[[val_name]] <- value
    .record <<- curr
    invisible(.record)
  }
}
