# While str() and summary() are great for an analyst's understanding of a
# dataframe, they are not "e-mail friendly." I can't e-mail them to a client to
# confirm the data's condition matches their expectations. The emailSummary
# function is a pretty print utility for corporate data analysts.

emailSummary <- function(dframe, dframe_name = "data") {
  obs <- nrow(dframe)
  cols <- colnames(dframe)
  paste("The", dframe_name, "matches")
}