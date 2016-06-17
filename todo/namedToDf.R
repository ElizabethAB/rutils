# Converts a named vector to a two-column data frame (useful for writing
# readable code, where the association between two items is visually obvious
# for anyone reviewing).

namedToDf <- function(named_vect, name_col = "Name", vect_col = "Vector") {
  tmp <- data.frame(names(named_vect), unname(named_vect))
  colnames(tmp) <- c(name_col, vect_col)
  tmp
}
