# Takes a vector to be "translated", a dataframe dict, and the column
# names in the dict which map to the orig (or to be searched terms)
# and the replacements

# Input to indices of tmp for those indices of str_vector where pmatch
# identifies a match from dict$orig the corresponding value of
# dict$tran, so that
# `pmatch(c("northeast", "unit", "apt", "test"), dict$orig)` returns
# c(3, NA, 10, NA) where 1 and 3 the index of dict$orig where it finds the match
# and NA where none or ambiguous.

strSwap <- function(str_vector, dict, orig = "orig", tran = "tran") {
  tmp <- as.character(dict[, tran][pmatch(str_vector, dict[,orig])])
  str_vector[!is.na(tmp)] <- tmp[!is.na(tmp)]
  str_vector
}
