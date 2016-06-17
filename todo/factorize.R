# Utility function to convert related string values into a single level of a
# factor. Requires user caution that patterns do not match to replacements or
# that an individual record would match to multiple patterns.

factorize <- function(col, dict, pat = "pat", rep = "rep") {
  internal <- sapply(dict[,pat], function(pat) grepl(pat, dict[,rep]))
  assert("Patterns should not match to multiple replacements.",
         !internal[lower.tri(internal)])
  matches <- sapply(dict[,pat], function(pat) which(grepl(pat, col)))
  assert("Records should match to only one pattern.",
         sapply(matches, function(r) length(r) <= 1))
  for (i in 1:length(matches)) {
    col[matches[[i]]] <- dict[i, rep]
  }
  col[setdiff(1:length(col), unlist(matches))] <- "unknown"
  factor(col)
}
