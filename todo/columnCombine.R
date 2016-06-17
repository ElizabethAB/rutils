# Provided a data frame, the columns (as a string vector) to be combined, and
# the name of the combined column on output, collapses the columns.
# Useful when reconciling related data frames with inconsistent column naming.
columnCombine <- function(dframe, cols, out = NULL) {
  if (is.null(out)) out <- cols[1]
  if (length(cols) == 1) {
    colnames(dframe)[which(colnames(dframe) == cols)] <- out
    return(dframe)
  }

  vals <- lapply(dframe[,cols], function(x) which(!is.na(x)))
  overlap <- Reduce(c, vals)
  overlap <- unique(overlap[duplicated(overlap)])
  final <- rep(NA, nrow(dframe))

  for (name in names(vals)) {
    final[vals[[name]]] <- dframe[vals[[name]], name]
  }
  if (length(overlap) > 0) {
    final[overlap] <- .collapseColumns(dframe[overlap, cols], out)
  }

  dframe <- dframe[,!(colnames(dframe) %in% cols)]
  dframe[,out] <- final
  dframe
}

.collapseColumns <- function(cols, out) {
  cols <- if (nrow(cols) == 1) {
    list(na.omit(unlist(cols[1,,drop = TRUE])))
  } else {
    apply(cols, 1, function(x) x[!is.na(x)])
  }
  conflicts <- sum(unlist(lapply(cols, length)) > 1)
  warning(paste("There are", conflicts, "conflicting records in", out))
  sapply(cols, function(x) names(which.max(table(x))))
}
