update <- function(aggr, curr, id) {
  cols <- colnames(curr)
  cols <- cols[cols != id]
  aggr[,cols] <- NA
  aggr[match(curr[,id], aggr[,id]), cols] <- curr[,cols]
  aggr
}
