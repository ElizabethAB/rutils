#' Compare two dataframes for their differences
#'
#' Creates a minimal output for easy perusal of where two datasets differ.
#' Useful for determining whether two datasets are "meaningfully" different,
#' where that judgement is subjective, by providing the user minimal information
#' to evaluate.
#'
#' Useful for confirming reproducible analysis or on receiving updated data from
#' a different party.
#'
#' Assumptions made:
#'  * The ID keys are unique
#'  * Shared columns share the same name
#'  * Factors should be compared as characters, not numerics
#'
#' @param df1 (data.frame) The first dataset
#' @param df2 (data.frame) The second dataset
#' @param ids (character vector) Columns that uniquely identify observations in
#'  the datasets to be compared
#' @param comp_cols = NULL
#'
#' @return (list) Containing three named objects:
#'   * "differences_df" - (data.frame) Contains the ID columns, columns from df1
#'      and df2 (marked by postscript, e.g. "_df1") where any discrepancy was
#'      found, and rows where a discrepancy was found. Only cells with
#'      identified discrepancies are populated.
#'   * "id_overlap" - (list) A named list containing character vectors
#'      describing the union and set differences of ID values:
#'      * "ids_in_both"
#'      * "ids_in_df1"
#'      * "ids_in_df2"
#'   * "column_overlap" - (list) A named list containing character vectors
#'      describing the union and set differences of column values:
#'      * "columns_in_both"
#'      * "columns_in_df1"
#'      * "columns_in_df2"
#'
#' @export
dframeCompare <- function(df1, df2, ids, comp_cols = NULL, ...) {

  # Confirm form of inputs
  assert("ids must be a string vector", is.character(ids))
  assert("df1 must be a dataframe", is.data.frame(df1))
  assert("df2 must be a dataframe", is.data.frame(df2))

  # Confirm ID columns are present in both dataframes
  assert("ID columns not found in df1", ids %in% colnames(df1))
  assert("ID columns not found in df2", ids %in% colnames(df2))

  # Confirm ID columns produce unique observations
  assert("ID is non-unique in df1", sum(duplicated(df1[,ids])) == 0)
  assert("ID is non-unique in df2", sum(duplicated(df2[,ids])) == 0)

  if (is.null(comp_cols)) {
    comp_cols <- intersect(colnames(df1), colnames(df2))
  }
  comp_cols <- setdiff(comp_cols, c(ids))

  # Track overlap of columns and ID values
  ol_col <- compareContains(colnames(df1), colnames(df2), "columns")
  ol_ids <- compareContains(df1$.id, df2$.id, "id")

  compare <- merge(df1[,c(ids, comp_cols)], df2[,c(ids, comp_cols)],
                   by = ids, all = FALSE, suffixes = c("_df1", "_df2"))

  dif_df <- lapply(comp_cols, function(column) {
    difs <- columnDif(compare[,paste0(column, "_df1")],
                       compare[,paste0(column, "_df2")])
    if (length(difs) > 0) {
      compare[difs, c(ids, paste0(column, c("_df1", "_df2"))), drop = FALSE]
    } else {
      compare[difs, ids, drop = FALSE]
    }
  })
  dif_df <- Reduce(function(x, y) merge(x, y, all = TRUE, by = ids),
                   dif_df[!is.null(dif_df)])

  list("differences_df" = dif_df,
       "id_overlap" = ol_ids,
       "column_overlap" = ol_col)
}

columnDif <- function(vector1, vector2, clean = TRUE, decimals = 2) {
  if (class(vector1) == "factor") vector1 <- as.character(vector1)
  if (class(vector2) == "factor") vector2 <- as.character(vector2)
  value_match <- if (clean) {
    if (is.numeric(vector1) & is.numeric(vector2)) {
      round(vector1, decimals) == round(vector2, decimals)
    } else if (is.character(vector1) & is.character(vector2)) {
      stringr::str_trim(tolower(gsub("\\s+", " ", vector1))) ==
        stringr::str_trim(tolower(gsub("\\s+", " ", vector2)))
    }
  } else vector1 == vector2
  value_match[is.na(value_match)] <- FALSE
  value_match <- value_match | (is.na(vector1) & is.na(vector2))

  which(!value_match)
}

compareContains <- function(vector1, vector2, value_name = "") {
  tmp <- list()
  tmp[[paste0(value_name, "_in_both")]] <- intersect(vector1, vector2)
  tmp[[paste0(value_name, "_in_df1")]] <- setdiff(vector1, vector2)
  tmp[[paste0(value_name, "_in_df2")]] <- setdiff(vector2, vector1)
  tmp
}
