# Relies on all values fed to be "convertible" to a dataframe and that any
# valuable naming convention is stored as names() or colnames()

# Useful for turning a list of useful output into a matrix for quick csv
# writeout and prettifying in Excel.

lstToMatrix <- function(lst, title = "") {
  cols <- max(sapply(names(lst), function(nm) {
    ncol(data.frame(lst[[nm]]))
  }))

  output <- matrix(ncol = cols, nrow = 1, c(title, rep(NA, cols - 1)))

  for (nm in names(lst)) {
    df <- lst[[nm]]
    df <- lapply(df, function(x) {
      if (is.factor(x)) as.character(x)
      else x
    })
    vect <- unlist(df)
    df_nm <- if (is.null(names(df))) {colnames(df)}
    else {names(df)}
    na_elements <- nrow(data.frame(df)) * cols - length(vect)
    output <- rbind(output, NA)
    output <- rbind(output, c(nm, rep(NA, cols - 1)))
    output <- rbind(output, c(df_nm, rep(NA, cols - ncol(data.frame(df)))))
    if (na_elements > 0) {
      output <- rbind(output,
                      matrix(c(vect, rep(NA, na_elements)), ncol = cols))
    } else {
      output <- rbind(output, matrix(vect, ncol = cols))
    }
  }
  output
}
