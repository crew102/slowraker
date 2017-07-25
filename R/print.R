#' @export
print.rakelist <- function(x, ...) {
  df <- x[[1]]
  num_docs <- length(x)

  cat(
    "#### A list of", as.character(num_docs), "data frames,",
    "where each data frame contains a document's keywords. E.g.:\n\n"
  )
  print(tail(df, 3), row.names = FALSE)
}