#' @export
print.rakelist <- function(x, ...) {
  num_docs <- length(x)
  up_ind <- ifelse(num_docs == 1, 1, 2)

  cat(
    "#### A list of", as.character(num_docs), "data frames:\n"
  )

  utils::str(x[1:up_ind], vec.len = 1, max.level = 2, give.attr = FALSE,
             digits.d = 1, strict.width = "cut", give.head = FALSE,
             no.list = TRUE)

  if (num_docs > 2) {
    frames <- ifelse(num_docs == 3, "frame.", "frames.")
    cat("#...With", as.character(num_docs - 2), "more data", frames)
  }
}