#' rbind a rakelist
#'
#' @param rakelist An object of class \code{rakelist}, which you create by
#'   calling \code{\link{slowrake}}.
#' @param doc_id An optional vector of document IDs, which should be the same
#'   length as \code{rakelist}. These IDs will be added to the resulting data
#'   frame.
#'
#' @return A single data frame which contains all documents' keywords. The
#'   \code{doc_id} column tells you which document a keyword was found in.
#'
#' @export
#'
#' @examples
#' rakelist <- slowrake(txt = dog_pubs$abstract[1:2])
#'
#' # Without specifying doc_id:
#' rbind_rakelist(rakelist = rakelist)
#'
#' # With specifying doc_id:
#' rbind_rakelist(rakelist = rakelist, doc_id = dog_pubs$doi[1:2])
rbind_rakelist <- function(rakelist, doc_id = NULL) {
  asrt(
    "rakelist" %in% class(rakelist),
    "rakelist must be an object of class 'rakelist'. See examples."
  )

  if (is.null(doc_id)) {
    doc_id <- seq_along(rakelist)
  } else {
    asrt(
      length(rakelist) == length(doc_id),
    "doc_id must have the same number of elements as your rakelist"
    )
  }

  num_rows <- vapply(rakelist, function(x)
    if (is.data.frame(x)) nrow(x) else 1, numeric(1))

  doc_id <- rep(doc_id, num_rows)

  df <- do.call("rbind", rakelist)

  cbind(doc_id, df, stringsAsFactors = FALSE, row.names = NULL)
}