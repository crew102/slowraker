#' rbind a rakelist
#'
#' @param rakelist An object of class \code{rakelist}, which you create by
#'   calling \code{\link{slowrake}}.
#' @param doc_id An optional vector of document IDs, which should be the same
#'   length as \code{rakelist}. These IDs will be added to the resulting data
#'   frame.
#'
#' @return A single data frame which contains the documents' keywords. The
#'   \code{doc_id} column in this data frame denotes which document a keyword
#'   was found in.
#'
#' @export
#'
#' @examples
#' rakelist <- slowrake(txt = dog_pubs$abstract[1:2])
#'
#' # Without specifying doc_id:
#' rbind_rakelist(rakelist = rakelist)
#' # With specifying doc_id:
#' rbind_rakelist(rakelist = rakelist, doc_id = dog_pubs$doi[1:2])
rbind_rakelist <- function(rakelist, doc_id = NULL) {
  asrt("rakelist" %in% class(rakelist), "rakelist must be an object of class ",
       "'rakelist'. See examples.")

  if (is.null(doc_id)) {
    doc_id <- as.character(seq_along(rakelist))
  } else {
    asrt(length(rakelist) == length(unique(doc_id)), "doc_id must have the ",
         "the same number of distinct elements as rakelist")
  }

  numeric_ids <- is.numeric(doc_id)
  if (numeric_ids)
    doc_id <- as.character(doc_id)

  names(rakelist) <- doc_id

  doc_ids_repped <- lapply(doc_id, function(x) {
    rws <- nrow(rakelist[[x]])
    rep(x, ifelse(is.null(rws), 1, rws))
  })

  df <- do.call("rbind", rakelist)
  doc_id <- unlist(doc_ids_repped)

  if (numeric_ids)
    doc_id <- as.numeric(doc_id)

  cbind(doc_id, df, stringsAsFactors = FALSE, row.names = NULL)
}