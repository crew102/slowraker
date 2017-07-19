#' rbind a rakelist
#'
#' @param rakelist A object of class \code{rakelist}, which you create by calling \code{\link{slowrake}}.
#' @param doc_id An optional vector of document IDs, which should be the same length as \code{rakelist}. These IDs will be added to results data frame.
#'
#' @return
#' @export
#'
#' @examples
rbind_rakelist <- function(rakelist, doc_id = NULL) {
  asrt("rakelist" %in% class(rakelist), "rakelist must be an object of class ",
       "'rakelist'. See examples.")

  if (is.null(doc_id))
    doc_id <- as.character(seq_along(rakelist))
  names(rakelist) <- doc_id

  doc_ids_repped <- lapply(doc_id, function(x) {
    rws <- nrow(rakelist[[x]])
    rep(x, ifelse(is.null(rws), 1, rws))
  })

  df <- do.call("rbind", rakelist)
  doc_id <- unlist(doc_ids_repped)
  cbind(doc_id, df, stringsAsFactors = FALSE, row.names = NULL)
}