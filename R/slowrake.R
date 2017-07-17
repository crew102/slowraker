slowrake_atomic <- function(txt, stop_words, word_min_char, stem, keep_pos) {

  if (!is.null(keep_pos))
    txt <- filter_pos_tags(txt = txt, keep_pos = keep_pos)

  cand_vec <- gen_candidates(txt = txt, stop_words = stop_words,
                             word_min_char = word_min_char)

  keyword <- sapply(cand_vec, function(x) paste0(x, collapse = " "))

  # get a list of unique words per keyword, so we don't double count (e.g.,
  # keyword like "vector times vector")
  if (stem)
    cand_vec <- lapply(cand_vec, SnowballC::wordStem)

  wrd_cnts <- gen_word_cnts(cand_vec = cand_vec)

  non_diag_deg <- gen_non_diag_deg(wrd_cnts = wrd_cnts, cand_vec = cand_vec)

  word_scores <- calc_scores(wrd_cnts = wrd_cnts, non_diag_deg = non_diag_deg)

  # add word scores for each (non-distinct) keyword
  score <- sapply(cand_vec, function(x) sum(word_scores[x]))

  keyword_df <- data.frame(
    keyword = keyword,
    score = score,
    stringsAsFactors = FALSE
  )

  if (stem)
    keyword_df$keyword_stem <- sapply(cand_vec, function(x) paste0(x, collapse = " "))

  process_keyword_df(keyword_df = keyword_df)
}

#' Slow RAKE
#'
#' @param txt A character vector, where each element of the vector contains the text for one document.
#' @param stop_words A vector of stop words, which will be removed from your corpus prior to the keyword extraction algorithm being applied. The default value (\code{smart_words}) is the "SMART" vector of stop words (equivilent to \code{\link[tm]{stopwords("SMART")}}).
#' @param word_min_char The minimum number of characters that a word must have to remain in the corpus. Words with fewer than \code{word_min_char} characters will be removed prior before keyword extraction is conducted.
#' @param stem Do you want to stem the words in your corpus before extracting keywords?
#' @param keep_pos Which part-of-speech (POS) are accepitble for keywords to have? This should be a vector of POS tags, containing all parts-of-speech that words can take on, in order to be considered as a keyword. The default value \code{c("NN", "NNS", "NNP", "NNPS")} has all POS tags for noun parts of speech. To see the other possible POS tags along with their descriptions, see the \code{\link{pos_tags}} vector (\code{slowraker::pos_tags}) or see their descriptions in \href{Part-Of-Speech Tagging with R}{http://martinschweinberger.de/docs/articles/PosTagR.pdf}.
#'
#' @return A list of data frames, with one data frame per document. Each data frame will have the following columns:
#'  \describe{
#'    \item{keyword}{A keyword that was identified by RAKE. This list is non-distinct, meaning that a keyword can appear in this column nice if it appears in the text twice.}
#'
#'    \item{freq}{The number of times the keyword appears in the document}
#'
#'    \item{score}{The keyword's score, as per the RAKE algorithm. Keywords with higher scores are considered to be more higher quality than those with lower scores.}
#'
#'    \itemP{keyword_stem}{If you specified \code{stem = TRUE}, you will also get the stemmed versions of all of the keywords. This column contains those stemmed versions.}
#'  }
#' @export
#'
#' @examples
slowrake <- function(txt, stop_words = smart_words, word_min_char = 3,
                     stem = TRUE, keep_pos = c("NN", "NNS", "NNP", "NNPS")) {

  num_docs <- length(txt)
  all_out <- vector(mode = "list", length = num_docs)
  prog_bar <- txtProgressBar(min = 0, max = num_docs, style = 3)

  for (i in seq_along(txt)) {
    all_out[[i]] <- slowrake_atomic(txt = txt[i], stop_words = stop_words,
                                    word_min_char = word_min_char, stem = stem,
                                    keep_pos = keep_pos)
    setTxtProgressBar(prog_bar, i)
  }

  all_out
}