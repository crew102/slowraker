# Although all of the code in slowrake_atomic is vectorized (and thus could be
# applied to a vector of txt instead of an element of txt), we still use a for
# loop so we can see progress of slowrake.
slowrake_atomic <- function(txt, stop_words, word_min_char, stem, stop_pos,
                            word_token_annotator, pos_annotator, phrase_delims) {
  txt <- paste0(txt, ".")

  if (!grepl("[[:alpha:]]", txt)) return(NA)

  if (!is.null(stop_pos)) {
    tryCatch(
      pos_word_df <- get_pos_tags(txt, word_token_annotator, pos_annotator),
      error = handle_pos_error
    )
    txt <- stop_pos_tags(pos_word_df, stop_pos)
  }

  txt <- tolower(txt)
  cand_words <- get_cand_words(txt, stop_words, phrase_delims)
  cand_words <- filter_words(cand_words, word_min_char)

  # drop dashes. have to do this at this point instead of sooner b/c we want to
  # apply min word length filter on the complete hyphenated word, not on each
  # component word. note: we are still limited by fact that single letters are
  # in list of stopwords and r thinks that - is a word boundary, so the "k" in
  # "k-means" will be dropped.
  cand_words <- split_hyphenated_words(cand_words)

  if (length(cand_words) == 0) return(NA)

  # Convert word vectors into keywords (a word vector contains the words in a
  # keyword)
  collapse <- function(x) paste0(x, collapse = " ")
  keyword <- vapply(cand_words, collapse, character(1))

  if (stem) cand_words <- lapply(cand_words, SnowballC::wordStem)

  score <- calc_keyword_scores(cand_words)

  keyword_df <- data.frame(
    keyword = keyword,
    score = score,
    stringsAsFactors = FALSE
  )

  if (stem)
    keyword_df$stem <- vapply(cand_words, collapse, character(1))

  process_keyword_df(keyword_df)
}

#' Slow RAKE
#'
#' A relatively slow version of the Rapid Automatic Keyword Extraction (RAKE)
#' algorithm. See \href{http://media.wiley.com/product_data/excerpt/22/04707498/0470749822.pdf}{Automatic keyword extraction from individual documents} for
#' details on how RAKE works or read the "Getting started" vignette (
#' \code{vignette("getting-started")}).
#'
#' @param txt A character vector, where each element of the vector contains the
#'   text for one document.
#' @param stop_words A vector of stop words which will be removed from your
#'   documents. The default value (\code{smart_words}) contains the 'SMART' stop
#'   words (equivalent to
#'   \href{https://rdrr.io/rforge/tm/man/stopwords.html}{tm::stopwords('SMART')})
#'   . Set \code{stop_words = NULL} if you don't want to remove stop words.
#' @param stop_pos All words that have a part-of-speech (POS) that appears in
#'   \code{stop_pos} will be considered a stop word. \code{stop_pos} should be a
#'   vector of POS tags. All possible POS tags along with their definitions are
#'   in the \code{\link{pos_tags}} data frame (\code{View(slowraker::pos_tags)}).
#'   The default value is to remove all words that have a verb-based
#'   POS (i.e., \code{stop_pos = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")}).
#'   Set \code{stop_pos = NULL} if you don't want a word's POS to matter during
#'   keyword extraction.
#' @param word_min_char The minimum number of characters that a word must have
#'   to remain in the corpus. Words with fewer than \code{word_min_char}
#'   characters will be removed before the RAKE algorithm is applied. Note
#'   that removing words based on \code{word_min_char} happens before stemming,
#'   so you should consider the full length of the word and not the length of
#'   its stem when choosing \code{word_min_char}.
#' @param stem Do you want to stem the words before running RAKE?
#' @param phrase_delims A regular expression containing the patterns that
#' will be used as phrase delimiters.
#'
#' @return An object of class \code{rakelist}, which is just a list of data
#'   frames (one data frame for each element of \code{txt}). Each data frame
#'   will have the following columns:
#'   \describe{
#'     \item{keyword}{A keyword that was identified by RAKE.}
#'     \item{freq}{The number of times the keyword appears in the document.}
#'     \item{score}{The keyword's score, as per the RAKE algorithm. Keywords
#'     with higher scores are considered to be higher quality than those with
#'     lower scores.}
#'     \item{stem}{If you specified \code{stem = TRUE}, you will get the
#'     stemmed versions of the keywords in this column. When you choose stemming,
#'     the keyword's score (\code{score}) will be based off its stem,
#'     but the reported number of times that the keyword appears (\code{freq})
#'     will still be based off of the raw, unstemmed version of the keyword.}
#'   }
#'
#' @export
#'
#' @examples
#' slowrake(txt = "some text that has great keywords")
#'
#' slowrake(txt = dog_pubs$title[1:2], stem = FALSE)
slowrake <- function(txt,
                     stop_words = smart_words,
                     stop_pos = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
                     word_min_char = 3,
                     stem = TRUE,
                     phrase_delims = "[[:space:]]-[[:space:]]|[,.?():;\"!/]|]|\\[") {

  num_docs <- length(txt)
  one_doc <- num_docs == 1

  if (!is.null(stop_pos)) {
    pos_annotator <- openNLP::Maxent_POS_Tag_Annotator()
    word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  }

  if (!one_doc)
    prog_bar <- utils::txtProgressBar(min = 0, max = num_docs, style = 3)

  all_out <- vector(mode = "list", length = num_docs)
  for (i in seq_along(txt)) {
    all_out[[i]] <- slowrake_atomic(
      txt = txt[i],
      stop_words = stop_words,
      word_min_char = word_min_char,
      stem = stem,
      stop_pos = stop_pos,
      pos_annotator = pos_annotator,
      word_token_annotator = word_token_annotator,
      phrase_delims = phrase_delims
    )
    if (!one_doc) utils::setTxtProgressBar(prog_bar, i)
  }

  structure(all_out, class = c(class(all_out), "rakelist"))
}