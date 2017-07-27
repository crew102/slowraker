# Although all of the code in slowrake_atomic is vectorized (and thus could be
# applied to a vector of txt instead of an atomic element of txt), we still
# choose to loop over elements of txt so that we can see progress of slowrake.
slowrake_atomic <- function(txt, stop_words, word_min_char, stem, filter_pos) {

  # Make sure there is at least one phrase delimitor in the txt
  txt <- paste0(txt, ".")

  # Make sure there is an alpha char in text before filtering based on POS (pos)
  if (!grepl("[[:alpha:]]", txt))
    return(NA)

  # Remove words based on their POS
  if (!is.null(filter_pos)) {
    pos_word_df <- get_pos_tags(txt = txt)
    txt <- filter_pos_tags(pos_word_df = pos_word_df, filter_pos = filter_pos)
  }

  txt <- tolower(txt)
  # Split txt into list of keywords based on stopwords/phrase delims
  cand_words <- get_cand_words(txt = txt, stop_words = stop_words)
  # Filter out words that are too short
  cand_words <- filter_words(cand_words = cand_words,
                             word_min_char = word_min_char)

  # Make sure we still have at least one keyword
  if (length(cand_words) == 0) return(NA)

  # Convert word vectors (one vector = words in a keyword) into keywords
  keyword <- vapply(cand_words, function(x)
    paste0(x, collapse = " "), character(1))

  if (stem)
    cand_words <- lapply(cand_words, SnowballC::wordStem)

  # Calculate keyword-level scores
  score <- calc_keyword_scores(cand_words = cand_words)

  keyword_df <- data.frame(
    keyword = keyword,
    score = score,
    stringsAsFactors = FALSE
  )

  # Convert stemmed versions of word vectors into keywords
  if (stem)
    keyword_df$stem <- vapply(cand_words, function(x)
      paste0(x, collapse = " "), character(1))

  # Create output data frames
  process_keyword_df(keyword_df = keyword_df)
}

#' Slow RAKE
#'
#' A relatively slow version of the Rapid Automatic Keyword Extraction (RAKE)
#' algorithm. See \href{http://media.wiley.com/product_data/excerpt/22/04707498/0470749822.pdf}{Automatic keyword extraction from individual documents} for details on
#' how RAKE works.
#'
#' @param txt A character vector, where each element of the vector contains the
#'   text for one document.
#' @param stop_words A vector of stop words which will be removed from your
#'   documents. The default value (\code{smart_words}) contains the 'SMART' stop
#'   words (equivalent to
#'   \href{https://rdrr.io/rforge/tm/man/stopwords.html}{tm::stopwords('SMART')})
#'   . A value of \code{NULL} indicates that no stop words will be removed.
#' @param filter_pos The parts-of-speech (POS) that should be filtered from
#'   the documents prior to keyword extraction. In other words, if you include
#'   a part-of-speech in \code{filter_pos}, then all words that are assigned to
#'   that POS will be removed prior to the RAKE algorithm. \code{filter_pos}
#'   should be a vector of POS tags. To see all POS tags along with their
#'   definitions (i.e., the POS that the tag denotes),see the
#'   \code{\link{pos_tags}} data frame (\code{View(slowraker::pos_tags)}). The
#'   default is to remove all parts-of-speech that are verbs (e.g.,
#'   \code{filter_pos = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")}). Set
#'   \code{filter_pos = NULL} if you don't want to remove any words based on
#'   its part-of-speech.
#' @param word_min_char The minimum number of characters that a word must have
#'   to remain in the corpus. Words with fewer than \code{word_min_char}
#'   characters will be removed before the RAKE algorithm is applied. Also note
#'   that filtering words based on \code{word_min_char} happens before stemming,
#'   so you should not consider the length of a word's stem when choosing
#'   \code{word_min_char}.
#' @param stem Do you want to stem the words in your documents before extracting
#'   keywords?
#' @return An object of class \code{rakelist}, which is just a list of data
#'   frames (one data frame per document/element of \code{txt}). Each data frame
#'   will have the following columns:
#'   \describe{
#'     \item{keyword}{A keyword that was identified by RAKE.}
#'     \item{freq}{The number of times the keyword appears in the document}
#'     \item{score}{The keyword's score, as per the RAKE algorithm. Keywords
#'     with higher scores are considered to be more higher quality, as compared
#'     to those with lower scores.}
#'     \item{stem}{If you specified \code{stem = TRUE}, you will also get the
#'     stemmed versions of the keywords in this column. When you choose stemming,
#'     the keyword's score (\code{score}) will be based off of its stem, but the
#'     reported number of times that the keyword appears (i.e., \code{freq})
#'     will still be based off of the raw, unstemmed version of the keyword.}
#'   }
#'
#' @export
#'
#' @examples
#' slowrake(txt = dog_pubs$title[1:5])
#'
#' slowrake(txt = dog_pubs$title[1:5], stem = FALSE)
#'
#' # Only consider words that are plural nouns (NNS) when creating candidate
#' # keywords:
#' slowrake(txt = c("dogs are the best, don't you think?",
#'                  "some people think dogs are the best, but i love my cat."),
#'          filter_pos = pos_tags$tag[!(pos_tags$tag == "NNS")])
#'
#' # ...Now only consider singular nouns:
#' slowrake(txt = c("dogs are the best, don't you think?",
#'                  "some people think dogs are the best, but i love my cat."),
#'          filter_pos = pos_tags$tag[!(pos_tags$tag == "NN")])
#'
#' # Removing "dogs" in this txt means we no longer have any candidate keywords:
#' slowrake(txt = "dogs are the best, don't you think?",
#'          stop_words = "dogs")
#'
#' # Don't filter words on POS or based on membership in stop_words:
#' slowrake(txt = c("hi there. dogs are the best, don't you think?"),
#'          filter_pos = NULL, stop_words = NULL)
slowrake <- function(txt, stop_words = smart_words,
                     filter_pos = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
                     word_min_char = 3, stem = TRUE) {

  num_docs <- length(txt)
  all_out <- vector(mode = "list", length = num_docs)
  prog_bar <- utils::txtProgressBar(min = 0, max = num_docs, style = 3)

  for (i in seq_along(txt)) {
    all_out[[i]] <- slowrake_atomic(txt = txt[i], stop_words = stop_words,
                                    word_min_char = word_min_char, stem = stem,
                                    filter_pos = filter_pos)
    utils::setTxtProgressBar(prog_bar, i)
  }

  structure(
    all_out,
    class = c(class(all_out), "rakelist")
  )
}