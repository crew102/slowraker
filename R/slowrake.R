# Although all of the code in slowrake_atomic is vectorized (and thus could be
# applied to a vector of txt instead of an element of txt), we still use a for
# loop so we can see progress of slowrake.
#
# Comments refer to tokens as words (e.g., "hi" is a word) and refer to
# contiguous sequences of tokens (i.e., phrases) as keywords (e.g., "hi there"
# could be a keyword).
slowrake_atomic <- function(txt, stop_words, word_min_char, stem, stop_pos,
                            word_token_annotator, pos_annotator, phrase_delims) {
  # Make sure there is at least one phrase delimitor in the txt
  txt <- paste0(txt, ".")

  # Make sure there is an alpha char in text before filtering based on POS
  if (!grepl("[[:alpha:]]", txt)) return(NA)

  # Remove words based on their POS
  if (!is.null(stop_pos)) {
    # Suggest how to solve an 'out of memory' Java error if it is thrown
    tryCatch(
      pos_word_df <- get_pos_tags(
        txt = txt,
        word_token_annotator = word_token_annotator,
        pos_annotator = pos_annotator
      ),
      error = handle_pos_error
    )
    txt <- stop_pos_tags(pos_word_df = pos_word_df, stop_pos = stop_pos)
  }

  txt <- tolower(txt)
  # Split txt into list of keywords based on stop words/phrase delims
  cand_words <- get_cand_words(
    txt = txt, stop_words = stop_words, phrase_delims = phrase_delims
  )
  # Filter out words that are too short
  cand_words <- filter_words(
    cand_words = cand_words, word_min_char = word_min_char
  )

  # Make sure we still have at least one keyword
  if (length(cand_words) == 0) return(NA)

  # Convert word vectors into keywords (a word vector contains the words in a
  # keyword)
  keyword <- vapply(cand_words, function(x)
    paste0(x, collapse = " "), character(1))

  if (stem) cand_words <- lapply(cand_words, SnowballC::wordStem)

  # Calculate keyword scores
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

  # Create output data frame
  process_keyword_df(keyword_df = keyword_df)
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
#' @param phrase_delims A regular expression containing the characters that
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
                     phrase_delims = "[-,.?():;\"!/]") {

  num_docs <- length(txt)
  one_doc <- num_docs == 1
  if (!one_doc)
    prog_bar <- utils::txtProgressBar(min = 0, max = num_docs, style = 3)

  all_out <- vector(mode = "list", length = num_docs)

  if (!is.null(stop_pos)) {
    pos_annotator <- openNLP::Maxent_POS_Tag_Annotator()
    word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  }

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

  structure(
    all_out,
    class = c(class(all_out), "rakelist")
  )
}