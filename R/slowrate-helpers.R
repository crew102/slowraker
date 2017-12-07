# get_pos_tags adapted from:
# http://martinschweinberger.de/docs/articles/PosTagR.pdf
get_pos_tags <- function(txt, word_token_annotator, pos_annotator) {
  str_txt <- NLP::as.String(txt)
  a2 <- NLP::Annotation(1L, "sentence", 1L, nchar(str_txt))
  a2 <- NLP::annotate(str_txt, word_token_annotator, a2)
  a3 <- NLP::annotate(str_txt, pos_annotator, a2)
  a3w <- a3[a3$type == "word"]
  pos <- unlist(lapply(a3w$features, `[[`, "POS"))
  data.frame(
    word = str_txt[a3w],
    pos = pos,
    stringsAsFactors = FALSE
  )
}

handle_pos_error <- function(c_obj) {
  er <- c_obj$message
  if (grepl("memory", er, ignore.case = TRUE)) {
    stop(
      er, "\n\nSee the second FAQ in the 'Frequently asked questions' ",
      "vignette for how to fix this."
    )
  } else {
    stop(er)
  }
}

stop_pos_tags <- function(pos_word_df, stop_pos) {
  in_stop_pos <- pos_word_df$pos %in% stop_pos
  # Replace unwanted words (based on POS) with a phrase delim (.):
  pos_word_df$word[in_stop_pos] <- "."
  paste(pos_word_df$word, collapse = " ")
}

apend_split_txt <- function(stop_words, phrase_delims) {
  if (!is.null(stop_words)) {
    stop_words_wrds <- paste0("\\b", tolower(stop_words), "\\b")
    stop_regex <- paste0(stop_words_wrds, collapse = "|")
    paste0(stop_regex, "|", phrase_delims)
  } else {
    phrase_delims
  }
}

split_string <- function(txt, regex) {
  x <- strsplit(txt, regex)[[1]]
  strsplit(x, " ")
}

get_cand_words <- function(txt, stop_words, phrase_delims) {
  regex <- apend_split_txt(stop_words, phrase_delims = phrase_delims)
  split_string(txt, regex = regex)
}

filter_words <- function(cand_words, word_min_char) {
  temp_vec <- lapply(cand_words, function(x)
    x[x != "" & grepl("[[:alpha:]]", x) & nchar(x) >= word_min_char])
  temp_vec[vapply(temp_vec, length, numeric(1)) > 0]
}

gen_word_cnts <- function(cand_words) {
  # Get a list of unique words in each keyword so we don't double count (e.g.,
  # don't double count "vector" in "vector times vector").
  unq_wrds <- unlist(lapply(cand_words, unique))
  as.matrix(table(unq_wrds))
}

gen_degree <- function(wrd_cnts, cand_words) {
  temp_score1 <- vapply(rownames(wrd_cnts), function(x)
    sum(
      vapply(cand_words, function(q)
        ifelse(x %in% q, length(q) - 1, 0), numeric(1)
      )
    )
  , numeric(1))

  temp_score1 + wrd_cnts[, 1]
}

calc_word_scores <- function(wrd_cnts, degree) {
  structure(
    degree / wrd_cnts, # degree / freq
    names = rownames(wrd_cnts)
  )
}

calc_keyword_scores <- function(cand_words) {
  # Get word counts for all distinct words
  wrd_cnts <- gen_word_cnts(cand_words)
  # Get word's degree score
  degree <- gen_degree(wrd_cnts, cand_words = cand_words)

  # Get each word's score as per degree/frequency
  word_scores <- calc_word_scores(wrd_cnts = wrd_cnts, degree = degree)
  # Add word scores for the words in each (non-distinct) keyword, to get
  # keyword scores
  unlist(lapply(cand_words, function(x) sum(word_scores[x])))
}

process_keyword_df <- function(keyword_df) {
  key_cnts <- table(keyword_df$keyword)
  key_cntsdf <- as.data.frame(key_cnts, stringsAsFactors = FALSE)
  colnames(key_cntsdf) <- c("keyword", "freq")
  key_df <- merge(key_cntsdf, keyword_df, by = "keyword")
  out_df <- unique(key_df[order(key_df$score, decreasing = TRUE), ])
  row.names(out_df) <- NULL
  out_df
}