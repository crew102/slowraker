# get_pos_tags adapted from:
# http://martinschweinberger.de/docs/articles/PosTagR.pdf
get_pos_tags <- function(txt) {
  str_txt <- NLP::as.String(txt)
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  a2 <- NLP::Annotation(1L, "sentence", 1L, nchar(str_txt))
  a2 <- NLP::annotate(str_txt, word_token_annotator, a2)
  a3 <- NLP::annotate(str_txt, openNLP::Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  pos <- unlist(lapply(a3w$features, `[[`, "POS"))
  data.frame(
    word = str_txt[a3w],
    pos = pos,
    stringsAsFactors = FALSE
  )
}

stop_pos_tags <- function(pos_word_df, stop_pos) {
  in_stop_pos <- pos_word_df$pos %in% stop_pos
  # Replace unwanted words (based on POS) with a phrase delim (.):
  pos_word_df$word[in_stop_pos] <- "."
  paste(pos_word_df$word, collapse = " ")
}

gen_split_regex <- function(stop_words) {
  phrase_end_regex <- '[,.?():;"-]'
  if (!is.null(stop_words)) {
    stop_words_wrds <- paste0("\\b", stop_words, "\\b")
    stop_regex <- paste0(stop_words_wrds, collapse = "|")
    paste0(stop_regex, "|", phrase_end_regex)
  } else {
    phrase_end_regex
  }
}

split_string <- function(txt, regex) {
  x <- strsplit(txt, regex)[[1]]
  strsplit(x, " ")
}

get_cand_words <- function(txt, stop_words) {
  regex <- gen_split_regex(stop_words = stop_words)
  split_string(txt = txt, regex = regex)
}

filter_words <- function(cand_words, word_min_char) {
  temp_vec <- lapply(cand_words, function(x) {
    x[x != "" & grepl("[[:alpha:]]", x) & nchar(x) >= word_min_char]
  })
  temp_vec[vapply(temp_vec, length, numeric(1)) > 0]
}

gen_word_cnts <- function(cand_words) {
  # Get a list of unique words in each keyword so we don't double count (e.g.,
  # don't double count "vector" keyword "vector times vector").
  unq_wrds <- unlist(lapply(cand_words, unique))
  as.matrix(table(unq_wrds))
}

gen_non_diag_deg <- function(wrd_cnts, cand_words) {
  # The non-diagonal component of a word's degree is the number of times that
  # the word co-occurs with another distinct word...To get this value, we count
  # the number of keywords the word is in and weight this by the keyword's
  # length, then substract 1.
  # what about case where word occurs twice "sum sum"
  temp_score1 <- lapply(rownames(wrd_cnts), function(x)
    unlist(lapply(cand_words, function(q) x %in% q)))
  kr <- vapply(cand_words, length, numeric(1)) - 1
  vapply(temp_score1, function(x) sum(kr[x]), numeric(1))
}

calc_word_scores <- function(wrd_cnts, non_diag_deg) {
  # Degree can now be found by adding non-diagonal component of degree to
  # the diagonal component (diagonal component = a word's frequency)
  mat <- cbind(non_diag_deg, wrd_cnts)
  mat[, 1] <- mat[, 1] + mat[, 2] # Add non-diag to diag
  structure(
    mat[, 1] / mat[, 2], # degree / freq
    names = rownames(wrd_cnts)
  )
}

calc_keyword_scores <- function(cand_words) {
  # Get word counts for all distinct words
  wrd_cnts <- gen_word_cnts(cand_words = cand_words)
  # Get the non-diagonal component of a word's degree score
  non_diag_deg <- gen_non_diag_deg(wrd_cnts = wrd_cnts, cand_words = cand_words)

  # Get each word's score as per degree/frequency
  word_scores <- calc_word_scores(wrd_cnts = wrd_cnts,
                                  non_diag_deg = non_diag_deg)

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