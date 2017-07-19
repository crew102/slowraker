gen_split_regex <- function(stop_words) {
  stop_words_wrds <- paste0("\\b", stop_words, "\\b")
  stop_regex <- paste0(stop_words_wrds, collapse = "|")
  paste0(stop_regex, "|[,.?():;-]|\"")
}

split_string <- function(txt, regex) {
  x <- strsplit(txt, regex)[[1]]
  strsplit(x, " ")
}

clean_words <- function(split, word_min_char) {
  temp_vec <- lapply(split, function(x) {
    x[x != "" & grepl("[[:alpha:]]", x) & nchar(x) >= word_min_char]
  })
  temp_vec[sapply(temp_vec, length) > 0]
}

gen_candidates <- function(txt, stop_words, word_min_char) {
  txt <- tolower(txt)
  regex <- gen_split_regex(stop_words = stop_words)
  split <- split_string(txt = txt, regex = regex)
  clean_words(split = split, word_min_char = word_min_char)
}

gen_word_cnts <- function(cand_vec) {
  unq_wrds <- unlist(sapply(cand_vec, unique))

  # get freq for each word, along with list of distinct words
  as.matrix(table(unq_wrds))
}

gen_non_diag_deg <- function(wrd_cnts, cand_vec) {
  # the non-diag component of the degree  of each word is the number of times
  # that it co-occurs with another distinct word...
  # what about case where word occurs twice "sum sum"
  # To do get this value, we get number of keywords word is in, then weight that by
  # keywords length (-1)..so if keyword is only one word, no impact on degree
  temp_score1 <- lapply(rownames(wrd_cnts),
                        function(x) sapply(cand_vec, function(q) x %in% q))
  kr <- unlist(sapply(cand_vec, length)) - 1
  sapply(temp_score1, function(x) sum(kr[x]))
}

calc_scores <- function(wrd_cnts, non_diag_deg) {
  # degree can now be found by adding non-diag component to diag componet (freq)
  mat <- cbind(non_diag_deg, wrd_cnts)
  mat[, 1] <- mat[, 1] + mat[, 2] # add non-diag to diag
  mat[, 1] / mat[, 2] # degree / freq
}

# Adapted from http://martinschweinberger.de/docs/articles/PosTagR.pdf
filter_pos_tags <- function(txt, keep_pos) {
  str_txt <- NLP::as.String(txt)
  word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  a2 <- NLP::Annotation(1L, "sentence", 1L, nchar(str_txt))
  a2 <- NLP::annotate(str_txt, word_token_annotator, a2)
  a3 <- NLP::annotate(str_txt, openNLP::Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  pos_tagged <- unlist(lapply(a3w$features, `[[`, "POS"))
  to_keep <- pos_tagged %in% keep_pos
  myvec <- str_txt[a3w]
  myvec[!to_keep] <- "."
  paste(myvec, collapse = " ")
}

process_keyword_df <- function(keyword_df) {
  key_cnts <- table(keyword_df$keyword)
  key_cntsdf <- as.data.frame(key_cnts, stringsAsFactors = F)
  colnames(key_cntsdf) <- c("keyword", "freq")
  key_df <- merge(key_cntsdf, keyword_df, by = "keyword")
  unique(key_df[order(key_df$score, decreasing = TRUE), ])
}