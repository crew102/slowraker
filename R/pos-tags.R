#' Part-of-speech (POS) tags
#'
#' A data frame containing all possible parts-of-speech, as per the \code{\link{openNLP}} package. This list was taken from \href{Part-Of-Speech Tagging with R}{http://martinschweinberger.de/docs/articles/PosTagR.pdf}. See details section below.
#'
#' @details
#'
#' \code{pos_tags} contains the following two columns:
#' \describe{
#'   \item{tag}{The abbreviation for the part-of-speech (i.e., its tag)}
#'   \item{dscrp}{A short description of the part-of-speech}
#' }
#'
#' @export
pos_tags <-
  structure(list(
    tag = c("CC", "CD", "DT", "EX", "FW", "IN", "JJ", "JJR", "JJS", "LS",
            "MD", "NN", "NNS", "NNP", "NNPS", "PDT", "POS", "PRP", "PRP$",
            "RB", "RBR", "RBS", "RP", "SYM", "TO", "UH", "VB", "VBD", "VBG",
            "VBN", "VBP", "VBZ", "WDT", "WP", "WP$", "WRB"),
    dscrp = c("Coordinating conjunction", "Cardinal number", "Determiner",
              "Existential there", "Foreign word",
              "Preposition or subordinating conjunction", "Adjective",
              "Adjective, comparative", "Adjective, superlative",
              "List item marker", "Modal", "Noun, singular or mass",
              "Noun, plural", "Proper noun, singular", "Proper noun, plural",
              "Predeterminer", "Possessive ending", "Personal pronoun",
              "Possessive pronoun", "Adverb", "Adverb, comparative",
              "Adverb, superlative", "Particle", "Symbol", "to", "Interjection",
              "Verb, base form", "Verb, past tense",
              "Verb, gerund or present participle", "Verb, past participle",
              "Verb, non-3rd person singular present",
              "Verb, 3rd person singular present", "Wh-determiner",
              "Wh-pronoun", "Possessive wh-pronoun", "Wh-adverb")
    ),
  .Names = c("tag", "dscrp"), row.names = c(NA, -36L), class = "data.frame"
  )