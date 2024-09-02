## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  progress = FALSE,
  error = FALSE, 
  message = FALSE,
  rownames.print = FALSE
)

options(digits = 2)

## -----------------------------------------------------------------------------
library(slowraker)

txt <- "Compatibility of systems of linear constraints over the set of natural numbers. Criteria of compatibility of a system of linear Diophantine equations, strict inequations, and nonstrict inequations are considered. Upper bounds for components of a minimal set of solutions and algorithms of construction of minimal generating sets of solutions for all types of systems are given. These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions can be used in solving all the considered types of systems and systems of mixed types."

## -----------------------------------------------------------------------------
slowrake(txt)[[1]]

## -----------------------------------------------------------------------------
slowrake(txt, stem = FALSE)[[1]]

## -----------------------------------------------------------------------------
slowrake(txt, stop_words = c(smart_words, "diophantine"))[[1]]

## -----------------------------------------------------------------------------
slowrake(txt, stop_pos = NULL)[[1]]

## -----------------------------------------------------------------------------
slowrake(txt, stop_pos = pos_tags$tag[!grepl("^N", pos_tags$tag)])[[1]]

## -----------------------------------------------------------------------------
res <- slowrake(txt)[[1]]
res2 <- aggregate(freq ~ keyword + stem, data = res, FUN = sum)
res2[order(res2$freq, decreasing = TRUE), ]

## -----------------------------------------------------------------------------
slowrake(txt = dog_pubs$abstract[1:10])

