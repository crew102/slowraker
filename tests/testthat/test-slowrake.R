context("slowrake")

data("dog_pubs")

test_that("slowrake works for non-atomic, non-empty char vectors", {
  out <- slowrake(dog_pubs$abstract[1:10])
  expect_equal(length(out), 10)
})

test_that("slowrake returns a score when there is only one word in txt", {
  out <- slowrake("dog")
  expect_true(is.numeric(out[[1]]$score))
})

test_that("slowrake works for txt without alpha chars", {
  out <- slowrake("")
  expect_true(is.na(unlist(out)))
})

test_that("slowrake works when all txt is removed based on POS tags", {
  out <- slowrake("walking")
  expect_true(is.na(unlist(out)))
})

test_that("slowrake removes stop words", {
  out <- slowrake("dogs", stop_words = "dogs")
  expect_true(is.na(unlist(out)))
})

test_that("pos stopping works as expected", {
  out1 <- slowrake("dogs are awesome", stop_pos = "NNS")
  out2 <- slowrake("dogs found food", stop_pos = NULL)
  expect_true(
    nrow(out2[[1]]) == 1 && nrow(out2[[1]]) == 1
  )
})

test_that("word_min_char filtering works as expected", {
  out <- slowrake("dogs", word_min_char = 5)
  expect_true(is.na(unlist(out)))
})

test_that("phrase_delims works as expected", {
  out <- slowrake(
    "dogs are great, arn't they? at least i think they are.",
    stop_words = NULL, stop_pos = NULL, phrase_delims = "\\?"
  )
  expect_true(grepl(",", out[[1]][1]))
})

test_that("hyphenated words are split apart", {
  out <- slowrake("i love computer-generated images")
  expect_true("computer generated images" %in% out[[1]]$keyword)
})

test_that("hyphen split doesn't result in NA scores", {
  out <- slowrake("all-dielectric metameterial")
  expect_true(!is.na(out[[1]]$score))
})