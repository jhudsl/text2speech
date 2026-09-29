test_that("text chunks respect the character limit and preserve the input", {
  text <- "A short sentence, followed by a much longer word."
  chunks <- tts_split_text(text, limit = 12)

  expect_true(all(nchar(chunks, type = "chars") <= 12))
  expect_identical(paste0(chunks, collapse = ""), text)
})

test_that("text without spaces is split into chunks within the limit", {
  chunks <- tts_split_text("abcdefghijklmnop", limit = 6)

  expect_identical(chunks, c("abcdef", "ghijkl", "mnop"))
})

test_that("short text is returned unchanged", {
  expect_identical(tts_split_text("short text", limit = 10), "short text")
})

test_that("limit must be a positive finite integer", {
  invalid_limits <- list(0, -1, Inf, NA_real_, 1.5, "10", c(1, 2))
  for (limit in invalid_limits) {
    expect_error(tts_split_text("text", limit = limit))
  }
})
