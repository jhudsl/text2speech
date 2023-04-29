testthat::test_that("coqui TTS works", {
      df = tts("Hi Candace, this is Howard.", service = "coqui")
      testthat::expect_is(df, "data.frame")
      testthat::expect_named(df, fixed_names)
      testthat::expect_is(df$wav[[1]], "Wave")
      wav = df$wav[[1]]
      testthat::expect_true(length(wav)/wav@samp.rate >= 0.5)
  }
)
