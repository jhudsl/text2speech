testthat::context("Translate")

fixed_names = c("index", "original_text", "text", "wav",
                "file", "audio_type",
                "duration",
                "service")

testthat::test_that(
  "Google Translation", {
    if (tts_google_auth()) {
      df = tts("hey what's up?", service = "google")
      testthat::expect_is(df, "data.frame")
      testthat::expect_named(df, fixed_names)
      testthat::expect_is(df$wav[[1]], "Wave")
      wav = df$wav[[1]]
      testthat::expect_true(length(wav)/wav@samp.rate >= 0.5)
    }
  }
)
