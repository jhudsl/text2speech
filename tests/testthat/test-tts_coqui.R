fixed_names = c("index", "original_text", "text", "wav",
                "file", "audio_type", "duration", "service")

testthat::test_that("Vanilla coqui TTS works", {
      x = tts("Algorithmic complexity is a key consideration when
               designing efficient solutions for large-scale data processing",
               service = "coqui")
      testthat::expect_s3_class(x, "data.frame")
      testthat::expect_named(x, fixed_names)
      testthat::expect_s4_class(x$wav[[1]], "Wave")
      wav = x$wav[[1]]
      testthat::expect_true(length(wav)/wav@samp.rate >= 0.5)
  }
)

testthat::test_that("coqui TTS works", {
  x = tts("Algorithmic complexity is a key consideration when
               designing efficient solutions for large-scale data processing",
          service = "coqui")
  testthat::expect_s3_class(x, "data.frame")
  testthat::expect_named(x, fixed_names)
  testthat::expect_s4_class(x$wav[[1]], "Wave")
  wav = x$wav[[1]]
  testthat::expect_true(length(wav)/wav@samp.rate >= 0.5)
}
)
