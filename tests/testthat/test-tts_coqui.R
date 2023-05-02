fixed_names = c("index", "original_text", "text", "wav",
                "file", "audio_type", "duration", "service")

testthat::test_that("Vanilla coqui TTS works", {
  response_df = tts("Algorithmic complexity is a key consideration when
               designing efficient solutions for large-scale data processing",
          service = "coqui")
  # Check x is a data.frame
  testthat::expect_s3_class(response_df, "data.frame")
  # Check column names
  testthat::expect_named(response_df, fixed_names)
  # Check Wave
  testthat::expect_s4_class(response_df$wav[[1]], "Wave")
}
)

testthat::test_that("coqui TTS works with wav as output_format", {
  response_df = tts("Algorithmic complexity is a key consideration when
               designing efficient solutions for large-scale data processing",
          service = "coqui",
          output_format = "wav")
  testthat::expect_s3_class(response_df, "data.frame")
  testthat::expect_named(response_df, fixed_names)
  testthat::expect_s4_class(response_df$wav[[1]], "Wave")

  # Check if audio_type is mp3
  testthat::expect_equal(response_df$audio_type, "wav")
}
)
