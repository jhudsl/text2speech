patrick::with_parameters_test_that("test tts() on Amazon, Google, and Microsoft engines) ",
                                   {
                                     if (tts_auth) {
                                       response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                         service = company)
                                       audio_value = response_df$wav[[1]]
                                       audio_path = response_df$file[[1]]

                                       testthat::expect_s3_class(response_df, "data.frame")
                                       testthat::expect_s4_class(audio_value, "Wave")
                                       testthat::expect_equal(file.exists(audio_path), TRUE)
                                     }
                                   },
                                   tts_auth = c(tts_amazon_auth(), tts_google_auth(), tts_microsoft_auth(region = "westus")),
                                   company  = c("amazon", "google", "microsoft")
)

fixed_names = c("index", "original_text", "text", "wav",
                "file", "audio_type", "duration", "service")

test_that("test tts() on Coqui engine", {
  response_df = tts("Algorithmic complexity is a key consideration when
               designing efficient solutions for large-scale data processing",
                    service = "coqui")
  # Check x is a data.frame
  expect_s3_class(response_df, "data.frame")
  # Check column names
  expect_named(response_df, fixed_names)
  # Check Wave
  expect_s4_class(response_df$wav[[1]], "Wave")
}
)
