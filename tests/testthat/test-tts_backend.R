patrick::with_parameters_test_that("tts() returns a data.frame",
                                   {
                                     if (tts_auth) {
                                       response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                         service = company)
                                       testthat::expect_s3_class(response_df, char_value)
                                     }
                                   },
                                   tts_auth = c(tts_amazon_auth(), tts_google_auth(), tts_microsoft_auth(region = "westus")),
                                   company  = c("amazon", "google", "microsoft"),
                                   char_value = "data.frame"
)

patrick::with_parameters_test_that("tts() returns a data.frame with an Wave object",
                                   {
                                     if (tts_auth) {
                                       response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                         service = company)
                                       audio_value = response_df$wav[[1]]
                                       testthat::expect_s4_class(audio_value, char_value)
                                     }
                                   },
                                   tts_auth = c(tts_amazon_auth(), tts_google_auth(), tts_microsoft_auth(region = "westus")),
                                   company  = c("amazon", "google", "microsoft"),
                                   char_value = "Wave"
)

patrick::with_parameters_test_that("tts() successfully created an audio output in a file path",
                                   {
                                     if (tts_auth) {
                                       response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                         service = company)
                                       audio_path = response_df$file[[1]]
                                       testthat::expect_equal(file.exists(audio_path), TRUE)
                                     }
                                   },
                                   tts_auth = c(tts_amazon_auth(), tts_google_auth(), tts_microsoft_auth(region = "westus")),
                                   company  = c("amazon", "google", "microsoft")
)
