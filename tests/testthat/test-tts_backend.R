fixed_names = c("index", "original_text", "text", "wav",
                "file", "audio_type",
                "duration",
                "service")

patrick::with_parameters_test_that("Google Cloud Text-to-Speech / Amazon Polly Translation",
                          {
                            if (tts_auth) {
                              response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                service = company)
                              expect_s3_class(response_df, char_value)
                            }
                          },
                          tts_auth = c(tts_amazon_auth(), tts_google_auth()),
                          company  = c("amazon", "google"),
                          char_value = "data.frame"
)

patrick::with_parameters_test_that("Google Cloud Text-to-Speech / Amazon Polly Translation",
                          {
                            if (tts_auth) {
                              response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                service = company)
                              expect_equal(response_df$service, char_value)
                            }
                          },
                          tts_auth = c(tts_amazon_auth(), tts_google_auth()),
                          company  = c("amazon", "google"),
                          char_value = c("amazon", "google")
)

patrick::with_parameters_test_that("Google Cloud Text-to-Speech / Amazon Polly Translation",
                          {
                            if (tts_auth) {
                              response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                service = company)
                              audio_value = response_df$wav[[1]]
                              expect_s4_class(audio_value, char_value)
                            }
                          },
                          tts_auth = c(tts_amazon_auth(), tts_google_auth()),
                          company  = c("amazon", "google"),
                          char_value = "Wave"
)
