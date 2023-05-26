patrick::with_parameters_test_that("test tts() on Amazon, Google, Microsoft, and Coqui TTS) ",
                                   {
                                     if (company != "coqui") {
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
                                     } else {
                                       # company == "coqui"
                                       if (!inherits(tts_auth, "try-error")) {
                                         response_df = tts("Algorithmic complexity is a key consideration
                                                when designing efficient solutions for large-scale data processing",
                                                           service = company)
                                         audio_value = response_df$wav[[1]]
                                         audio_path = response_df$file[[1]]

                                         testthat::expect_s3_class(response_df, "data.frame")
                                         testthat::expect_s4_class(audio_value, "Wave")
                                         testthat::expect_equal(file.exists(audio_path), TRUE)
                                       }
                                     }
                                   },
                                   # tts_auth(key_or_json_file = getOption("api_key")))
                                   tts_auth = c(tts_amazon_auth(getOption("aws_access_key")),
                                                tts_google_auth(getOption("google_api_key")),
                                                tts_microsoft_auth(key_or_json_file = getOption("ms_api_key"),
                                                                   region = "westus"),
                                                try(find_coqui())
                                   ),
                                   company  = c("amazon", "google", "microsoft", "coqui")
)

# Right now, we don't wanna run coqui test if installed. Next week, we can find time to store coqui TTS exectuable on Docker
# if company == coqui then if coqui_test_path(), run this test, if not, then don't run this test.
