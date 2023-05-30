fixed_names = c("voice", "language", "language_code",
                "gender", "service")

patrick::with_parameters_test_that("test tts_voices() on Amazon, Google, and Microsoft engines",
                                   {
                                     if (tts_auth) {
                                       response_df = tts_voices(service = company)
                                       testthat::expect_s3_class(response_df, "data.frame")
                                       testthat::expect_named(response_df, fixed_names)
                                     }
                                   },
                                   tts_auth = c(tts_amazon_auth(), tts_google_auth(), tts_microsoft_auth(region = "westus")),
                                   company  = c("amazon", "google", "microsoft")
)

fixed_names_coqui <- c("language", "dataset", "model_name", "service")

test_that("test tts_voices() on Coqui engine",
          {
            tts_auth <- try(find_coqui())
            if (!inherits(tts_auth, "try-error")) {
              response_df = tts_voices(service = "coqui")
              # Check x is a data.frame
              expect_s3_class(response_df, "data.frame")
              # Check column names
              expect_named(response_df, fixed_names_coqui)
            }
          }
)
