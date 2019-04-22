testthat::context("List the Voices")

fixed_names = c("voice", "language", "language_code",
                "gender", "service")

testthat::test_that(
  "Microsoft Voices", {
    df = tts_voices(service = "microsoft")
    testthat::expect_is(df, "data.frame")
    testthat::expect_named(df, fixed_names)
  }
)

testthat::test_that(
  "Google Voices", {
    if (tts_google_auth()) {
      df = tts_voices(service = "google")
      testthat::expect_is(df, "data.frame")
      testthat::expect_named(df, fixed_names)
    }
  }
)
testthat::test_that(
  "Amazon Voices", {
    if (tts_amazon_auth()) {
      df = tts_voices(service = "amazon")
      testthat::expect_is(df, "data.frame")
      testthat::expect_named(df, fixed_names)
    }
  }
)
