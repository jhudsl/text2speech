fixed_names = c("index", "original_text", "text", "wav",
                "file", "audio_type",
                "duration",
                "service")

test_that(
  "Amazon Polly Translation", {
    if (tts_amazon_auth()) {
      response_df = tts("Algorithmic complexity is a key consideration when
               designing efficient solutions for large-scale data processing",
                        service = "amazon")
      expect_s3_class(response_df, "data.frame")
      expect_named(response_df, fixed_names)
      expect_s4_class(response_df$wav[[1]], "Wave")
    }
  }
)
