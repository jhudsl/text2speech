fixed_names = c("index", "original_text", "text", "wav",
                "file", "audio_type",
                "duration",
                "service")

test_that(
  "Google Cloud Text-to-Speech", {
    if (tts_google_auth()) {
      response_df = tts("Algorithmic complexity is a key consideration when
               designing efficient solutions for large-scale data processing",
                        service = "google")
      expect_s3_class(response_df, "data.frame")
      expect_named(response_df, fixed_names)
      expect_s4_class(response_df$wav[[1]], "Wave")
    }
  }
)
