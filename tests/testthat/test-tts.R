patrick::with_parameters_test_that("test tts() on Amazon, Google, Microsoft, Coqui TTS, and Speechify) ",
                                   {
                                     testthat::skip_on_cran()
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
                                       tts_auth <- try(find_coqui())
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
                                                TRUE,
                                                tts_speechify_auth()
                                   ),
                                   company  = c("amazon", "google", "microsoft", "coqui", "speechify")
)

test_that("tts_speechify_auth() is FALSE without an API key", {
  withr::with_envvar(c(SPEECHIFY_API_KEY = ""), {
    expect_false(tts_speechify_auth())
  })
})

test_that("tts_speechify() defaults to simba-3.5 and keeps language", {
  expect_equal(formals(tts_speechify)$model, "simba-3.5")

  sent = list()
  mock = function(req) {
    sent <<- c(sent, list(req))
    httr2::response_json(
      body = list(audio_data = jsonlite::base64_enc(as.raw(0:3)))
    )
  }
  tts_audio_read_stub = function(...) NULL
  testthat::local_mocked_bindings(tts_audio_read = tts_audio_read_stub)
  withr::local_envvar(c(SPEECHIFY_API_KEY = "test-key"))
  httr2::with_mocked_responses(mock, {
    try(tts_speechify("Bonjour", language = "fr-FR", bind_audio = FALSE), silent = TRUE)
  })

  expect_length(sent, 1)
  body = sent[[1]]$body$data
  expect_equal(body$model, "simba-3.5")
  expect_equal(body$language, "fr-FR")
  expect_equal(sent[[1]]$headers[["Speechify-Caller"]], "text2speech")
})
