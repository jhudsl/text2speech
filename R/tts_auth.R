#' Authentication for Text-to-Speech (Speech Synthesis) Engines
#'
#' @description Verify the authentication status of different text-to-speech
#' engines, including Amazon Polly, Coqui TTS, Google Cloud Text-to-Speech API,
#' and Microsoft Cognitive Services Text to Speech REST API.
#'
#' @details To determine the availability of Coqui TTS, `tts_auth()` examines whether the tts
#' executable exists on local system.
#'
#' @param service Service to use (Amazon, Google, Microsoft, or Coqui)
#' @param key_or_json_file Either an API key (for Microsoft) or JSON file (for
#'   Google)
#' @param ... Additional arguments
#'
#' @return A logical indicator of authorization
#'
#' @export
#'
#' @examples \dontrun{
#' # Amazon Polly
#' tts_auth("amazon")
#'
#' # Google Cloud Text-to-Speech API
#' tts_auth("google")
#'
#' # Microsoft Cognitive Services Text to Speech REST API
#' tts_auth("microsoft")
#'
#' # Coqui TTS
#' tts_auth("coqui")
#' }
tts_auth = function(service = c("amazon",
                                "google",
                                "microsoft",
                                "coqui"),
                    key_or_json_file = NULL,
                    ...) {
  service = match.arg(service)
  if (service == "google") {
    res = tts_google_auth(key_or_json_file, ...)
  }
  if (service == "amazon") {
    res = tts_amazon_auth(key_or_json_file, ...)
  }
  if (service == "microsoft") {
    res = tts_microsoft_auth(key_or_json_file, ...)
  }
  if (service == "coqui") {
    res = tts_coqui_auth()
  }
  return(res)
}

#' Authenticate the user's credentials for using the Amazon Polly Text-to-Speech (TTS) service
#'
#' @rdname tts_auth
#' @export
tts_amazon_auth = function(key_or_json_file = NULL, ...) {
  if (tts_amazon_check()) {
    return(TRUE)
  }
  credential_file = aws.signature::default_credentials_file()
  if (file.exists(credential_file)) {
    aws.signature::use_credentials(...)
  }
  return(tts_amazon_check())
}

# Check Amazon Polly Text-to-Speech API Authentication Status
tts_amazon_check = function() {
  if (!requireNamespace("aws.polly", quietly = TRUE)) {
    stop(paste0(
      "This function requires aws.polly to operate",
      " please use\n",
      "install.packages('aws.polly')\n",
      "to use these functions"))
  }

  # Try aws.polly::list_voices()
  L = try({
    aws.polly::list_voices()
  })
  if (inherits(L, "try-error")) {
    return(FALSE)
  }
  if (length(L) == 0 || NROW(L) == 0) {
    return(FALSE)
  }
  return(TRUE)
}


#' @export
#' @rdname tts_auth
tts_google_auth = function(key_or_json_file = NULL, ...) {
  if (!is.null(key_or_json_file)) {
    suppressMessages({
      auth = googleLanguageR::gl_auth(json_file = key_or_json_file)
    })
  }
  needed <- c("https://www.googleapis.com/auth/cloud-language",
              "https://www.googleapis.com/auth/cloud-platform")
  if (!tts_google_check()) {
    suppressMessages({
      auth = googleAuthR::gar_attach_auto_auth(
        needed,
        environment_var = "GL_AUTH")
    })
  }
  if (!tts_google_check()) {
    suppressMessages({
      auth = googleAuthR::gar_attach_auto_auth(
        needed,
        environment_var = "GL_AUTH_FILE")
    })
  }
  return(tts_google_check())
}

#  Check Google Cloud Text-to-Speech API Authentication Status
tts_google_check = function() {
  res = try({
    suppressMessages({
      googleLanguageR::gl_talk_languages()
    })
  }, silent = TRUE)
  !inherits(res, "try-error")
}


#' @export
#' @rdname tts_auth
tts_microsoft_auth = function(key_or_json_file = NULL, ...) {
  if (!conrad::ms_exist_key()) {
    conrad::ms_set_key(api_key = key_or_json_file)
    res = conrad::ms_exist_key()
  }
  return(tts_microsoft_check(...))
}


# Check Microsoft Cognitive Services Text to Speech REST API Authentication Status
tts_microsoft_check = function(...) {
  res = try({
    conrad::ms_get_token(...)
  })
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  res_status_code = res$response$status_code
  res_status_code < 400
}

#' @export
#' @rdname tts_auth
tts_coqui_auth <- function() {
  use_coqui()
  coqui_path <- getOption("path_to_coqui")

  res <- suppressWarnings(withr::with_path(process_coqui_path(coqui_path),
                                           system("which tts", intern = TRUE)))

  if (!is.null(attr(res, "status"))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
