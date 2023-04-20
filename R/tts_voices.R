# Return a data frame of language codes and their corresponding names for text-to-speech services
tts_language_codes = function() {
  df = data.frame(
    language_code = c("ar-XA", "ar-EG", "ar-SA", "bg-BG", "ca-ES", "cs-CZ",
                      "da-DK", "de-AT", "de-CH", "de-DE", "el-GR", "en-AU", "en-CA",
                      "en-GB", "en-IE", "en-IN", "en-US", "es-ES", "es-MX", "fi-FI",
                      "fr-CA", "fr-CH", "fr-FR", "he-IL", "hi-IN", "hr-HR", "hu-HU",
                      "id-ID", "it-IT", "ja-JP", "ko-KR", "ms-MY", "nb-NO", "nl-NL",
                      "pl-PL", "pt-BR", "pt-PT", "ro-RO", "ru-RU", "sk-SK", "sl-SI",
                      "sv-SE", "ta-IN", "te-IN", "th-TH", "tr-TR", "vi-VN", "zh-CN",
                      "zh-HK", "zh-TW", "fil-PH", "uk-UA"),
    language = c("Arabic", "Arabic (Egypt)", "Arabic (Saudi Arabia)",
                 "Bulgarian", "Catalan (Spain)", "Czech", "Danish", "German (Austria)",
                 "German (Switzerland)", "German (Germany)", "Greek", "English (Australia)",
                 "English (Canada)", "English (UK)", "English (Ireland)", "English (India)",
                 "English (US)", "Spanish (Spain)", "Spanish (Mexico)", "Finnish",
                 "French (Canada)", "French (Switzerland)", "French (France)",
                 "Hebrew (Israel)", "Hindi (India)", "Croatian", "Hungarian",
                 "Indonesian", "Italian", "Japanese", "Korean", "Malay", "Norwegian",
                 "Dutch", "Polish", "Portuguese (Brazil)", "Portuguese (Portugal)",
                 "Romanian", "Russian", "Slovak", "Slovenian", "Swedish", "Tamil (India)",
                 "Telugu (India)", "Thai", "Turkish", "Vietnamese", "Chinese (Mainland)",
                 "Chinese (Hong Kong)", "Chinese (Taiwan)",
                 "Filipino (Philippines)", "Ukrainian (Ukraine)"),
    stringsAsFactors = FALSE)
  df
}

#' Text to Speech Voices
#'
#' @param ... Additional arguments to service voice listings.
#' @param service service to use
#'
#' @return A `data.frame` of language codes, voices, genders,
#' and language names
#' @export
#'
#' @examples
#' if (tts_microsoft_auth()) {
#' tts_voices(service = "microsoft")
#' }
#' if (tts_google_auth()) {
#' tts_voices(service = "google")
#' }
#' if (requireNamespace("aws.polly", quietly = TRUE)) {
#' if (tts_amazon_auth()) {
#' tts_voices(service = "amazon")
#' }
#' }
tts_voices = function(
    service = c("amazon", "google", "microsoft", "coqui"),
    ...
) {
  service = match.arg(service)
  res = switch(service,
               amazon = tts_amazon_voices(...),
               google = tts_google_voices(...),
               microsoft = tts_microsoft_voices(...),
               coqui = tts_coqui_voices(...),
  )
  res
}


#' Get Amazon Polly TTS voices
#' @rdname tts_voices
#' @export
tts_amazon_voices = function(...) {
  tts_amazon_auth(...)
  res =  try({
    suppressWarnings({
      aws.polly::list_voices(language = NULL)
    })
  }, silent = TRUE)
  if (utils::packageVersion("aws.polly") <= package_version("0.1.4") |
      inherits(res, "try-error") | length(res) == 0) {
    # as per https://docs.aws.amazon.com/polly/latest/dg/SupportedLanguage.html

    amazon_language_codes = c("arb", "cmn-CN", "da-DK", "nl-NL",
                              "en-AU", "en-GB",
                              "en-IN", "en-US", "en-GB-WLS",
                              "fr-FR", "fr-CA", "hi-IN",
                              "de-DE", "is-IS", "it-IT",
                              "ja-JP", "ko-KR", "nb-NO", "pl-PL",
                              "pt-BR", "pt-PT", "ro-RO", "ru-RU",
                              "es-ES", "es-MX", "es-US",
                              "sv-SE", "tr-TR", "cy-GB")
    res = lapply(amazon_language_codes,
                 function(x) {
                   aws.polly::list_voices(language = x)
                 })
    res = do.call(rbind, res)
  }
  # else {
  #   res = aws.polly::list_voices(language = NULL)
  # }
  cn = colnames(res)
  cn[ cn == "Gender" ] = "gender"
  cn[ cn == "LanguageCode" ] = "language_code"
  cn[ cn == "Name" ] = "voice"
  cn[ cn == "LanguageName" ] = "language"
  colnames(res) = cn
  res = res[, c("voice", "language", "language_code", "gender")]
  res$service = "amazon"

  res
}

#' Get Microsoft Cognitive Services Text to Speech voices
#' @rdname tts_voices
#' @export
tts_microsoft_voices = function(...) {
  # tts_microsoft_auth(...)
  res = mscstts::ms_locale_df()
  cn = colnames(res)
  cn[ cn == "Gender" ] = "gender"
  cn[ cn == "code" ] = "language_code"
  cn[ cn == "locale" ] = "voice"
  cn[ cn == "language" ] = "language"
  colnames(res) = cn
  res = res[, c("voice", "language", "language_code", "gender")]
  res$service = "microsoft"

  res
}


#' Get Google Cloud TTS voices
#' @rdname tts_voices
#' @export
tts_google_voices = function(...) {
  tts_google_auth(...)
  res = googleLanguageR::gl_talk_languages()
  cn = colnames(res)
  cn[ cn == "ssmlGender" ] = "gender"
  cn[ cn == "languageCodes" ] = "language_code"
  cn[ cn == "name" ] = "voice"
  cn[ cn == "language" ] = "language"
  colnames(res) = cn
  if (!("language" %in% cn)) {
    # df = tts_microsoft_voices()
    df = tts_language_codes()
    df = df[, c("language_code", "language")]
    res = merge(res, df, all.x = TRUE, by = "language_code")
  }
  res = res[, c("voice", "language", "language_code", "gender")]
  res$service = "google"
  res
}


#' Get Coqui TTS voices (list models)
#' @rdname tts_voices
#' @export
tts_coqui_voices = function(coqui_path) {
  # Run `which tts` system command with temporary system search path
  # ignore.stdout: don't print to console
  check_coqui <- withr::with_path(coqui_path,
                                  system("which tts", intern = FALSE, ignore.stdout = TRUE))
  # Check if coqui TTS is installed
  if (check_coqui > 0) {
    message("coqui TTS library not found. Please install from https://github.com/coqui-ai/TTS#install-tts")
  }

  # Run command
  out <- withr::with_path(coqui_path, system("tts --list_models", intern = TRUE))
  out <- trimws(out)
  message("Test out different voices (models) at https://huggingface.co/spaces/coqui/CoquiTTS")
  # Only show tts_models
  out[grepl("tts_models", out)]
}
