#'Text-to-Speech (Speech Synthesis) Voices
#'
#'@description Various services offer a range of voice options:
#' * Amazon Polly : <https://docs.aws.amazon.com/polly/latest/dg/voicelist.html>
#' * Microsoft Cognitive Services Text to Speech REST API : <https://learn.microsoft.com/en-us/azure/cognitive-services/speech-service/language-support?tabs=tts#voice-styles-and-roles>
#' * Google Cloud Text-to-Speech API : <https://cloud.google.com/text-to-speech/docs/voices>
#' * Coqui TTS : <https://huggingface.co/spaces/coqui/CoquiTTS>
#'
#'@param service Service to use (Amazon, Google, Microsoft, or Coqui)
#'@param ... Additional arguments to service voice listings.
#
#'@return (Amazon, Microsoft, and Google) A standardized `data.frame` featuring
#'  the following columns:
#' * `voice` : Name of the voice
#' * `language` : Spoken language
#' * `language_code` : Abbreviation for the language of the speaker
#' * `gender` : Male or female
#' * `service` : The text-to-speech engine used
#'
#'  (Coqui TTS) A `tibble` featuring the following columns:
#' * `language` : Spoken language
#' * `dataset` : Dataset the deep learning model was trained on
#' * `model_name` : Name of deep learning model
#' * `service` : The text-to-speech engine used
#'
#' @export
#' @examples \dontrun{
#' #' # Amazon Polly
#' if (requireNamespace("aws.polly", quietly = TRUE)) {
#' if (tts_amazon_auth()) {
#' tts_voices(service = "amazon")
#' }
#' }
#'
#' # Microsoft Cognitive Services Text to Speech REST API
#' if (tts_microsoft_auth()) {
#' tts_voices(service = "microsoft")
#' }
#'
#' # Google Cloud Text-to-Speech API
#' if (tts_google_auth()) {
#' tts_voices(service = "google")
#' }
#'
#' # Coqui TTS
#' tts_auth("coqui")
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


#' Default voice for text-to-speech engine
#'
#' @param service Text-to-speech engine
#'
#' @export
tts_default_voice = function(
    service = c("amazon", "google", "microsoft", "coqui")
) {
  voice = switch(
    service,
    google = "en-US-Standard-C",
    microsoft = "Microsoft Server Speech Text to Speech Voice (en-US, ZiraRUS)",
    amazon = "Joanna",
    coqui = "tacotron2-DDC")

  voice
}

#' @export
#' @rdname tts_voices
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


#' @export
#' @rdname tts_voices
tts_microsoft_voices = function(region = "westus") {
  res = mscstts2::ms_list_voice()
  cn = colnames(res)
  cn[ cn == "Name" ] <- "voice"
  cn[ cn == "Locale" ] <- "language_code"
  cn[ cn == "LocaleName" ] <- "language"
  cn[ cn == "Gender" ] <- "gender"
  colnames(res) <- cn
  res = res[, c("voice", "language", "language_code", "gender")]
  res$service = "microsoft"

  res
}



#' @export
#' @rdname tts_voices
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
    df <- data.frame(
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
    df = df[, c("language_code", "language")]
    res = merge(res, df, all.x = TRUE, by = "language_code")
  }
  res = res[, c("voice", "language", "language_code", "gender")]
  res$service = "google"
  res
}



#' @export
#' @rdname tts_voices
tts_coqui_voices = function() {
  # Look for coqui_path
  use_coqui()
  coqui_path <- getOption("path_to_coqui")

  # Run command to list models
  out <- withr::with_path(process_coqui_path(coqui_path),
                          system("tts --list_models", intern = TRUE))
  out <- trimws(out)
  # Format the output into a dataframe with 3 columns: language, dataset, voice
  out <- out[grepl("tts_models", out)]
  # Extract out everything after [number]:
  out <- sub("^.*tts_models/", "", out)
  out <- data.frame(out)

  # Separate by "//" using tidyr::separate()
  out <- out %>%
    tidyr::separate_wider_delim(out,
                                delim = "/",
                                names = c("language", "dataset", "model_name")) %>%
    dplyr::mutate(service = "coqui")

  cli::cli_alert_info("Test out different voices on the {.href [CoquiTTS Demo](https://huggingface.co/spaces/coqui/CoquiTTS)}")
  out
}
