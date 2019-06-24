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
#' if (tts_amazon_auth()) {
#' tts_voices(service = "amazon")
#' }
tts_voices = function(
  service = c("amazon", "google", "microsoft")
) {
  service = match.arg(service)
  res = switch(service,
         amazon = tts_amazon_voices(),
         google = tts_google_voices(),
         microsoft = tts_microsoft_voices()
  )
  res
}

#' @rdname tts_voices
#' @export
tts_amazon_voices = function() {
  if (utils::packageVersion("aws.polly") <= package_version("0.1.4")) {
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
  } else {
    res = aws.polly::list_voices(language = NULL)
  }
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

#' @rdname tts_voices
#' @export
tts_microsoft_voices = function() {
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

#' @rdname tts_voices
#' @export
tts_google_voices = function() {
  res = googleLanguageR::gl_talk_languages()
  cn = colnames(res)
  cn[ cn == "ssmlGender" ] = "gender"
  cn[ cn == "languageCodes" ] = "language_code"
  cn[ cn == "name" ] = "voice"
  cn[ cn == "language" ] = "language"
  colnames(res) = cn
  if (!("language" %in% cn)) {
    df = tts_microsoft_voices()
    df = df[, c("language_code", "language")]
    res = merge(res, df, all.x = TRUE, by = "language_code")
  }
  res = res[, c("voice", "language", "language_code", "gender")]
  res$service = "google"
  res
}
