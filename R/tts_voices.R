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
  res = aws.polly::list_voices(language = NULL)
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
