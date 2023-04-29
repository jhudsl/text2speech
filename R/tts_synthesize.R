#' Convert Text to Speech
#'
#' @param text A character vector of text to speak
#' @param output_format Format of output files
#' @param ... Additional arguments to
#' `text2speech::tts_google()`,
#' `text2speech::tts_amazon()`, or
#' `text2speech::tts_microsoft()`
#' @param service service to use
#'
#' @note All functions have a  `voice`` argument fro a
#' full voice name that can be passed to the
#' service, such as `voice` for `get_synthesis`` from \code{aws.polly}
#'
#' @param bind_audio Should the [text2speech::tts_bind_wav()]
#' be run on after the audio has been created, to ensure that
#' the length of text and the number of rows is consistent?
#' This affects the output format of some audio.
#'
#'
#' @return A `data.frame` of text and wav files
#' @export
#' @examples
#'
#' if (requireNamespace("stringi", quietly = TRUE)) {
#'     set.seed(1)
#'     text = stringi::stri_rand_lipsum(10)
#'     text[3] = paste0(text[3:length(text)], collapse = " " )
#'     text = text[c(1,3)]
#'     nchar(text)
#'     if (tts_auth("google")) {
#'         res = tts(text, service = "google", bind_audio = FALSE)
#'         testthat::expect_equal(nrow(res), length(text) + 1)
#'         bound = tts_bind_wav(res)
#'         testthat::expect_equal(nrow(bound), length(text))
#'     }
#' }
tts = function(
  text,
  output_format = c("mp3", "wav"),
  ...,
  service = c("amazon", "google", "microsoft", "coqui"),
  bind_audio = TRUE) {

  service = match.arg(service)
  if (!tts_auth(service = service)) {
    warning(paste0("Service ", service, " not authorized/unavailable"))
  }
  # TODO: Add coqui as an option
  output_format = match.arg(output_format)
  if (service == "google") {
    res = tts_google(
      text = text,
      output_format = output_format,
      bind_audio = bind_audio,
      ...)
  }
  if (service == "amazon") {
    res = tts_amazon(
      text = text,
      output_format = output_format,
      bind_audio = bind_audio,
      ...)
  }
  if (service == "microsoft") {
    res = tts_microsoft(
      text = text,
      output_format = output_format,
      bind_audio = bind_audio,
      ...)
  }
  if (service == "coqui") {
    res <- tts_coqui(

    )
  }
  res$service = service
  return(res)
}

#' Bind Wavs together
#'
#' @param result A \code{data.frame} from
#' [text2speech::tts()].
#' @param same_sample_rate force the same sample rate
#'
#' @return A `data.frame` with the same structure as
#' that of \code{tts}
#'
#' @note As the data are split due to limits of the
#' API, then this allows the text and the
#' results to be harmonized
#'
#' @export
tts_bind_wav = function(result, same_sample_rate = TRUE) {
  index = NULL
  rm(list = "index")
  result = result %>%
    dplyr::arrange(index)
  ss = split(result, result$index)
  ss = lapply(ss, function(x) {
    if (nrow(x) == 1) {
      return(x)
    }
    wav = x$wav
    names(wav) = NULL
    wav = do.call(tuneR::bind, wav)
    txt = paste(x$text, collapse = " ")
    output = tempfile(fileext = ".wav")
    tuneR::writeWave(wav, output)
    dplyr::tibble(original_text = txt,
                  text = txt,
                  index = unique(x$index),
                  wav = list(wav),
                  file = output,
                  audio_type = "wav")
  })
  ss = dplyr::bind_rows(ss)
  if (same_sample_rate && nrow(ss) > 0) {
    sample_rate = sapply(ss$wav, function(r) r@samp.rate)
    if (!all(sample_rate == sample_rate[[1]])) {
      message("enforcing same sample rate, using minimum")
    }
    sample_rate = min(sample_rate, na.rm = TRUE)
    ss$wav = lapply(ss$wav, function(x) {
      if (x@samp.rate == sample_rate) return(x)
      tuneR::downsample(x, samp.rate = sample_rate)
    })
  }
  return(ss)
}
