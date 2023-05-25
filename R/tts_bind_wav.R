#' Bind Wavs together
#'
#' @description As the data are split due to limits of the API, `tts_bind_wav()`
#'   allows the text and the results to be harmonized
#'
#' @param result A \code{data.frame} from [text2speech::tts()].
#' @param same_sample_rate force the same sample rate
#'
#' @return A `data.frame` with the same structure as that of \code{tts}
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
