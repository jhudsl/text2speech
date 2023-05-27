#' Convert PCM to Wav
#'
#' @param input output from `get_synthesis`` from \code{aws.polly} or
#' PCM filename
#' @param output output file for Wav file
#' @param extensible passed to [tuneR::writeWave]
#' @param sample_rate Sampling rate for [tuneR::Wave]
#' @return A filename of the output
#' @export
#' @examples \dontrun{
#' fname = system.file("extdata", "pcm_file.wav", package = "text2speech")
#' res = pcm_to_wav(fname)
#' testthat::expect_error(tuneR::readWave(fname))
#' testthat::expect_is(tuneR::readWave(res), "Wave")
#' }
#' \dontrun{
#' if (requireNamespace("aws.polly", quietly = TRUE)) {
#' text = "hey, ho, let's go!"
#' if (tts_amazon_auth()) {
#'    res = tts_amazon(text, output_format = "wav")
#' }
#' }
#' }
pcm_to_wav = function(
  input,
  output = tempfile(fileext = ".wav"),
  sample_rate = 16000,
  extensible = FALSE) {


  tfile = tempfile(fileext = ".pcm")
  if (is.raw(input)) {
    writeBin(input, con = tfile)
    input = tfile
  }
  stopifnot(file.exists(input))
  sz = file.size(input)
  buffer = 1000
  pcm <- readBin(input, what = integer(),
                 n = 2 * sz + buffer,
                 size = 2, endian = "little", signed = TRUE)
  wav = tuneR::Wave(pcm, samp.rate = sample_rate, bit = 16, pcm = TRUE)
  tuneR::writeWave(object = wav, filename = output, extensible = extensible)
  return(output)
}
