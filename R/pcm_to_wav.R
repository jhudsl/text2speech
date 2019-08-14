#' Convert PCM to Wav
#'
#' @param input output from [aws.polly::get_synthesis] or PCM filename
#' @param output output file for Wav file
#' @param extensible passed to [tuneR::writeWave]
#'
#' @return A filename of the output
#' @export
pcm_to_wav = function(
  input,
  output = tempfile(fileext = ".wav"),
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
  wav = tuneR::Wave(pcm, samp.rate = 16000, bit = 16, pcm = TRUE)
  tuneR::writeWave(object = wav, filename = output, extensible = extensible)
  return(output)
}
