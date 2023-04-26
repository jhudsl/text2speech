#' Convert Text to Speech using Google Cloud Text-to-Speech API
#' @export
#' @rdname tts
#' @param voice A full voice name that can be passed to the
#' service, such as the
#' argument `voice` for `get_synthesis`` from \code{aws.polly}, or
#' or [mscstts::ms_synthesize()] or the
#' `name` argument for [googleLanguageR::gl_talk()]
#' @examples
#' tts_default_voice("amazon")
#' tts_default_voice("google")
#' tts_default_voice("microsoft")
tts_google = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "en-US-Standard-C",
    bind_audio = TRUE,
    ...) {

  limit = 5000
  output_format = match.arg(output_format)
  audio_type = output_format

  output_format = switch(
    output_format,
    "mp3" = "MP3",
    "wav" = "LINEAR16")

  res = lapply(text, function(string) {
    strings = tts_split_text(string, limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)
      out = googleLanguageR::gl_talk(
        tt,
        output = output,
        audioEncoding = output_format,
        name = voice,
        ...)
    }, FUN.VALUE = character(1L))
    names(res) = NULL
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    # out = do.call(tuneR::bind, out)
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)
  res$audio_type = audio_type

  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }

  return(res)
}

#' Convert Text to Speech using Amazon Polly
#' @export
#' @rdname tts
#' @examples \dontrun{
#' text='<speak>
#'   He was caught up in the game.<break time="1s"/> In the middle of the
#'   10/3/2014 <sub alias="World Wide Web Consortium">W3C</sub> meeting,
#'   he shouted, "Nice job!" quite loudly. When his boss stared at him, he repeated
#'   <amazon:effect name="whispered">"Nice job,"</amazon:effect> in a
#'   whisper.
#' </speak>'
#' }
tts_amazon = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "Joanna",
    bind_audio = TRUE,
    ...) {
  if (!requireNamespace("aws.polly", quietly = TRUE)) {
    stop(paste0(
      "This function requires aws.polly to operate",
      " please use\n",
      "install.packages('aws.polly')\n",
      "to use these functions"))
  }
  limit = 1500
  output_format = match.arg(output_format)
  audio_type = output_format

  sample_rate = switch(
    output_format,
    "mp3" = 24000,
    "wav" = 16000)
  output_format = switch(
    output_format,
    "mp3" = "mp3",
    "wav" = "pcm")

  args = list(...)
  if (is.null(args$rate)) {
    args$rate = sample_rate
  }
  if (!is.null(args$format)) {
    warning(
      paste0(
        "format was specified in ... for tts_amazon",
        ", this should be specified in output_format argument, format",
        " is overridden")
    )
  }
  res = lapply(text, function(string) {
    strings = tts_split_text(string,
                             limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)

      args$text = tt
      args$voice = voice
      args$format = output_format

      out = do.call(
        aws.polly::get_synthesis,
        args = args)

      writeBin(out, con = output)
      if (audio_type == "wav") {
        output = pcm_to_wav(input = output, sample_rate = args$rate)
      }
      output
    }, FUN.VALUE = character(1L))
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    df
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)
  res$audio_type = audio_type
  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }

  return(res)

}


#' Convert Text to Speech using Microsoft Cognitive Services API
#' @export
#' @rdname tts
tts_microsoft = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "Microsoft Server Speech Text to Speech Voice (en-US, ZiraRUS)",
    bind_audio = TRUE,
    ...) {

  limit = 800
  output_format = match.arg(output_format)
  audio_type = output_format

  output_format = switch(
    output_format,
    "mp3" = "audio-24khz-160kbitrate-mono-mp3",
    "wav" = "riff-24khz-16bit-mono-pcm")


  res = lapply(text, function(string) {
    strings = tts_split_text(string,
                             limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)
      out = mscstts::ms_synthesize(
        tt,
        output_format = output_format,
        voice = voice,
        ...)
      writeBin(out$content, con = output)
      output
    }, FUN.VALUE = character(1L))
    names(res) = NULL
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    # out = do.call(tuneR::bind, out)
    df
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)

  res$audio_type = audio_type
  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }

  return(res)
}


tts_coqui <- function(
    text,
    # Set Default as WAV and put it in Documentation
    output_format = c("wav", "mp3"),
    voice = "tacotron2-DDC",
    bind_audio = TRUE,
    ...) {
  # Is there a max number of limits that coqui TTS takes? (https://github.com/coqui-ai/TTS/discussions/917)
  limit <- 2500
  output_format = "wav"
  audio_type = output_format
  bind_audio <- TRUE
  # IF we don't need Linear16, we can delete this switch() chunk
  output_format = switch(
    output_format,
    "wav" = "LINEAR16")

  voice <- "en-US-Standard-C"
  # In ari::ari_stitch(), where tts_coqui() is called, only WAV is an option for audio file
  # output_format = match.arg(output_format)
  text <- c("Good evening, everyone. My name is Howard, and I'm here to talk to you about the exciting world of computer science. Computers have become an integral part of our lives, and the field of computer science is all about understanding how they work and how we can use them to solve real-world problems.
In this lecture, we'll be covering some of the basics of computer science and what makes it such a fascinating and important field of study.",
            "First, let's talk about what computer science is. At its core, computer science is all about understanding how computers work and how to use them to solve problems.
          This includes everything from understanding the basics of programming languages to designing and building complex software systems.",
            "One of the key concepts in computer science is algorithms. An algorithm is a set of instructions that a computer can follow to solve a problem. Algorithms can be simple or complex, and they're used in everything
          from sorting data to powering search engines.",
            "Data structures are the ways in which we organize and store data in a computer. Examples of data structures include arrays, linked lists, and trees.",
            "Of course, one of the most important aspects of computer science is programming. Programming is the process of writing code that tells a computer what to do.
          There are many different programming languages out there, from the classic languages like C++ and Java to newer languages like Python and Swift.",
            "Finally, computer science is a field that's constantly evolving. As technology advances and new problems arise, computer scientists are always developing new tools and techniques to solve them.
          Whether it's developing new programming languages, creating more efficient algorithms, or designing more powerful hardware, computer science is a field that's always on the cutting edge.")

  res = lapply(text, function(string) {
    string_processed = tts_split_text(string, limit = limit)

    res = vapply(string_processed, function(tt) {
      output_path = tts_temp_audio(audio_type)
      tts_args <- paste0("--text ", shQuote(tt), " ",
                         "--model_name tts_models/en/ljspeech/tacotron2-DDC_ph --vocoder_name vocoder_models/en/ljspeech/univnet",
                         " ", "--out_path /private", output_path)
      # # Run command with temporary system search path
      res <- withr::with_path("/opt/homebrew/Caskroom/miniforge/base/bin",
                              system2("tts", tts_args))
      # Output file path
      output_path
    }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = string_processed,
                       wav = out, file = res)
  })

  names(res) = seq_along(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)
  res$audio_type = audio_type

  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }



}

#' @rdname tts
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
