# .onLoad <- function (libname, pkgname) {
# }

.onAttach <- function(libname, pkgname) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(speak = tts_speak_engine)
  }
}
