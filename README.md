
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text2speech

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/text2speech.svg?branch=master)](https://travis-ci.com/muschellij2/text2speech)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/text2speech?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/text2speech)
[![CRAN
status](https://www.r-pkg.org/badges/version/text2speech)](https://cran.r-project.org/package=text2speech)
<!-- badges: end -->

The goal of text2speech is to unify different text to speech engines,
such as Google, Microsoft, and Amazon.

## Installation

You can install the GitHub version of `text2speech`:

``` r
devtools::install_github("jhudsl/text2speech")
```

``` r
library(text2speech)
```

## Authentication

This should allow for authentication, which still needs to be set up in
each service separately, to be one function at least:

``` r
tts_auth("google")
#> [1] TRUE
tts_auth("amazon")
#> [1] TRUE
tts_auth("microsoft")
#> [1] TRUE
```

## Voices

Listing out different voices for each service.

``` r
df = tts_voices(service = "microsoft")
print(head(df))
#>                                                             voice
#> 1      Microsoft Server Speech Text to Speech Voice (ar-EG, Hoda)
#> 2     Microsoft Server Speech Text to Speech Voice (ar-SA, Naayf)
#> 3      Microsoft Server Speech Text to Speech Voice (bg-BG, Ivan)
#> 4 Microsoft Server Speech Text to Speech Voice (ca-ES, HerenaRUS)
#> 5     Microsoft Server Speech Text to Speech Voice (cs-CZ, Jakub)
#> 6  Microsoft Server Speech Text to Speech Voice (da-DK, HelleRUS)
#>                language language_code gender   service
#> 1        Arabic (Egypt)         ar-EG Female microsoft
#> 2 Arabic (Saudi Arabia)         ar-SA   Male microsoft
#> 3             Bulgarian         bg-BG   Male microsoft
#> 4       Catalan (Spain)         ca-ES Female microsoft
#> 5                 Czech         cs-CZ   Male microsoft
#> 6                Danish         da-DK Female microsoft

if (tts_google_auth()) {
  df = tts_voices(service = "google")
  print(head(df))
}
#>              voice language language_code gender service
#> 1  ar-XA-Wavenet-A     <NA>         ar-XA FEMALE  google
#> 2  ar-XA-Wavenet-B     <NA>         ar-XA   MALE  google
#> 3  ar-XA-Wavenet-C     <NA>         ar-XA   MALE  google
#> 4 ar-XA-Standard-B     <NA>         ar-XA   MALE  google
#> 5 ar-XA-Standard-A     <NA>         ar-XA FEMALE  google
#> 6 ar-XA-Standard-C     <NA>         ar-XA   MALE  google
if (tts_amazon_auth()) {
  df = tts_voices(service = "amazon")
  print(head(df))
}
#>   voice         language language_code gender service
#> 1 Zeina           Arabic           arb Female  amazon
#> 2 Zhiyu Chinese Mandarin        cmn-CN Female  amazon
#> 3  Naja           Danish         da-DK Female  amazon
#> 4  Mads           Danish         da-DK   Male  amazon
#> 5 Ruben            Dutch         nl-NL   Male  amazon
#> 6 Lotte            Dutch         nl-NL Female  amazon
if (tts_microsoft_auth()) {
  df = tts_voices(service = "microsoft")
  print(head(df))
}
#>                                                             voice
#> 1      Microsoft Server Speech Text to Speech Voice (ar-EG, Hoda)
#> 2     Microsoft Server Speech Text to Speech Voice (ar-SA, Naayf)
#> 3      Microsoft Server Speech Text to Speech Voice (bg-BG, Ivan)
#> 4 Microsoft Server Speech Text to Speech Voice (ca-ES, HerenaRUS)
#> 5     Microsoft Server Speech Text to Speech Voice (cs-CZ, Jakub)
#> 6  Microsoft Server Speech Text to Speech Voice (da-DK, HelleRUS)
#>                language language_code gender   service
#> 1        Arabic (Egypt)         ar-EG Female microsoft
#> 2 Arabic (Saudi Arabia)         ar-SA   Male microsoft
#> 3             Bulgarian         bg-BG   Male microsoft
#> 4       Catalan (Spain)         ca-ES Female microsoft
#> 5                 Czech         cs-CZ   Male microsoft
#> 6                Danish         da-DK Female microsoft
```
