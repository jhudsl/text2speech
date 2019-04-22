
<!-- README.md is generated from README.Rmd. Please edit that file -->

# text2speech

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/text2speech.svg?branch=master)](https://travis-ci.com/muschellij2/text2speech)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/muschellij2/text2speech?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/text2speech)
<!-- badges: end -->

The goal of text2speech is to unify different text to speech engines,
such as Google, Microsoft, and Amazon.

## Installation

You can install the released version of text2speech from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("text2speech")
```

``` r
library(text2speech)
```

## Authentication

This should allow for authentication, which still needs to be set up in
each syervice separately, to be one function at least:

``` r
tts_auth("google")
#> Successfully authenticated via ~/Dropbox/Projects/cbds_translate/RClass-Translator.json
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
#> 4 ar-XA-Standard-A     <NA>         ar-XA FEMALE  google
#> 5 ar-XA-Standard-B     <NA>         ar-XA   MALE  google
#> 6 ar-XA-Standard-C     <NA>         ar-XA   MALE  google
if (tts_amazon_auth()) {
  df = tts_voices(service = "amazon")
  print(head(df))
}
#>     voice   language language_code gender service
#> 1   Filiz    Turkish         tr-TR Female  amazon
#> 2  Astrid    Swedish         sv-SE Female  amazon
#> 3 Tatyana    Russian         ru-RU Female  amazon
#> 4   Maxim    Russian         ru-RU   Male  amazon
#> 5  Carmen   Romanian         ro-RO Female  amazon
#> 6    Inês Portuguese         pt-PT Female  amazon
```
