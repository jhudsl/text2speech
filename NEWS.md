# text2speech 1.0.0

* Added Coqui TTS as a text-to-speech engine to `tts()` and `tts_voices()`

# text2speech 0.3.0

* Updated the binding wavs so that they have the same sample rate

# text2speech 0.2.13

* Added duration to the synthesis output.

# text2speech 0.2.12

* Fix bug in `tts_bind_wav` when trying to bind audio together for named list.

# text2speech 0.2.11

* The `tts_speak_engine` is now enabled on attachment so that it works with `knitr` out of the box.

# text2speech 0.2.9

* Removing `aws.polly` links as it is a `Suggests` field now as Prof. Ripley states as `aws.polly` has been orphaned.

# text2speech 0.2.9

* Making `aws.polly` a `Suggests` as Prof. Ripley states as `aws.polly` has been orphaned.

# text2speech 0.2.7

* Fixing bug with sample rate for Amazon.

# text2speech 0.2.6

* List voices allows passing of authentication down there.

# text2speech 0.2.5

* Fixed small issue with `aws.polly::list_voices`.  Will fix with new version of `aws.polly`: https://github.com/cloudyr/aws.polly/pull/6.

# text2speech 0.2.4

* Fixed ordering problem of `tts`.

# text2speech 0.2.3

* More examples added and `\dontrun` removed.

# text2speech 0.2.1

* Added to the vignette and the tests for translation.
* Fixing errors with authentication if the keys are not set.

# text2speech 0.2.0

* Added a `NEWS.md` file to track changes to the package.
