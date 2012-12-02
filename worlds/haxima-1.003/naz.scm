;; naz.scm is the traditional name of the file that loads the game system. It
;; needs to be the first thing loaded by a session startup script, and the
;; nazghul engine is hard-coded to ensure every saved game starts by loading
;; this.

;; Load the top-level init. This adds some generic scheme utilities.
(load "init.scm")

;; Load some generic custom utilities.
(load "utils.scm")

;; Load the game system. This adds all the object types and various constants
;; and procedures needed for any haxima-style game.
(load "system/init.scm")
