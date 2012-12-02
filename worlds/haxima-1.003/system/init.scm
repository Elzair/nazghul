;; This is the system load file for a Haxima-like game. It loads all the files
;; that contain object definitions, rules and procedures that are common to any
;; Haxima-like game.

;; The order of these files is important.
(load "system/constants.scm")
(load "system/passability.scm")
(load "system/sprite-sets.scm")
(load "system/sprites.scm")
(load "system/terrains.scm")
(load "system/palette.scm")
(load "system/occupations.scm")
(load "system/species.scm")
(load "system/obj.scm")
(load "system/misc.scm")
