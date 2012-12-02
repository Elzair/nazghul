;; This file is the startup script for Haxima. It is invoked any time the
;; player wants to start a new Haxima game. Once the player has saved a game
;; and wants to resume it, the save file should be invoked instead of this.

;; Every game startup script must begin by loading naz.scm. It will define some
;; common utilities and load the game system.
(load "naz.scm")

;; Load the data specific to the start of a new session. This adds all the
;; starting maps, characters, objects, etc.
(load "data/init.scm")

;; Register a procedure to run at start-of-game that will put the player on the
;; world map at coordinates (15, 15). The second arg to kern-add-hook must be a
;; quoted symbol, so it must be a named procedure (ie, not a lambda expression).
(define (new-start kplayer)
  (println "starting....")
  (kern-obj-put-at kplayer (list p_world 10 5)))
(kern-add-hook 'new_game_start_hook 'new-start)
