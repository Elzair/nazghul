;; ----------------------------------------------------------------------------
;; af-entry.scm
;;
;; This file defines the on-entry procedure executed whenever the player enters
;; the abandoned farm place. The purpose of this proc is to respawn some
;; monsters in the place.
;;
;; I never want there to be more than 5 each of trolls or spiders. I'll roll to
;; add monsters if there are 2 or less, and I'll never add more than 3.
;; ----------------------------------------------------------------------------

(define (af-entry kplace kplayer)
  (let ((chars (filter kern-obj-is-char? (kern-place-get-objects kplace))))
    (let ((trolls (filter char-is-troll? chars))
          (spiders (filter char-is-spider? chars)))
      (if (< (length trolls) 2)
          (psummon (mk-loc kplace 19 13)
                  mk-hill-troll
                  (kern-dice-roll "1d2")))
      (if (< (length spiders) 2)
          (psummon (mk-loc kplace 23 3)
                  mk-wood-spider
                  (kern-dice-roll "1d2")))))
  #t)
