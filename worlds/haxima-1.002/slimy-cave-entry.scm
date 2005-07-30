;; ----------------------------------------------------------------------------
;; slimy-cave-entry.scm
;;
;; This file defines the on-entry procedure executed whenever the player enters
;; the slimy cave. The purpose of this proc is to respawn some monsters in the
;; place.
;; ----------------------------------------------------------------------------

(define (slimy-cave-entry kplace kplayer)
  (let ((slimes (filter obj-is-green-slime? (kern-place-get-objects kplace))))
    (if (< (length slimes) 1)
        (psummon (mk-loc kplace 10 20)
                 mk-hill-troll
                 (kern-dice-roll "1d4"))))
  #t)
