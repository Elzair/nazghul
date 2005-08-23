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

(define (af-spawn-spiders kplace n)
  (if (> n 0)
      (begin
        (psummon (place-random-corner kplace)
                 mk-wood-spider 
                 1)
        (af-spawn-spiders kplace (- n 1)))))

(define (af-entry kplace kplayer)
  (let ((chars (kern-place-get-beings kplace)))
    (let ((trolls (filter is-troll? chars))
          (spiders (filter is-spider? chars)))
      (if (< (length trolls) 2)
          (psummon (mk-loc kplace 19 13)
                  mk-troll
                  (kern-dice-roll "1d2")))
      (if (< (length spiders) 2)
          (af-spawn-spiders kplace (kern-dice-roll "1d2")))))
  #t)
