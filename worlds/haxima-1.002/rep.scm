;;----------------------------------------------------------------------------
;; Player reputation management
;;
;; Crimes are worth various points. When the player crosses the line he becomes
;; an outlaw (his faction is changed). Over time the points decrement, and
;; eventually, if he stops committing crimes, his faction will revert to
;; normal. In general the third misdemeanor, second felony or first murder
;; pushes the player over the outlaw line.
;;
;; Murder should result in an outlaw period of one week. To allow for accidents
;; one assault alone will have no ill effect, but the second should result in a
;; day of outlawry.
;; 
;; TURNS_PER_MINUTE is currently hardcoded in src/common.h as 20.
;;
;;   1 minute = 20 turns
;;   1 hour   = 1200 turns
;;   1 day    = 28,800 turns
;;   1 week   = 201,600 turns
;; 
;; Let x be the outlaw boundary (measured in turns). Murder should result in 1
;; week:
;;
;;   murder = x + 201,600
;;
;; 2 assaults results in 1 day:
;;
;;   2 * assault = x + 28,800
;;       assault = x/2 + 14,400
;;
;; And 1 assault should be just below the cutoff:
;;
;;       assault = x - 1 
;;        2x - 2 = x + 28,800
;;             x = 28,802
;; 
;; Round x to 30000 (this is the boundary at which the player faction changes)
;; and recompute:
;; 
;;   assault = 15,000 + 14,400 = 29,400 turns
;;   murder = 30,000 + 201,600 = 231,600 turns
;;   max = 1,000,000 turns
;;

;;----------------------------------------------------------------------------
;; Constants
(define outlaw-rep -30000)
(define max-bad-rep -1000000)
(define assault-lethal 29400)
(define assault-non-lethal 15000)
(define murder-rep 231600)


;;----------------------------------------------------------------------------
;; Utility methods
(define (player-set-rep! val)
  (if (> val max-bad-rep)
      (let ((kplayer (kern-get-player))
            (orep (player-get 'rep)))
        (player-set! 'rep val)
        (if (< val outlaw-rep)
            (if (not (equal? (kern-being-get-base-faction kplayer)
                             faction-player-outlaw))
                (begin
                  (kern-being-set-base-faction kplayer faction-player-outlaw)
                  (kern-obj-add-effect kplayer ef_outlaw nil)
                  (kern-log-msg "^c+rYou are an outlaw!^c-")
                  ))
            (if (not (equal? (kern-being-get-base-faction kplayer)
                             faction-player))
                (begin
                  (kern-being-set-base-faction kplayer faction-player)
                  (kern-obj-remove-effect kplayer ef_outlaw)
                  (kern-log-msg "^c+gYou are no longer an outlaw!^c-")
                  ))
            ))))

;;----------------------------------------------------------------------------
;; Rap Sheet
;;
(define (crime-mk when what whom where)
  (list what when where whom))

(define (crime-date-str crime)
  (let ((date (cadr crime)))
    (string-append (number->string (time-year date)) "/"
                   (number->string (time-month date)) "/"
                   (number->string (+ (* (time-week date) 7)
                                      (time-day date))))))

(define (crime-descr crime) (car crime))

(define (crime-victim crime) (cadddr crime))

(define (crime-where crime) (caddr crime))

(define (player-add-crime! what kvictim)
  (let ((rap (player-get 'rapsheet))
        (where (kern-place-get-name (loc-place (kern-obj-get-location kvictim))))
        (towhom (kern-obj-get-name kvictim))
        )
    (player-set! 'rapsheet (cons (crime-mk what (kern-get-time) towhom where)
                                 rap))))


;;----------------------------------------------------------------------------
;; Turn-end-hook
;;
;; Cool off the reputation over time.
;;
(define (rep-update-after-turn kplayer)
  (let ((rep (player-get 'rep))
        (ticks (kern-ticks-per-turn)))
    (cond ((> rep 0) 
           (player-set-rep! (- rep (min ticks rep))))
          ((< rep 0) 
           (player-set-rep! (+ rep (min ticks (- 0 rep)))))
          )))

(kern-add-hook 'turn_end_hook 'rep-update-after-turn)

;;----------------------------------------------------------------------------
;; Post-attack hook
;;
;; Check for attacks on friendlies and apply the consequences: player
;; reputation change, calling the guard, panicking the village, etc.
;;
(define (rep-update-after-attack kpc knpc degree)
  (if (not (is-hostile? kpc knpc))
      (let ((rep (player-get 'rep)))
        (cond ((and (is-dead? knpc)
                    (can-be-seen-by-any? kpc (all-allies knpc)))
               ;; Murder with witnesses...
               (player-set-rep! (- rep murder-rep))
               (player-add-crime! "murder" knpc)
               (raise-alarm knpc #f)
               )
              (else
               ;; The victim is a witness, might flip to hostile after this
               (player-set-rep! (- rep degree))
               (player-add-crime! "assault" knpc)
               (if (in-town? knpc)
                   (cond ((not (is-hostile? kpc knpc))
                          ;; not hostile yet, but not happy
                          (taunt knpc nil (list "Hey, watch it!" 
                                                "Watch where you point that thing!"
                                                "Behave!" 
                                                "Take it outside!" 
                                                "Keep it up and see what happens." 
                                                "I dare you to try that again."
                                                "You want to start something?"
                                                "You're lucky I'm in a good mood."
                                                "You got a problem?")
                                 ))
                         (else
                          ;; that's it, call the guards
                          (raise-alarm knpc #t)
                          ))))))))

(define (rep-post-attack-hook kpc knpc)
  (rep-update-after-attack kpc knpc assault-lethal))

(kern-add-hook 'post_attack_hook 'rep-post-attack-hook)

;;----------------------------------------------------------------------------
;; Reputation UI
;;
;; Add a pane to the ztats window that will show details of the player's
;; current reputation, including time left on outlawry or probation and a rap
;; sheet of past crimes.
;;
;; Use 'rz' as the shortened prefix for 'rep-ztats'.
;;

(define (rz-mk) (list nil nil 0 0 0))
(define (rz-dims gob) (list-ref gob 0))
(define (rz-dims! gob dims) (set-car! gob dims))
(define (rz-text gob) (list-ref gob 1))
(define (rz-text! gob val) (list-set-ref! gob 1 val))
(define (rz-cur-entry gob) (list-ref gob 2))
(define (rz-cur-entry! gob val) (list-set-ref! gob 2 val))
(define (rz-max-entry! gob val) (list-set-ref! gob 3 val))
(define (rz-max-entry gob) (list-ref gob 3))
(define (rz-top-entry gob) (list-ref gob 4))
(define (rz-top-entry! gob val) (list-set-ref! gob 4 val))


(define (rz-enter self kparty dir rect)
  (define (rep-text rep rapsheet)
    (define (rep-hdr)
      (cond ((= rep 0)
             ;; rep cooled off, but may have a rap sheet
             (cond ((null? rapsheet)  "^c+gModel Citizen^c-")
                   (else "^c+yKnown Miscreant^c-")))
            (else
             ;; rep still hot, emit some flavor text
             (cond ((<= rep max-bad-rep) "^c+yBlackguard^c-")
                   ((<= rep (/ max-bad-rep 2)) "^c+yVillain^c-")
                   ((<= rep (/ max-bad-rep 8)) "^c+yVarlet^c-")
                   ((<= rep (/ max-bad-rep 32)) "^c+yKnave^c-")
                    ((<= rep (/ max-bad-rep 128)) "^c+yScoundrel^c-")
                    (else "^c+yTroublemaker^c-")))))
    (define (cooloff->str rep)
      (if (= rep 0) 
          ""
          (let ((units (car (last-pair (turns->time (- rep))))))
            (string-append "^c+c("
                           (number->string (car units))
                           " more "
                           (cdr units)
                           (if (> (car units) 1)
                               "s"
                               "")
                           ")^c-"))))
    (define (rep-entry crime)
      (string-append (crime-date-str crime) ": "
                     (crime-descr crime) "ed "
                     (crime-victim crime) " in "
                     (crime-where crime)))
    (cons (rep-hdr)
          (cons (cooloff->str rep)
                (map rep-entry rapsheet))))
  (kern-status-set-title "Reputation")
  (rz-dims! self rect)
  (rz-text! self (rep-text (player-get 'rep)
                           (player-get 'rapsheet)))
  )

(define (rz-scroll self dir)
  (let* ((top (rz-top-entry self))
         (winh (/ (rect-h (rz-dims self)) 
                  kern-ascii-h))
         (maxtop (max 0 (- (length (rz-text self)) winh)))
        )
    (cond ((= dir scroll-up) (rz-top-entry! self (max 0 (- top 1))) #t)
          ((= dir scroll-down) (rz-top-entry! self (min maxtop (+ top 1))) #t)
          ((= dir scroll-pageup) (rz-top-entry! self (max 0 (- top winh))) #t)
          ((= dir scroll-pagedown) (rz-top-entry! self (min maxtop (+ top winh))) #t)
          ((= dir scroll-top) (rz-top-entry! self 0) #t)
          ((= dir scroll-bottom) (rz-top-entry! self maxtop) #t)
          (else #f))))

(define (rz-paint self)
  (let ((dims (rz-dims self)))
    (define (scrnprn lst rect)
      (if (null? lst)
          (kern-screen-update dims)
          (begin
            (kern-screen-print rect 0 (car lst))
            (scrnprn (cdr lst) (rect-crop-down rect kern-ascii-h)))))
    (scrnprn (list-tail (rz-text self) 
                        (rz-top-entry self))
             dims)))

(kern-ztats-add-pane rz-enter 
                     rz-scroll 
                     rz-paint 
                     nil 
                     (rz-mk))