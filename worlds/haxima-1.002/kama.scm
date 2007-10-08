;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define kama-lvl 4)
(define kama-species sp_forest_goblin)
(define kama-occ oc_wrogue)
(define kama-exit-x 6)
(define kama-exit-y 2)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define kama-cell gtl-cell1)
(kern-mk-sched 'sch_kama
               (list 0  0 kama-cell        "sleeping")
               (list 7  0 kama-cell        "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (kama-mk jail-door-tag) (list #f #f jail-door-tag))
(define (kama-gave-food? gob) (car gob))
(define (kama-gave-food! gob) (set-car! gob #t))
(define (kama-joined-once? gob) (cadr gob))
(define (kama-joined-once! gob) (set-car! (cdr gob) #t))
(define (kama-get-jail-door-tag gob) (caddr gob))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (kama-hail knpc kpc)
  (meet "You meet a calm goblin who regards you with a fearless, calculating gaze.")
  (if (kama-gave-food? (gob knpc))
      (say knpc "Bonaha.")
      (say knpc "Nuki?")
      ))

(define (kama-no knpc kpc)
  (if (kama-gave-food? (gob knpc))
      (say knpc "[He points to himself] Kama.")
      (kama-default knpc kpc)))

(define (kama-me knpc kpc)
  (if (not (kama-gave-food? (gob knpc)))
      (kama-default knpc kpc)
      (begin
        (say knpc "Ninto. [He points to you] Zuto?")
        (if (yes? kpc)
            (say knpc "[He nods]")
            (say knpc "[He chuckles as if he disbelieves you]")))))

(define (kama-jo knpc kpc)
  (define (exit-point)
    (mk-loc (kobj-place knpc)
            kama-exit-x
            kama-exit-y))
  (define (door-still-locked?)
    (let ((kdoor (eval (kama-get-jail-door-tag (gob knpc)))))
      (cond ((null? kdoor) (error "Kama's door tag is undefined!") #t)
            (else
             (let ((gob (kobj-gob kdoor)))
               (or (door-locked? gob)
                   (door-magic-locked? gob)))))))
  (define (rejoin)
    (say knpc "Ha! Iki!")
    (join-player knpc)
    (kern-conv-end)
    )
  (define (join-first-time)    
    (say knpc "Hajo! Bona ka ruka! Iki [" (loc-x angriss-lair-loc)
         " " (loc-y angriss-lair-loc) "]")
    (kama-joined-once! (gob knpc))
    (join-player knpc)
    ;; Improve the player's relations with forest goblins
    (kern-dtable-inc faction-player faction-forest-goblin)
    (kern-dtable-inc faction-player faction-forest-goblin)
    (kern-conv-end)
    )
  (if (is-player-party-member? knpc)
      (say knpc "[He looks confused] Ha...")
      (if (kama-joined-once? (gob knpc))
          (rejoin)
          (if (not (kama-gave-food? (gob knpc)))
              (kama-default knpc kpc)
              (if (door-still-locked?)
                  (say knpc "[He points to the cell door and shrugs, maybe if you opened it...]")
                  (join-first-time)
                  )))))

(define (kama-food knpc kpc)
  (kern-log-msg "[Do you give him some food?]")
  (define (no-food)
    (say knpc "[He grunts and turns away]")
    (kern-conv-end))
  (define (yes-food)
    (kama-gave-food! (gob knpc))
    (say knpc "[He gobbles it down hungrily and smacks his lips] "
         "Ha nuki! [He points to you] Bonaha."))
  (if (yes? kpc)
      (if (> (get-food-donation knpc kpc) 0)
          (yes-food)
          (no-food))
      (no-food)))
          
(define (kama-rune knpc kpc)
  (if (not (kama-gave-food? (gob knpc)))
      (kama-default knpc kpc)
      (if (any-in-inventory? kpc rune-types)
          (say knpc "[You show him a Rune. He nods uneasily] Ruka.")
          (say knpc "[He looks confused as you try to describe a Rune]"))))

(define (kama-ruka knpc kpc)
  (if (kama-joined-once? (gob knpc))
      (say knpc "Iki [" (loc-x angriss-lair-loc)
           " " (loc-y angriss-lair-loc) "]")
      (begin
        (say knpc "[In the dust on the cell floor he draws a circle with jointed legs. A spider. He then points to you, himself, and then he scuffs out the spider.]")
        (prompt-for-key)
        (say knpc "[You get the impression he is proposing an alliance with you against the spider, or whatever it is.]"))))
      
(define (kama-default knpc kpc)
  (say knpc "[no response]"))

(define (kama-bye knpc kpc)
  (say knpc "[his expression never changes]"))

(define (kama-clov knpc kpc)
  (if (not (kama-gave-food? (gob knpc)))
      (kama-default knpc kpc)  
      (say knpc "[He looks puzzled at first, but then nods] Ruka ka choto.")))

(define (kama-leav knpc kpc)
  (if (is-player-party-member? knpc)
      (begin
        (say knpc "Kama tujo?")
        (if (yes? kpc)
            (begin
              (if (kern-char-leave-player knpc)
                  (begin
                    (say knpc "Kama iki")
                    (kern-conv-end))
                  (say knpc "Kama tu iki")))
            (kern-log-msg "[He looks relieved]")))
      (kern-log-msg "[He looks confused]")))

(define kama-conv
  (ifc nil
       (method 'hail kama-hail)
       (method 'default kama-default)
       (method 'bye kama-bye)
       (method 'no kama-no)
       (method 'me kama-me)
       (method 'jo kama-jo)
       (method 'food kama-food)
       (method 'rune kama-rune)
       (method 'ruka kama-ruka)
       (method 'clov kama-clov)
       (method 'leav kama-leav)
    ))

(define (mk-kama jail-door-tag)
  (bind 
    (kern-mk-char 
     'ch_kama           ; tag
     "Kama"             ; name
     kama-species         ; species
     kama-occ              ; occ
     s_fgob_civilian  ; sprite
     faction-men      ; starting alignment
     2 0 10            ; str/int/dex
     pc-hp-off  ; hp bonus
     pc-hp-gain ; hp per-level bonus
     0 ; mp off
     0 ; mp gain
     max-health ; hp
     -1                   ; xp
     max-health ; mp
     0
     kama-lvl
     #f               ; dead
     'kama-conv         ; conv
     sch_kama           ; sched
     nil              ; special ai
     nil              ; container
     nil              ; readied
     )
   (kama-mk jail-door-tag)))
