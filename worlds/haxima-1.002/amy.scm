;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define amy-lvl 1)
(define amy-species sp_human)
(define amy-occ oc_wright)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the Poor House (at least until such time as she joins the Wanderer).
;;----------------------------------------------------------------------------
(define amy-bed poorh-bed2)
(define amy-mealplace poorh-sup2)
(define amy-workplace poorh-pasture)
(define amy-leisureplace poorh-dining)
(kern-mk-sched 'sch_amy
               (list 0  0 amy-bed          "sleeping")
               (list 7  0 amy-mealplace    "eating")
               (list 8  0 amy-workplace    "working")
               (list 12 0 amy-mealplace    "eating")
               (list 13 0 amy-workplace    "working")
               (list 18 0 amy-mealplace    "eating")
               (list 19 0 amy-leisureplace "idle")
               (list 22 0 amy-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (amy-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Amy is a female tinker, fallen upon hard times.
;; She currently dwells in the Poor House.
;; Amy is a potential party member.
;;----------------------------------------------------------------------------

;; Basics...
(define (amy-hail knpc kpc)
  (meet "You meet a practical-looking tinker woman.")
  (say knpc "Hello.")
  )

(define (amy-name knpc kpc)
  (say knpc "You can call me Amy.")
  )

(define (amy-join knpc kpc)
  (if (is-player-party-member? knpc)
      (say knpc "I already joined you!")
      (begin
        (say knpc "I thought you'd never ask!")
        (join-player knpc)
        (kern-conv-end)
        )))

(define (amy-job knpc kpc)
  (say knpc "Well, I'm a tinker by trade, "
       "but I haven't had much luck finding work lately.")
  )

(define (amy-bye knpc kpc)
  (say knpc "So long.")
  )

(define (amy-mean knpc kpc)
  (say knpc "He's great. "
       "I don't know where I'd go if it weren't for the poor house. "
       "He doesn't even stare at my boobs all that much.")
  )

(define (amy-tink knpc kpc)
  (say knpc "A tinker is a wandering wright. "
       "We travel from town to town, fixing things up for people.")
  )

(define (amy-luck knpc kpc)
  (say knpc "People are nervous of strangers now, "
       "what with the Accursed and all.")
  )
  
(define (amy-accu knpc kpc)
  (say knpc "The Accursed are a secret cult who "
       "follow evil ways.")
  )

;; Quest-related

(define amy-conv
  (ifc basic-conv

       ;; basics
       (method 'hail amy-hail)
       (method 'bye amy-bye)
       (method 'job amy-job)
       (method 'name amy-name)
       (method 'join amy-join)
       

       (method 'mean amy-mean)
       (method 'tink amy-tink)
       (method 'luck amy-luck)
       (method 'accu amy-accu)
       ))

(define (mk-amy)
  (bind 
   (kern-mk-char 
    'ch_amy           ; tag
    "Amy"             ; name
    amy-species         ; species
    amy-occ              ; occ
    s_companion_tinker ; sprite
    faction-men      ; starting alignment
    2 4 4            ; str/int/dex
    pc-hp-off  ; hp bonus
    pc-hp-gain ; hp per-level bonus
    1 ; mp off
    1 ; mp gain
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    amy-lvl
    #f               ; dead
    'amy-conv         ; conv
    sch_amy           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list
     t_armor_leather
     t_leather_helm
     t_sling
     t_sword
    ))
   (amy-mk)))
