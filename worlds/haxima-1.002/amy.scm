;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define amy-lvl 1)
(define amy-species sp_human)
(define amy-occ oc_wright)

;;----------------------------------------------------------------------------
;; Schedule
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
  (say knpc "I thought you'd never ask!")
  (join-player knpc)
  (kern-conv-end)
  )

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
       ))

(define (mk-amy)
  (bind 
   (kern-mk-char 
    'ch_amy           ; tag
    "Amy"             ; name
    amy-species         ; species
    amy-occ              ; occ
    s_townswoman     ; sprite
    faction-men      ; starting alignment
    2 4 4            ; str/int/dex
    0 2              ; hp mod/mult
    0 1              ; mp mod/mult
    (max-hp amy-species amy-occ amy-lvl 0 0) ; hp
    0                   ; xp
    (max-mp amy-species amy-occ amy-lvl 0 0) ; mp
    amy-lvl
    #f               ; dead
    'amy-conv         ; conv
    sch_amy           ; sched
    nil              ; special ai
    nil              ; container
    (list
     t_armor_leather
     t_leather_helm
     t_sling
     t_sword
    ))
   (amy-mk)))
