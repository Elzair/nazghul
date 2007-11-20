;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define kalc-lvl 6)
(define kalc-species sp_human)
(define kalc-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; Kalcifax travels (notionally by the moon gates) 
;; to many places, carrying messages and such.
;;----------------------------------------------------------------------------
(define kalc-bed cheerful-bed-2)
(define kalc-mealplace )
(define kalc-workplace )
(define kalc-leisureplace )
(kern-mk-sched 'sch_kalc
               (list 0  0 kalc-bed          "sleeping")
               (list 7  0 bilge-water-seat-5 "eating")
               (list 8  0 enchtwr-hall       "idle")
               (list 11 0 g-fountain         "idle")
               (list 12 0 ghg-s6             "eating")
               (list 13 0 eng-workshop       "idle")
               (list 16 0 trigrave-tavern-hall "idle")
               (list 17 0 trigrave-tavern-table-3b "eating")
               (list 19 0 gt-ws-hall           "idle")
               (list 23 0 kalc-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (kalc-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Kalcifax is a female wizard with considerable knowledge of the moon gates.
;; She travels to many places (notionally by use of the gates),
;; carrying messages and such.
;; Kalcifax is a potential party member.
;;----------------------------------------------------------------------------

;; Basics...
(define (kalc-hail knpc kpc)
  (meet "You meet a cute wizard.")
  (say knpc "Well met, fellow traveler.")
  )

(define (kalc-name knpc kpc)
  (say knpc "I'm Kalcifax. And you are...?")
  (kern-conv-get-reply kpc)
  (say knpc "It's nice to meet you.")
  )

(define (kalc-join knpc kpc)
  (if (is-player-party-member? knpc)
      (say knpc "I've already joined you!")
      (begin
        (say knpc "Ok, this will be fun!")
        (join-player knpc)
        (kern-conv-end)
        )
  ))

(define (kalc-job knpc kpc)
  (say knpc "I travel the gates, running errands for people.")
  )

(define (kalc-bye knpc kpc)
  (say knpc "I'm sure we'll meet again!")
  )

(define (kalc-gate knpc kpc)
  (say knpc "The moongates! Do you know how they work?")
  (if (yes? kpc)
      (say knpc "I don't know why more people don't do it.")
      (say knpc "The phase of Lumis decides where you can enter, "
           "the phase of Ord decides where you emerge!")))

(define (kalc-lumi knpc kpc)
  (say knpc "Lumis is the yellow slow-moving moon."))

(define (kalc-ord knpc kpc)
  (say knpc "Ord is the blue fast-moving moon."))

(define (kalc-engi knpc kpc)
  (say knpc "I'm one of the only people who ever visits the Engineer! "
       "You have to use a moongate to get to his place. "
       "Enter when Ord has waxed almost full."))

(define (kalc-peop knpc kpc)
  (say knpc "I deliver messages and packages for the Engineer, the Enchanter, city officials. "
       "Anyone who needs something delivered safely and fast, and is willing to pay!"))

(define (kalc-pay knpc kpc)
  (say knpc "I do pretty good."))

(define kalc-conv
  (ifc basic-conv

       ;; basics
       (method 'hail kalc-hail)
       (method 'bye  kalc-bye)
       (method 'job  kalc-job)
       (method 'name kalc-name)
       (method 'join kalc-join)
       
       (method 'gate kalc-gate)
       (method 'lumi kalc-lumi)
       (method 'ord  kalc-ord)
       (method 'engi kalc-engi)
       (method 'peop kalc-peop)
       (method 'erra kalc-peop)
       (method 'pay  kalc-pay)
       ))

(define (mk-kalcifax)
  (bind 
   (kern-mk-char 
    'ch_kalc           ; tag
    "Kalcifax"             ; name
    kalc-species         ; species
    kalc-occ              ; occ
    s_blue_wizard
    faction-men      ; starting alignment
    0 7 0            ; str/int/dex
    (/ pc-hp-off 2)  ; hp bonus
    (/ pc-hp-gain 2) ; hp per-level bonus
    pc-mp-off        ; mp bonus
    pc-mp-gain       ; mp per-level bonus
    max-health ; hp
    -1  ; xp
    max-health ; mp
    0
    kalc-lvl
    #f               ; dead
    'kalc-conv         ; conv
    sch_kalc           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list            ;; readied
     t_staff
     )
    )
   (kalc-mk)))
