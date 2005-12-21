;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define kalc-lvl 6)
(define kalc-species sp_human)
(define kalc-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define kalc-bed cheerful-bed-2)
(define kalc-mealplace )
(define kalc-workplace )
(define kalc-leisureplace )
(kern-mk-sched 'sch_kalc
               (list 0  0 kalc-bed          "sleeping")
               (list 7  0 bilge-water-seat-5 "eating")
               (list 8  0 enchtwr-hall       "idle")
               (list 11  0 g-fountain         "idle")
               (list 12 0 ghg-s8             "eating")
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
  (say knpc "I thought you'd never ask!")
  (join-player knpc)
  (kern-conv-end)
  )

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
  (say knpc "Lumis is the slow-moving moon."))

(define (kalc-ord knpc kpc)
  (say knpc "Ord is the fast-moving moon."))

(define (kalc-engi knpc kpc)
  (say knpc "I'm one of the only people who ever visits the Engineer! "
       "You have to use a moongate to get to his place. "
       "Enter when Ord is a waning quarter moon."))

(define (kalc-peop knpc kpc)
  (say knpc "I deliver messages and packages for the Engineer, the Enchanter, city officials. "
       "Anyone who needs to something delivered safely and fast, and is willing to pay!"))

(define (kalc-pay knpc kpc)
  (say knpc "I do pretty good."))

(define kalc-conv
  (ifc basic-conv

       ;; basics
       (method 'hail kalc-hail)
       (method 'bye kalc-bye)
       (method 'job kalc-job)
       (method 'name kalc-name)
       (method 'join kalc-join)
       
       (method 'gate kalc-gate)
       (method 'lumi kalc-lumi)
       (method 'ord  kalc-ord)
       (method 'engi kalc-engi)
       (method 'peop kalc-peop)
       (method 'erra kalc-peop)
       (method 'pay kalc-pay)
       ))

(define (mk-kalcifax)
  (bind 
   (set-level
   (kern-mk-char 
    'ch_kalc           ; tag
    "Kalcifax"             ; name
    kalc-species         ; species
    kalc-occ              ; occ
    s_companion_wizard ; sprite
    faction-men      ; starting alignment
    0 5 0            ; str/int/dex
    3 2              ; hp mod/mult
    3 2              ; mp mod/mult
    (max-hp kalc-species kalc-occ kalc-lvl 3 2) ; hp
    (lvl-xp kalc-lvl)  ; xp
    (max-mp kalc-species kalc-occ kalc-lvl 3 2) ; mp
    kalc-lvl
    #f               ; dead
    'kalc-conv         ; conv
    sch_kalc           ; sched
    nil              ; special ai
    nil              ; container
    (list            ;; readied
     t_staff
     )
    )
   kalc-lvl)
   (kalc-mk)))
