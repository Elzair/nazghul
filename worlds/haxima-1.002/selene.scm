;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define selene-lvl 5)
(define selene-species sp_human)
(define selene-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Old Absalot.
;;----------------------------------------------------------------------------
(define selene-bed oa-bed3)
(define selene-mealplace oa-tbl2)
(define selene-workplace oa-baths)
(define selene-leisureplace oa-temple)
(kern-mk-sched 'sch_selene
               (list 0  0 selene-bed          "sleeping")
               (list 7  0 selene-mealplace    "eating")
               (list 8  0 selene-workplace    "working")
               (list 12 0 selene-mealplace    "eating")
               (list 13 0 selene-workplace    "working")
               (list 18 0 selene-mealplace    "eating")
               (list 19 0 selene-leisureplace "idle")
               (list 22 0 selene-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (selene-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Selene is a female cult member of the Accursed.
;; She is unstable, wicked, and depraved.
;;----------------------------------------------------------------------------

;; Basics...
(define (selene-hail knpc kpc)
  (say knpc "[This wild-eyed young woman stares at you impishly]"))

(define (selene-default knpc kpc)
  (say knpc "[She giggles. It is an evil sound]"))

(define (selene-name knpc kpc)
  (say knpc "Selene."))

(define (selene-join knpc kpc)
  (say knpc "[She shakes here head emphatically]"))

(define (selene-job knpc kpc)
  (say knpc "[She shrugs] I play with things."))

(define (selene-bye knpc kpc)
  (say knpc "[She calls out after your back] Be seeing you, hero!"))

(define (selene-play knpc kpc)
  (say knpc "Things... people..."))

(define (selene-peop knpc kpc)
  (say knpc "I like to make them do what I want."))

(define (selene-want knpc kpc)
  (say knpc "Whatever I want. I don't care. I just like to make people do it. "
       "If they don't do it... [she shrugs and giggles]"))

(define (selene-accu knpc kpc)
  (say knpc "It's not so bad, hero. It's kind of fun."))

(define (selene-fun knpc kpc)
  (say knpc "Being Accursed? Sure it is! My favorite part is the sacrifices."))

(define (selene-sacr knpc kpc)
  (say knpc "You'll find out, silly!"))

(define (selene-denn knpc kpc)
  (say knpc "It's easy to make him do what I want. He's afraid of me. "
       "And he wants me."))

(define (selene-sila knpc kpc)
  (say knpc "[She suddenly looks afraid, and becomes vicious] "
       "Why are you bothering me? Go away!")
  (kern-conv-end))

(define selene-conv
  (ifc basic-conv

       ;; basics
       (method 'default selene-default)
       (method 'hail selene-hail)
       (method 'bye  selene-bye)
       (method 'job  selene-job)
       (method 'name selene-name)
       (method 'join selene-join)

       (method 'play selene-play)
       (method 'peop selene-peop)
       (method 'want selene-want)
       (method 'accu selene-accu)
       (method 'fun  selene-fun)
       (method 'sacr selene-sacr)
       (method 'denn selene-denn)
       (method 'sila selene-sila)
       ))

(define (mk-selene)
  (bind 
   (kern-mk-char 
    'ch_selene           ; tag
    "Selene"             ; name
    selene-species         ; species
    selene-occ              ; occ
    s_townswoman     ; sprite
    faction-men      ; starting alignment
    0 2 1            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    selene-lvl
    #f               ; dead
    'selene-conv         ; conv
    sch_selene           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list t_dagger)              ; readied
    )
   (selene-mk)))
