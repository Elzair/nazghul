;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define alex-lvl 8)
(define alex-species sp_human)
(define alex-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define alex-bed ph-bed3)
(define alex-mealplace ph-tbl3)
(define alex-workplace ph-hall)
(define alex-leisureplace ph-dine)
(kern-mk-sched 'sch_alex
               (list 0  0 alex-bed          "sleeping")
               (list 7  0 alex-mealplace    "eating")
               (list 8  0 alex-workplace    "working")
               (list 12 0 alex-mealplace    "eating")
               (list 13 0 alex-workplace    "working")
               (list 18 0 alex-mealplace    "eating")
               (list 19 0 alex-leisureplace "idle")
               (list 22 0 alex-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (alex-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (alex-hail knpc kpc)
  (say knpc "Welcome, adventurer, and find some measure of safety behind our walls."))

(define (alex-name knpc kpc)
  (say knpc "I am Captain Alex of the Glasdrin militia."))

(define (alex-job knpc kpc)
  (say knpc "I am a warmage, and commander of this garrison. Do you mean to pass through our gates?")
  (if (yes? knpc)
      (alex-pass knpc kpc)
      (say knpc "If you change your mind and need the password come ask me.")))

(define (alex-bye knpc kpc)
  (say knpc "Watch your back out there."))

(define (alex-warm knpc kpc)
  (say knpc "A warmage specializes in combat spells. You know what my biggest problem is?")
  (yes? kpc)
  (say knpc "Finding reagents. Be sure and stock up before traveling any deeper!"))

(define (alex-garr knpc kpc)
  (say knpc "This garrison is on the front line in our campaign to pacify Kurpolis. Fighting is fierce at this level."))

(define (alex-leve knpc kpc)
  (say knpc "This level of Kurpolis is an ancient keep, mostly controlled by the undead. Do you want to know what I think?")
  (if (yes? kpc)
      (say knpc "I think the undead are in the service of a lich.")
      (say knpc "Then I won't trouble you with my opinion!")))

(define (alex-lich knpc kpc)
  (say knpc "What is a lich? Why, it's an undead wizard. A lich can command the dead as well as cast all manner of spells. A most troublesome foe."))

(define (alex-pass knpc kpc)
  (say knpc "The password is deep."))

(define alex-conv
  (ifc basic-conv

       ;; basics
       (method 'hail alex-hail)
       (method 'bye alex-bye)
       (method 'job alex-job)
       (method 'name alex-name)

       (method 'warm alex-warm)
       (method 'garr alex-garr)
       (method 'comm alex-garr)
       (method 'leve alex-leve)
       (method 'fier alex-leve)
       (method 'lich alex-lich)
       (method 'pass alex-pass)
       ))

(define (mk-alex)
  (bind 
   (kern-mk-char 
    'ch_alex           ; tag
    "Alex"             ; name
    alex-species         ; species
    alex-occ              ; occ
    s_companion_wizard     ; sprite
    faction-men      ; starting alignment
    0 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    (max-hp alex-species alex-occ alex-lvl 0 0) ; hp
    0                   ; xp
    (max-mp alex-species alex-occ alex-lvl 0 0) ; mp
    alex-lvl
    #f               ; dead
    'alex-conv         ; conv
    sch_alex           ; sched
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   (alex-mk)))
