;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define alex-lvl 8)
(define alex-species sp_human)
(define alex-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Paladin's Hold, in the Keep guarding Kurpolis.
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
;; 
;; Alex is a Captain in the Glasdrin Militia, 
;; serving in the second garrison which guards 
;; the undead-filled prison level of Kurpolis.
;;----------------------------------------------------------------------------

;; Basics...
(define (alex-hail knpc kpc)
  (say knpc "Welcome, adventurer, and find some measure of safety behind our walls."))

(define (alex-name knpc kpc)
  (say knpc "I am Captain Alex of the Glasdrin militia."))

(define (alex-job knpc kpc)
  (say knpc "I am a warmage, and commander of this garrison. Do you mean to pass through our gates?")
  (if (yes? kpc)
      (alex-pass knpc kpc)
      (say knpc "If you change your mind and need the password come ask me.")))

(define (alex-bye knpc kpc)
  (say knpc "Watch your back out there."))

(define (alex-warm knpc kpc)
  (say knpc "A warmage specializes in combat spells. You know what my biggest problem is?")
  (yes? kpc)
  (say knpc "Finding reagents. Be sure and stock up before traveling any deeper!"))

(define (alex-garr knpc kpc)
  (say knpc "This is one of three garrisons the militia keeps in Kurpolis. "
       "The first garrison guards the entrance to Kurpolis to keep monsters from getting to the surface. ")
  (prompt-for-key)
  (say knpc "This, the second garrison, manages the prison below and keeps the undead at bay.")
  (prompt-for-key)
  (say knpc "The third garrison guards the way to... well, to someplace no one has any business going. ")
  )

(define (alex-unde knpc kpc)
  (say knpc "This level of Kurpolis is an ancient keep, mostly controlled by the undead. Do you want to know what I think?")
  (if (yes? kpc)
      (say knpc "I think the undead are in the service of a lich.")
      (say knpc "Then I won't trouble you with my opinion!")))

(define (alex-lich knpc kpc)
  (say knpc "What is a lich? Why, it's an undead wizard. A lich can command the dead as well as cast all manner of spells. A most troublesome foe."))

(define (alex-pass knpc kpc)
  (say knpc "The password is deep."))

(define (alex-thir knpc kpc)
  (say knpc "We've lost contact with the third garrison. "
       "One of the soldiers from that regiment is in our prison below. "
       "I'm afraid he is quite insane. The deep places can do that to a man."))

(define (alex-pris knpc kpc)
  (say knpc "If you want to visit the prison take the ladder down."))

(define (alex-firs knpc kpc)
  (say knpc "If you want to visit the first garrison take the ladder up, then travel north and then west."))

(define alex-conv
  (ifc kurpolis-conv

       ;; basics
       (method 'hail alex-hail)
       (method 'bye alex-bye)
       (method 'job alex-job)
       (method 'name alex-name)

       (method 'warm alex-warm)
       (method 'garr alex-garr)
       (method 'comm alex-garr)
       (method 'lich alex-lich)
       (method 'pass alex-pass)

       (method 'thir alex-thir)
       (method 'pris alex-pris)
       (method 'firs alex-firs)
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
    2 5 1            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    alex-lvl
    #f               ; dead
    'alex-conv         ; conv
    sch_alex           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list t_sword
    		t_shield
    		t_leather_helm
					         t_armor_leather_2
					         )               ; readied
    )
   (alex-mk)))
