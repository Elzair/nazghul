;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define doug-bed ke-bed1)
(define doug-mealplace ke-tbl1)
(define doug-workplace ke-hall)
(define doug-leisureplace ke-dine)
(kern-mk-sched 'sch_doug
               (list 0  0 doug-bed          "sleeping")
               (list 11 0 doug-mealplace    "eating")
               (list 12 0 doug-workplace    "working")
               (list 18 0 doug-mealplace    "eating")
               (list 19 0 doug-leisureplace "idle")
               (list 24 0 doug-workplace    "working")               
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (doug-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (doug-hail knpc kpc)
  (say knpc "[You meet a grim paladin] Welcome to hell's doorstep, traveler."))

(define (doug-default knpc kpc)
  (say knpc "I cannot help you with that."))

(define (doug-name knpc kpc)
  (say knpc "I am Captain Douglas, commander of the Kurpolis outpost."))

(define (doug-join knpc kpc)
  (say knpc "And desert my post? I think not, and don't be asking my troopers, or I'll have you expelled back to the surface."))

(define (doug-job knpc kpc)
  (say knpc "I command the outpost here in Kurpolis."))

(define (doug-bye knpc kpc)
  (say knpc "Be careful when traveling the deeps."))

;; Special
(define (doug-outp knpc kpc)
  (say knpc "Glasdrin keeps this outpost here to suppress the monsters of Kurpolis. There's another outpost deeper down called Paladin's Hold."))

(define (doug-mons knpc kpc)
  (say knpc "At this level you find mostly goblins and trolls. Occasionally something worse comes up from the deeps."))

(define (doug-gobl knpc kpc)
  (say knpc "The cave goblins dominate this level. They are savage but ill-equipped. "
       "There is also a grotto where you can find some forest goblins who are usually not not as hostile. "
       "The forest and cave goblins do not get along, which helps keep them under control."))

(define (doug-trol knpc kpc)
  (say knpc "The filthy creatures have a cave beyond the goblin village. "
       "We clean them out from time to time but they just come back. "
       "Sometimes I swear the rocks themselves must be giving birth to trolls."))

(define (doug-wors knpc kpc)
  (say knpc "If you want to see worse, take the ladder down to the next level."))

(define (doug-kurp knpc kpc)
  (say knpc "Kurpolis is a breeding ground for monsters. We can't wipe them out, but we can keep a lid on things."))


(define doug-conv
  (ifc basic-conv

       ;; basics
       (method 'default doug-default)
       (method 'hail doug-hail)
       (method 'bye doug-bye)
       (method 'job doug-job)
       (method 'name doug-name)
       (method 'join doug-join)

       (method 'outp doug-outp)
       (method 'mons doug-mons)
       (method 'gobl doug-gobl)
       (method 'trol doug-trol)
       (method 'wors doug-wors)
       (method 'kurp doug-kurp)
       (method 'hell doug-kurp)
       (method 'door doug-kurp)
       ))

(define (mk-douglas)
  (bind 
   (kern-mk-char 'ch_douglas        ; tag
                 "Douglas"           ; name
                 sp_human            ; species
                 oc_warrior          ; occ
                 s_companion_paladin ; sprite
                 faction-men         ; starting alignment
                 0 0 0               ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 30 0 0 4            ; hp/xp/mp/lvl
                 #f                  ; dead
                 'doug-conv         ; conv
                 sch_doug           ; sched
                 nil                 ; special ai
                 nil                 ; container
                 (list t_armor_chain
                       t_chain_coif
                       t_sword
                       ))         ; readied
   (doug-mk)))
