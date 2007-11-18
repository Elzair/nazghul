;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define dennis-lvl 3)
(define dennis-species sp_human)
(define dennis-occ oc_wright)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Old Absalot.
;;----------------------------------------------------------------------------
(define dennis-bed oa-bed2)
(define dennis-mealplace oa-tbl1)
(define dennis-workplace oa-slaves)
(define dennis-leisureplace oa-dining-hall)
(kern-mk-sched 'sch_dennis
               (list 0  0 dennis-bed          "sleeping")
               (list 7  0 dennis-mealplace    "eating")
               (list 8  0 dennis-workplace    "working")
               (list 12 0 dennis-mealplace    "eating")
               (list 13 0 dennis-workplace    "working")
               (list 18 0 dennis-mealplace    "eating")
               (list 19 0 dennis-leisureplace "idle")
               (list 22 0 dennis-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (dennis-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Dennis is an acolyte of the Accursed, who lives in Old Absalot.
;; He is somewhat naive, but not yet wholly corrupt.
;;----------------------------------------------------------------------------

;; Basics...
(define (dennis-hail knpc kpc)
  (say knpc "Hello."))

(define (dennis-default knpc kpc)
  (say knpc "I don't know about that sort of thing."))

(define (dennis-name knpc kpc)
  (say knpc "I'm Dennis."))

(define (dennis-join knpc kpc)
  (say knpc "[He scoffs] I don't think so, wayfarer."))

(define (dennis-job knpc kpc)
  (say knpc "I am a student of Master Silas."))

(define (dennis-bye knpc kpc)
  (say knpc "Goodbye."))

;; Tier 2
(define (dennis-stud knpc kpc)
  (say knpc "Master Silas teaches that by focusing my will, and by sacrificing things which impede my progess, I can achieve anything I desire. Or, at least, I will be able to once I have mastered the ways of the Accursed."))

(define (dennis-accu knpc kpc)
  (say knpc "The Accursed are misunderstood. It is not evil to pursue one's desires, it is good! Why can't our enemies see that?"))

(define (dennis-enem knpc kpc)
  (say knpc "The butchers of Glasdrin and that old fool the Enchanter have more blood on their hands than anyone!"))

(define (dennis-ways knpc kpc)
  (say knpc "The ways of the Accursed are revealed to students in phases. At each phase, an acolyte gains more power. To advance to the next phase the student must master the rites and perform a suitable sacrifice."))

(define (dennis-sacr knpc kpc)
  (say knpc "The sacrificial rites are secret. I cannot speak of them with an uninitiate like yourself."))

(define (dennis-powe knpc kpc)
  (say knpc "Power unimaginable awaits those who have the will to grasp it."))

(define (dennis-sila knpc kpc)
  (say knpc "Master Silas is a powerful wizard and a wise teacher."))

(define (dennis-absa knpc kpc)
  (say knpc "Those fools destroyed Absalot, thinking we were there! But they didn't know about Old Absalot, a city beneath the city."))

(define (dennis-old knpc kpc)
  (say knpc "I can't help but feel awed when I walk among these ruins. But they are a bit creepy. The ancients had some strange beliefs!"))

(define (dennis-sele knpc kpc)
  (say knpc "[He blushes] If you know what's good for you, you will stay away from her!")
  (kern-conv-end)
  )

(define dennis-conv
  (ifc basic-conv

       ;; basics
       (method 'default dennis-default)
       (method 'hail dennis-hail)
       (method 'bye dennis-bye)
       (method 'job dennis-job)
       (method 'name dennis-name)
       (method 'join dennis-join)
       
       (method 'sele dennis-sele)
       (method 'stud dennis-stud)
       (method 'teac dennis-stud)
       (method 'accu dennis-accu)
       (method 'enem dennis-enem)
       (method 'ways dennis-ways)
       (method 'sacr dennis-sacr)
       (method 'powe dennis-powe)
       (method 'sila dennis-sila)
       (method 'absa dennis-absa)
       (method 'old dennis-old)
       ))

(define (mk-dennis)
  (bind 
   (kern-mk-char 
    'ch_dennis           ; tag
    "Dennis"             ; name
    dennis-species         ; species
    dennis-occ              ; occ
    s_townsman     ; sprite
    faction-men      ; starting alignment
    0 1 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    dennis-lvl
    #f               ; dead
    'dennis-conv         ; conv
    sch_dennis           ; sched
    'spell-sword-ai              ; special ai
    nil              ; container
    (list t_staff)              ; readied
    )
   (dennis-mk)))
