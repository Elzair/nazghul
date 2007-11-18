;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define anne-lvl 4)
(define anne-species sp_human)
(define anne-occ oc_wizard)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Paladin's Hold, in the Keep guarding Kurpolis.
;;----------------------------------------------------------------------------
(define anne-bed ph-bed2)
(define anne-mealplace ph-tbl2)
(define anne-workplace ph-medik)
(define anne-leisureplace ph-dine)
(kern-mk-sched 'sch_anne
               (list 0  0 anne-bed          "sleeping")
               (list 7  0 anne-mealplace    "eating")
               (list 8  0 anne-workplace    "working")
               (list 12 0 anne-mealplace    "eating")
               (list 13 0 anne-workplace    "working")
               (list 18 0 anne-mealplace    "eating")
               (list 19 0 anne-leisureplace "idle")
               (list 22 0 anne-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (anne-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Anne is a female White Magick mage serving as a medik 
;; in the service of Glasdrin.  
;; She currently serves in the Paladin's Hold of the Keep guarding Kurpolis.
;;----------------------------------------------------------------------------

;; Basics...
(define (anne-name knpc kpc)
  (say knpc "I am called Anne."))

(define (anne-job knpc kpc)
  (say knpc "I am a medik in the service of Glasdrin. Are you in need of healing?")
  (if (yes? kpc)
      (anne-trade knpc kpc)))

(define (anne-trade knpc kpc)
  (if (trade-services knpc kpc
                      (list
                       (svc-mk "Heal" 30 heal-service)
                       (svc-mk "Cure" 30 cure-service)
                       (svc-mk "Resurrect" 100 resurrect-service)))
      (begin
        (say knpc "What else can I do for you?")
        (anne-trade knpc kpc))
      (begin
        (say knpc "Will there be anything else?")
        (if (kern-conv-get-yes-no? kpc)
            (anne-trade knpc kpc)
            (say knpc "Very well.")))))

(define (anne-medik knpc kpc)
  (say knpc "I heal paladins who are wounded in combat. I will heal others, too, for a price."))

(define (anne-kurp knpc kpc)
  (say knpc "This is a savage place, not for the inexperienced. It gets worse the deeper you go."))

(define anne-conv
  (ifc kurpolis-conv

       ;; basics
       (method 'job anne-job)
       (method 'name anne-name)
       
       ;; trade
       (method 'trad anne-trade)
       (method 'heal anne-trade)
       (method 'pric anne-trade)

       (method 'medik anne-medik)
       (method 'kurp anne-kurp)
       ))

(define (mk-anne)
  (bind 
   (kern-mk-char 
    'ch_anne           ; tag
    "Anne"             ; name
    anne-species         ; species
    anne-occ              ; occ
    s_companion_wizard     ; sprite
    faction-men      ; starting alignment
    1 3 2            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    0
    anne-lvl
    #f               ; dead
    'anne-conv         ; conv
    sch_anne           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list t_sword
					         t_armor_leather
					         )               ; readied
    )
   (anne-mk)))
