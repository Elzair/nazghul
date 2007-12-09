;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define tetzl-lvl 2)
(define tetzl-species sp_spider)
(define tetzl-occ oc_wright)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the monster village of Kun.
;;----------------------------------------------------------------------------
(define tetzl-bed campfire-3)
(define tetzl-mealplace cantina-1)
(define tetzl-workplace cantina-1)
(define tetzl-leisureplace cantina-1)
(kern-mk-sched 'sch_tetzl
               (list 0  0 tetzl-bed          "sleeping")
               (list 7  0 tetzl-mealplace    "eating")
               (list 8  0 tetzl-workplace    "working")
               (list 12 0 tetzl-mealplace    "eating")
               (list 13 0 tetzl-workplace    "working")
               (list 18 0 tetzl-mealplace    "eating")
               (list 19 0 tetzl-leisureplace "idle")
               (list 22 0 tetzl-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (tetzl-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;; 
;; Tetzl is a sentient spider of giant size, 
;; living in the monster village of Kun.
;; 
;; Currently, no dialog or quests, but this seems a missed opportunity...
;;----------------------------------------------------------------------------


(define (mk-tetzl)
  (bind 
   (kern-mk-char 
    'ch_tetzl           ; tag
    "Tetzl"             ; name
    tetzl-species         ; species
    tetzl-occ              ; occ
    s_spider     ; sprite
    faction-men      ; starting alignment
    1 0 1            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
	0
    tetzl-lvl
    #f               ; dead
    nil         ; conv
    sch_tetzl           ; sched
    nil              ; special ai
    nil              ; container
    nil              ; readied
    )
   (tetzl-mk)))
