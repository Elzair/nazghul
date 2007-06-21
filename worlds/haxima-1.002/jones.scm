;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define jones-lvl 6)
(define jones-species sp_human)
(define jones-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(define jones-bed ph-bed1)
(define jones-mealplace ph-tbl1)
(define jones-workplace ph-arms)
(define jones-leisureplace ph-hall)
(kern-mk-sched 'sch_jones
               (list 0  0 jones-bed          "sleeping")
               (list 7  0 jones-mealplace    "eating")
               (list 8  0 jones-workplace    "working")
               (list 12 0 jones-mealplace    "eating")
               (list 13 0 jones-workplace    "working")
               (list 18 0 jones-mealplace    "eating")
               (list 19 0 jones-leisureplace "idle")
               (list 22 0 jones-bed          "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;----------------------------------------------------------------------------
(define (jones-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (jones-name knpc kpc)
  (say knpc "Jonesy at yer service."))

(define (jones-job knpc kpc)
  (say knpc "I run the Supply Depot for the Glasdrin militia. Would you like to buy some supplies?")
  (if (yes? kpc)
      (jones-trade knpc kpc)
      (say knpc "If you ever need any I'll be right here.")))

;; Trade...
(define jones-shop-name "Supply Depot")
(define jones-shop-open-time "9:00AM")
(define jones-shop-close-time "6:00PM")
(define (jones-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by the " jones-shop-name "when I'm working "
           "between " jones-shop-open-time " and " jones-shop-close-time)
      (begin
        (kern-conv-trade knpc kpc
                         (list t_arrow        1)
                         (list t_bolt         1)
                         (list t_oil          6)
                         (list t_torch        6)
                         (list t_heal_potion 23)
                         (list t_mana_potion 23)
                         (list t_food        10)
                         ))))

;; Quest-related

(define jones-conv
  (ifc kurpolis-conv

       ;; basics
       (method 'job jones-job)
       (method 'name jones-name)
       
       ;; trade
       (method 'trad jones-trade)
       (method 'buy jones-trade)
       (method 'sell jones-trade)

       ))

(define (mk-jones)
  (bind 
   (kern-mk-char 
    'ch_jones        ; tag
    "Jones"             ; name
    jones-species         ; species
    jones-occ              ; occ
    s_townsman     ; sprite
    faction-men      ; starting alignment
    2 0 0            ; str/int/dex
    0 0              ; hp mod/mult
    0 0              ; mp mod/mult
    max-health ; hp
    -1                   ; xp
    max-health ; mp
    speed-human-med-armor
    jones-lvl
    #f               ; dead
    'jones-conv         ; conv
    sch_jones           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    nil              ; readied
    )
   (jones-mk)))
