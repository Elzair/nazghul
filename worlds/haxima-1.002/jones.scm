;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define jones-lvl 6)
(define jones-species sp_human)
(define jones-occ nil)

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In the Keep guarding Kurpolis.
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
;; 
;; Jones runs the supply depot for the Glasdrin militia.
;; He is stationed in the Keep guarding Kurpolis.
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
(define jones-merch-msgs
  (list "Come by the Supply Depot between 9:00AM and 6:00PM."
        "I keep the basics in stock."
        nil
        nil
        "Are you sure you have enough? You better get some more."
        "I hope you've got enough, for your sake."
   ))

(define jones-catalog
  (list
   (list t_arrow        1 "Down here you'll go through arrows like a troll goes through grog!")
   (list t_bolt         1 "You can run out of bolts in the blink of a gazer's eye!")
   (list t_oil          6 "Yes, stock up on plenty of that flaming oil. It gets worse deeper down.")
   (list t_torch        6 "You don't want to run out of torches down here!")
   (list t_heal_potion 23 "Yes, they are spendy, but healing potions are hard to get and critical to have.")
   (list t_mana_potion 23 "You better buy extra mana potions. Your mages will be working overtime.")
   (list t_food        10 "There's nothing worse than runnning out of food when you're lost in the lower levels.")
   ))

(define (jones-trade knpc kpc) (conv-trade knpc kpc "buy" jones-merch-msgs jones-catalog))

;; Quest-related

(define jones-conv
  (ifc kurpolis-conv

       ;; basics
       (method 'job jones-job)
       (method 'name jones-name)
       
       ;; trade
       (method 'trad jones-trade)
       (method 'buy jones-trade)

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
    0
    jones-lvl
    #f               ; dead
    'jones-conv         ; conv
    sch_jones           ; sched
    'townsman-ai              ; special ai
    nil              ; container
    (list t_axe
    		t_armor_chain)              ; readied
    )
   (jones-mk)))
