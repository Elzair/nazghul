;;----------------------------------------------------------------------------
;; Gregor
;;
;; Gregor is one of the first characters the player will meet. He starts out
;; in the moongate clearing in the shrine room.
;;----------------------------------------------------------------------------
(bind 
 (kern-mk-char 'ch_gregor ; tag
               "Gregor"              ; name
               sp_human            ; species
               nil                 ; occ
               s_townsman          ; sprite
               faction-men         ; starting alignment
               0 10 5              ; str/int/dex
               0 0                 ; hp mod/mult
               0 0                 ; mp mod/mult
               max-health -1 max-health 2            ; hp/xp/mp/lvl
               #f                  ; dead
               'gregor-conv        ; conv
               sch_gregor          ; sched
               nil                 ; special ai
               nil                 ; container
               nil                 ; readied
               )
 (gregor-mk #f #f))

;;-----------------------------------------------------------------------------
;; Make some chests containing items to get the player started. 
;; These will be placed on the map in the shrine room in the SW.
;;-----------------------------------------------------------------------------
(define supplies_chest
  (kern-mk-container
   t_chest ;; type
   nil ;; trap

   (list
    
    ;; Food
    (list 10 t_food)

    ;; Gold
    (list 10 t_gold_coins)

    ;; Reagents
    (list 5 sulphorous_ash)
    (list 5 ginseng)
    (list 5 garlic)
    (list 5 spider_silk)
    (list 3 blood_moss)
    (list 3 black_pearl)
    (list 1 nightshade)
    (list 1 mandrake)
    
    ;; Items
    (list 1 t_heal_potion)
    (list 1 t_cure_potion)
    (list 1 t_mana_potion)
    (list 1 t_torch)
    (list 1 t_picklock)
    
    ;; Arms
    (list 1 t_sword)
    (list 1 t_shield)

    ;; Hints/instructions
    (list 1 t_manual)
    (list 1 t_letter_from_enchanter)
    )
   ))



;;----------------------------------------------------------------------------
;; Moongate Clearing
;;
;; This is where the player starts out.
;;----------------------------------------------------------------------------
(kern-mk-place 'p_moongate_clearing "Moongate Clearing"
  s_shrine ;; sprite
  (kern-mk-map 'm_moongate_clearing 23 28 pal_expanded
    (list
		"^3 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^d t3 tt tt tt td ^3 ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^5 "
		"^^ {{ {{ {{ ^^ ^^ ^^ ^c t3 tt tt || || ^b ^^ ^^ {{ {{ {{ ^^ {{ {{ ^^ "
		"^^ {{ {{ {{ {{ ^^ t3 tt tt || tt tt || |% ^a ^^ {C t7 {{ ^^ ^^ {{ ^^ "
		"^^ {{ {{ {{ {{ {C tt || || || || tt || || |% ^e t3 tt {A {{ ^^ {{ ^^ "
		"^^ ^^ {{ {{ {C t3 || || || || tt tt tt || || tt tt tt t5 {A {{ {{ ^^ "
		"^^ ^^ ^^ ^^ t3 tt || || || tt tt tt tt tt tt tt tt tt tt td {{ ^^ ^^ "
		"^^ ^^ ^^ ^^ || || || || tt tt tc bb ta tt tt tt tt tt || ^b ^^ ^^ ^c "
		"^^ ^^ ^^ ^^ || || || tt tt bb .. .. .. bb tt tt tt || || |% ^a ^c |& "
		"^^ ^^ ^^ ^c || || tt tt tt td .. .. .. tb tt tt tt tt || || || || || "
		"^^ ^c |# || || || tt tt bb .. .. .. .. .. bb tt tt tt tt tt tt tt tt "
		"^^ |# || || || || tt tc .. .. .. .. .. .. .. ta tt tt tt tt tt tt tc "
		"{{ |A || || || || tt bb .. .. .. .. .. .. .. bb tt tt tt tt tt tc {& "
		"{{ {% |A || || tt tt t5 .. .. .. .. .. .. .. t3 tt tc {# {% te {# {{ "
		"^^ {{ {{ {{ {% ta tt tt bb .. .. .. .. .. bb tt tc {# {{ {{ {{ {{ {{ "
		"^^ ^^ ^^ {{ {{ {% ta tt tt td .. .. .. tb tt tt {# ^^ ^^ {{ {{ ^^ ^^ "
		"^^ ^^ ^^ ^^ {{ {{ {% ta tt bb .. .. .. bb tt tt {A ^^ ^^ ^^ {{ {{ ^^ "
		"^^ ^^ {7 ^^ ^^ ^^ {{ {% tt td .. .. .. tb tt tt td {{ ^^ ^^ {{ {{ ^^ "
		"^^ {3 .. {1 {5 ^^ {{ {{ tt bb .. .. .. bb tt tt {B {{ {{ {{ {{ ^^ ^^ "
		"^^ {2 .. .. {4 ^^ {{ {{ tt td .. .. .. tb tt tt tt tt t5 {A ^^ ^^ ^^ "
		"^^ {a .. {8 {c ^^ {{ {{ tt bb .. .. .. bb te bb te bb tt t5 ^^ ^^ ^^ "
		"^^ ^^ {e ^^ ^^ ^^ {{ {{ tt td .. .. .. .. .. .. .. .. ta tt ^^ ^^ ^^ "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt bb .. .. .. .. .. .. .. .. bb tt ^^ ^^ ^^ "
		"^^ ^^ ,H ,A ,I ,L ^^ ^^ tt t5 .. .. .. .. .. .. .. .. tb tt ^a ^^ ^^ "
		"^^ {3 .. .. .. .. {5 ^^ ta tc bb .. bb t7 bb .. .. .. bb tt t5 ^a ^^ "
		"^^ {2 .. .. .. .. .. {9 .. .. .. tC t3 tt td .. .. .. tb tt tt t5 ^e "
		"^^ {2 .. .. .. .. {4 ^^ t3 || tt tt tt tt bb .. .. .. bb tt tt tt t5 "
		"^^ ,S ,E ,E ,K ,E ,R ^^ || || || tt tt tc .. .. .. .. .. ta tt tt tt "
		"^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ta || tt tt tc bb .. .. .. .. .. bb ta tt tc "
     )
    )
  #f #f #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil

  ;; *** contents of the place ***
  (list
   (list (kern-tag 'black-gate (mk-moongate nil)) 11 11)
   (list ch_gregor  1 23)
   (list supplies_chest   1 24)
   )

  nil ;; hooks
  (list  ;; edge entrances
   (list north 16 27)
   (list east   0 11)
   (list west  22 10)
   )
) ;; end of place p_moongate_clearing

