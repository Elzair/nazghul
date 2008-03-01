(kern-load "gregor.scm")

;;-----------------------------------------------------------------------------
;; Make some chests containing items to get the player started. 
;; These will be placed on the map in the shrine room in the SW.
;;-----------------------------------------------------------------------------
(define supplies_chest
  (mk-chest
   nil ;; trap

   '(
    
    ;; Food
    (10 t_food)

    ;; Gold
    (10 t_gold_coins)

    ;; Reagents
    (10 sulphorous_ash)
    (10 ginseng)
    (10 garlic)
    (10 spider_silk)
    (6 blood_moss)
    (6 black_pearl)
    (3 nightshade)
    (3 mandrake)
    
    ;; Items
    (2 t_heal_potion)
    (2 t_cure_potion)
    (2 t_mana_potion)
    (6 t_torch)
    (3 t_picklock)
    
    ;; Arms
    (1  t_sword)
    (1  t_shield)
    (1  t_staff)

    (1  t_sling)
    (1  t_self_bow)
    (20 t_arrow)

    ;; Hints/instructions
    (1 t_manual)
    (1 t_letter_from_enchanter)
    (1 t_spell_book_white_magick_1   )
    (1 t_spell_book_force_magick_12 )
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
   (list (mk-gregor) 1 23)
   (list supplies_chest   1 24)
   )

  nil ;; hooks
  (list  ;; edge entrances
   (list north 16 27)
   (list east   0 11)
   (list west  22 10)
   (list northeast 8 27)
   (list southeast   9 0)
   (list southwest  22 7)
   )
) ;; end of place p_moongate_clearing

(mk-place-music p_moongate_clearing 'ml-small-town)
