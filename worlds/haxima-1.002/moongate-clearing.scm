;;----------------------------------------------------------------------------
;; Gregor
;;
;; Gregor is one of the first characters the player will meet. He starts out
;; in the moongate clearing in the shrine room.
;;----------------------------------------------------------------------------
(kern-load "gregor.scm")
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
               30 0 9 9            ; hp/xp/mp/lvl
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
    (list 1 t_dagger)

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
     "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt tt tt tt tt ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
     "^^ {{ {{ {{ ^^ ^^ ^^ ^^ tt tt tt || || ^^ ^^ ^^ {{ {{ {{ ^^ {{ {{ ^^ "
     "^^ {{ {{ {{ {{ ^^ tt tt tt || tt tt || || ^^ ^^ {{ tt {{ ^^ ^^ {{ ^^ "
     "^^ {{ {{ {{ {{ {{ tt || || || || tt || || || ^^ tt tt {{ {{ ^^ {{ ^^ "
     "^^ ^^ {{ {{ {{ tt || || || || tt tt tt || || tt tt tt tt {{ {{ {{ ^^ "
     "^^ ^^ ^^ ^^ tt tt || || || tt tt tt tt tt tt tt tt tt tt tt {{ ^^ ^^ "
     "^^ ^^ ^^ ^^ || || || || tt tt tt bb tt tt tt tt tt tt || ^^ ^^ ^^ ^^ "
     "^^ ^^ ^^ ^^ || || || tt tt bb .. .. .. bb tt tt tt || || || ^^ ^^ || "
     "^^ ^^ ^^ ^^ || || tt tt tt tt .. .. .. tt tt tt tt tt || || || || || "
     "^^ ^^ || || || || tt tt bb .. .. .. .. .. bb tt tt tt tt tt tt tt tt "
     "^^ || || || || || tt tt .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt "
     "{{ || || || || || tt bb .. .. .. .. .. .. .. bb tt tt tt tt tt tt {{ "
     "{{ {{ || || || tt tt tt .. .. .. .. .. .. .. tt tt tt {{ {{ tt {{ {{ "
     "^^ {{ {{ {{ {{ tt tt tt bb .. .. .. .. .. bb tt tt {{ {{ {{ {{ {{ {{ "
     "^^ ^^ ^^ {{ {{ {{ tt tt tt tt .. .. .. tt tt tt {{ ^^ ^^ {{ {{ ^^ ^^ "
     "^^ ^^ ^^ ^^ {{ {{ {{ tt tt bb .. .. .. bb tt tt {{ ^^ ^^ ^^ {{ {{ ^^ "
     "^^ ^^ .. ^^ ^^ ^^ {{ {{ tt tt .. .. .. tt tt tt tt {{ ^^ ^^ {{ {{ ^^ "
     "^^ .. .. .. .. ^^ {{ {{ tt bb .. .. .. bb tt tt {{ {{ {{ {{ {{ ^^ ^^ "
     "^^ .. .. .. .. ^^ {{ {{ tt tt .. .. .. tt tt tt tt tt tt {{ ^^ ^^ ^^ "
     "^^ .. .. .. .. ^^ {{ {{ tt bb .. .. .. bb tt bb tt bb tt tt ^^ ^^ ^^ "
     "^^ ^^ .. ^^ ^^ ^^ {{ {{ tt tt .. .. .. .. .. .. .. .. tt tt ^^ ^^ ^^ "
     "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ tt bb .. .. .. .. .. .. .. .. bb tt ^^ ^^ ^^ "
     "^^ ^^ ,H ,A ,I ,L ^^ ^^ tt tt .. .. .. .. .. .. .. .. tt tt ^^ ^^ ^^ "
     "^^ .. .. .. .. .. .. ^^ tt tt bb .. bb tt bb .. .. .. bb tt tt ^^ ^^ "
     "^^ .. .. .. .. .. .. .. .. .. .. .. tt tt tt .. .. .. tt tt tt tt ^^ "
     "^^ .. .. .. .. .. .. ^^ tt || tt tt tt tt bb .. .. .. bb tt tt tt tt "
     "^^ ,S ,E ,E ,K ,E ,R ^^ || || || tt tt tt .. .. .. .. .. tt tt tt tt "
     "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ tt || tt tt tt bb .. .. .. .. .. bb tt tt tt "
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
   (list (mk-dungeon 'p_slimy_cavern 8 30) 20 1)
   )

  nil ;; hooks
  (list  ;; edge entrances
   (list north 16 27)
   (list east   0 11)
   (list west  22 10)
   )
) ;; end of place p_moongate_clearing

