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
               nil ;sch_gregor     ; sched
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
   t_small_wooden_chest ;; type
   nil ;; trap

   (list
    
    ;; Food
    (list 10 t_mushroom)

    ;; Gold
    (list 100 t_gold_coins)

    ;; Reagents
    (list 23 sulphorous_ash)
    (list 35 ginseng)
    (list 32 garlic)
    (list 20 spider_silk)
    (list 16 blood_moss)
    (list 12 black_pearl)
    (list  3 nightshade)
    (list  2 mandrake)
    
    ;; Items
    (list 5 heal-potion)
    (list 3 cure-poison-potion)
    (list 2 poison-bolt-scroll-type)
    (list 1 death-bolt-scroll-type)
    (list 1 resurrection-scroll-type)
    (list 5 t_torch)
    (list 3 t_picklock)
    
    ;; Arms
    (list 1 t_shield_wooden_buckler)
    (list 1 short-sword)
    (list 1 t_sling)
    (list 5 t_oil)

    ;; Hints/instructions
    (list 1 basic-survival-manual)
    )
   ))


;; This is a chest full of war gear
(define war_chest
  (kern-mk-container
   t_large_wooden_chest ;; type
   nil ;; trap

   (list
    
    ;; Clothing
    (list 6  t_clothing_rags)
    (list 1  t_robe_red)
    (list 1  t_robe_green)
    (list 1  t_robe_blue)

    ;; Armor and Worn Items
    (list 3  t_ranger_cap)
    (list 2  t_leather_helm)
    (list 1  t_iron_helm)
        
    (list 3  t_armor_leather)
    (list 2  t_armor_l_stud)
    (list 1  t_armor_chain)
    (list 1  t_armor_plate)

    ;; Shields
    (list 1 t_shield_wooden_buckler)
    (list 1 t_sm_shield)
    
    ;; Melee Weapons
    (list  6 t_dagger)
    (list  3 short-sword)
    (list  2 t_longsword)
    (list  1 t_2H_sword)

    (list  3 t_mace)
    (list  1 t_mace_and_chain)

    (list  4 t_hatchet)
    (list  1 t_war_axe)

    (list  6 t_spear)
    (list  1 t_halberd)

    (list  8 t_quarterstaff)
    
    ;; Missile Weapons
    (list 36 t_oil)
    
    )
   ))

;; This is a chest full of rare / eldritch items
(define eldritch_chest
  (kern-mk-container
   t_large_iron_chest ;; type
   nil ;; trap

   (list
    
    ;; Rare/unusual/enchanted Armor and Worn Items
    (list  1  t_robe_purple)

    (list  1  t_ancient_helm)
    (list  1  t_silver_crown)
    
    (list  1  t_amulet_green)
    (list  1  t_amulet_red)
    (list  3  t_amulet_blue)
    
    (list  4 t_ring_wooden)
    (list  3  t_ring_silver)
    (list  2  t_ring_golden)
    (list  1  t_ring_opal)
    (list  2  t_ring_ruby)
    (list  2  t_ring_sapphire)
    
    (list  1  t_armor_viridian)
    (list  1  t_armor_opaline)
    (list  1  t_armor_ancient)
    (list  1  t_armor_warlord)
    (list  1  t_armor_golden)
    
    ;; Rare/unusual/enchanted Melee Weapons
    (list  1  t_eldritch_blade)
    (list  1  t_mystic_sword)
    (list  1  t_flaming_sword)

    (list  1  t_nunchaku)
    (list  1  t_bo_staff)
    (list  1  t_bullwhip)
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

   (list war_chest        1 23)
   (list supplies_chest   1 24)
   (list eldritch_chest   1 25)

   (list (mk-cave-entrance 'p_slimy_cavern 8 30) 20 1)
   )

  nil ;; hooks
  (list  ;; edge entrances
   (list north 16 27)
   (list east   0 11)
   (list west  22 10)
   )
) ;; end of place p_moongate_clearing

