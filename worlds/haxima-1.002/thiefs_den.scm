;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_thiefs_den 19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, pp ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, ,, pp ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "mouse.scm")
(mk-mouse)

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------
;(kern-load "thiefs_den_mechs.scm")

;;----------------------------------------------------------------------------
;; Other dungeon rooms
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_thiefs_den ; tag
 "Thief's Den"   ; name
 nil              ; sprite
 m_thiefs_den  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 nil ;; neighbors 
 ;; objects
 (list

  (put ch_mouse 3 3)
  (put (mk-ladder-up 'p_traps_4 9 7) 9 9)

  ;; foods
  (put (kern-mk-obj t_food 10) 4 1)
  (put (kern-mk-obj t_wine 1) 4 2)

  ;; potions
  (put (kern-mk-obj t_cure_potion 1) 3 4)
  (put (kern-mk-obj t_mana_potion 1) 4 5)
  (put (kern-mk-obj t_heal_potion 1) 5 5)
  (put (kern-mk-obj t_poison_immunity_potion 1) 6 5)
  (put (kern-mk-obj t_inv_potion 1) 7 5)

  ;; tools
  (put (kern-mk-obj t_torch 1) 8 5)
  (put (kern-mk-obj t_picklock 1) 8 6)
  (put (kern-mk-obj t_gem      1) 8 7)

  ;; scrolls
  (put (kern-mk-obj t_an_tym_scroll 1) 9 7)
  (put (kern-mk-obj t_in_mani_corp_scroll 1) 10 7)
  (put (kern-mk-obj t_vas_rel_por_scroll 1) 10 8)
  (put (kern-mk-obj t_xen_corp_scroll 1) 10 9)
  (put (kern-mk-obj t_sanct_lor_scroll 1) 10 10)
  (put (kern-mk-obj t_in_quas_xen_scroll 1) 9 10)
  (put (kern-mk-obj t_in_vas_por_ylem_scroll 1) 8 10)
  (put (kern-mk-obj t_an_xen_exe_scroll 1) 8 9)
  (put (kern-mk-obj t_in_an_scroll 1) 7 9)
  (put (kern-mk-obj t_in_ex_por_scroll 1) 6 9)
  (put (kern-mk-obj t_vas_mani_scroll 1) 6 8)
  (put (kern-mk-obj t_wis_quas_scroll 1) 6 7)

  ;; weapons
  (put (kern-mk-obj t_dagger 1) 5 7)
  (put (kern-mk-obj t_mace 1) 4 7)
  (put (kern-mk-obj t_sword 1) 3 7)
  (put (kern-mk-obj t_2H_axe 1) 2 7)
  (put (kern-mk-obj t_2H_sword 1) 2 6)
  (put (kern-mk-obj t_morning_star 1) 1 6)
  (put (kern-mk-obj t_halberd 1) 1 5)
  (put (kern-mk-obj t_staff 1) 1 4)

  ;; armor
  (put (kern-mk-obj t_leather_helm 1) 1 7)
  (put (kern-mk-obj t_chain_coif 1) 5 8)
  (put (kern-mk-obj t_iron_helm 1) 9 6)
  (put (kern-mk-obj t_armor_leather 1) 5 4)
  (put (kern-mk-obj t_armor_chain 1) 5 3)
  (put (kern-mk-obj t_armor_plate 1) 5 1)
  (put (kern-mk-obj t_spiked_helm 1) 6 1)
  (put (kern-mk-obj t_spiked_shield 1) 1 8)

  ;; lucre
  (put (kern-mk-obj t_gold_coins 74) 6 2)
  (put (kern-mk-obj t_gold_coins 112) 2 9)
  (put (kern-mk-obj t_gold_coins 243) 1 17)
  (put (kern-mk-obj t_gold_coins 30) 7 3)
  
  ;; door
  (put (mk-bed) 3 1)
  
  )
 nil ; hooks
 nil ; edge entrances
 )

