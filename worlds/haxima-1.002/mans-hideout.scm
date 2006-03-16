;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_mans_hideout 19 19 pal_expanded
    (list
      "xx xx xx x! xx xx xx rr rr rr rr rr xx xx xx xx xx xx xx "
      "xx pp cc cc cc pp xx xx xx rr rr rr xx .T .O .O .L .S xx "
      "xx cc ,, ,, ,, cc ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, xx "
      "x! cc ,, ,, ,, cc x! ,, ?? ,, ?? ?? ?? ,, ,, ,, ,, ,, xx "
      "xx cc ,, ,, ,, cc xx xx xx x! ?? xx xx ,, ,, ,, ,, ,, xx "
      "xx pp cc cc cc pp xx xx ,, ,, ,, xx xx xx x! ,, x! xx xx "
      "xx xx xx ,, xx xx x! cc ,, ,, ,, cc x! xx xx ,, xx rr rr "
      "rr rr xx ,, xx xx cc pp cc ,, cc pp cc xx xx ,, xx rr rr "
      "rr rr xx ,, xx ,, ,, cc ,, ,, ,, cc ,, ,, xx ,, xx rr rr "
      "rr rr x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! rr rr "
      "rr rr xx ,, xx ,, ,, cc ,, ,, ,, cc ,, ,, xx ,, xx ~~ ~~ "
      "rr rr xx ,, xx xx cc pp cc ,, cc pp cc xx xx ,, xx ~~ rr "
      "rr xx x! ,, x! xx x! cc ,, ,, ,, cc x! xx ~~ ,, ~~ ~~ rr "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ~~ ee ~~ rr rr "
      "xx ,, ,, ,, ,, ,, rr rr rr rr rr rr rr ~~ ~~ ~~ ~~ rr rr "
      "x! ,, ,, 00 ,, ,, rr rr .. .. .. rr rr ~~ rr rr rr rr rr "
      "xx ,, ,, ,, ,, ,, rr .. .. .. .. .. rr ~~ rr ,, ,, rr rr "
      "xx xx xx && xx xx rr .. .. .. .. ~~ ~~ ~~ rr ,, ,, rr rr "
      "rr rr xx xx xx rr rr rr .. .. .. ~~ rr rr rr rr rr rr rr "
    )
  )

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "the-man.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_mans_hideout ; tag
 "The MAN's Hideout"   ; name
 s_mountains      ; sprite
 m_mans_hideout  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 nil ;; neighbors 
 ;; objects
 (list

  (put (mk-the-man) 9 9)

  ;; kitchen
  (put (mk-chest
        nil
        (mk-contents
         (add-content 10 t_food))) 5 16)

  ;; bedroom
  (put (mk-bed) 3 3)
  (put (mk-chest
        'bomb-trap
        (mk-contents
         (add-content 100 t_gold_coins))) 7 3)

  ;; closet
  (put (mk-door) 6 2)

  ;; secret wall mech
  (put (mk-disg-lvr 'mh-b1 's_wall_rock) 7 18)
  (put (mk-hidden-mech) 7 18)
  (put (kern-tag 'mh-b1
                 (mk-tblitter 'p_mans_hideout 
                              8 14
                              3 1
                              'm_hall_section))
       0
       0)
  
  ;; tool room
  (put (mk-magic-locked-door) 15 5)
  ;; tools...
  (put (mk-chest
        'bomb-trap
        (mk-contents
         (add-content 10 t_gem)
         (add-content 10 t_torch)
         (add-content 10 t_picklock)
         )) 13 2)
  ;; arms...
  (put (mk-chest
        'bomb-trap
        (mk-contents
         (add-content 1 t_sling_4)
         (add-content 1 t_dagger_4)
         (add-content 1 t_leather_helm_4)
         (add-content 1 t_armor_leather_4)
         )) 13 3)
  ;; potions 1...
  (put (mk-chest
        'bomb-trap
        (mk-contents
         (add-content 10 t_cure_potion)
         (add-content 10 t_poison_immunity_potion)
         (add-content 10 t_inv_potion)
         )) 13 4)
  ;; potions 2...
  (put (mk-chest
        'bomb-trap
        (mk-contents
         (add-content 10 t_heal_potion)
         (add-content 10 t_mana_potion)
         )) 17 2)
  ;; scrolls 1...
  (put (mk-chest
        'bomb-trap
        (mk-contents
         (add-content 10 t_in_ex_por_scroll)
         (add-content 10 t_wis_quas_scroll)
         (add-content 10 t_vas_mani_scroll)
         )) 17 3)
  ;; scrolls 2...
  (put (mk-chest
        'bomb-trap
        (mk-contents
         (add-content 10 t_an_tym_scroll)
         (add-content 10 t_sanct_lor_scroll)
         (add-content 10 t_in_quas_xen_scroll)
         (add-content 10 t_an_xen_exe_scroll)
         )) 17 4)

  (put (mk-ladder-down 'p_forsaken_prison 2 16) 9 3)
  )
 nil ; hooks
 nil ; edge entrances
 )
