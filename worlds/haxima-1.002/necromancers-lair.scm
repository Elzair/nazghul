;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_necromancers_lair 19 19 pal_expanded
 (list
      "xx xx xx xx xx xx xx xx xx && xx xx xx xx xx x! xx xx xx "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, x! ,, ,, ,, ,, [[ @@ ]] ,, xx ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx xx xx xx xx xx xx xx x! ,, x! xx xx xx x! ,, x! xx xx "
      "xx xx .. .. .. xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx .. .. .. ,, .. xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx .. ,, ,, .. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
      "xx xx .. .. .. xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx xx xx ,, xx xx xx xx x! ,, x! xx xx xx x! ,, x! xx xx "
      "xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx ,, ,, ,, xx xx "
      "xx ,, ,, ,, ,, ,, x! ,, ~~ ee ~~ ~~ xx 00 ,, ,, ,, 00 xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ee ee ~~ ~~ xx 00 ,, ,, ,, 00 xx "
      "xx ,, ,, ,, ,, ,, x! ,, ~~ ~~ ~~ ~~ xx 00 ,, ,, ,, 00 xx "
      "xx x! [[ @@ ]] x! xx xx ~~ ~~ ~~ xx xx x! 00 00 00 x! xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
))

;;----------------------------------------------------------------------------
;; NPC's
;;----------------------------------------------------------------------------
(kern-load "luximene.scm")
(kern-load "necromancer.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_necromancers_lair     ; tag
 "Necromancers Lair" ; name
 nil      ; sprite
 m_necromancers_lair      ; map
 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects

  (put (mk-ladder-up 'p_shard 40 70) 9 9)

  ;; npc's
  (put (mk-necromancer) 9 9)

  ;; bedroom
  (put (mk-bed) 15 3)
  (put (mk-door) 15 6)

  ;; galley
  (put (mk-chest
        nil
        '((5 t_food))) 10 1)

  ;; library
  (put (mk-door) 15 12)

  (put (kern-mk-obj t_spell_book_white_magick_1 1) 13 14)
  (put (kern-mk-obj t_spell_book_white_magick_2 1) 13 15)
  (put (kern-mk-obj t_spell_book_white_magick_3 1) 13 16)

  (put (kern-mk-obj t_spell_book_necromancy        1) 15 17)
  (put (kern-mk-obj t_spell_book_enchantment_wards 1) 16 17)

  (put (kern-mk-obj t_spell_book_summoning   1) 17 14)
  (put (kern-mk-obj t_spell_book_gate_magick 1) 17 15)
  (put (kern-mk-obj t_spell_book_divination  1) 17 16)

  ;; center
  (put (mk-door) 9 12)
  (put (mk-door) 9 6)
  (put (mk-door) 12 9)

  ;; lab
  (put (mk-door) 6 15)
  (put (mk-locked-door) 3 12)

  ;; morgue
  (put (mk-corpse) 2 7)
  (put (mk-corpse) 3 7)
  (put (mk-corpse) 4 7)
  (put (mk-corpse) 1 8)
  (put (mk-corpse) 2 8)
  (put (mk-corpse) 5 8)
  (put (mk-corpse) 1 10)
  (put (mk-corpse) 5 9)

  ;; potion room
  (put (mk-magic-locked-door) 6 3)
  (put (kern-mk-obj mandrake 6) 2 1)
  (put (kern-mk-obj nightshade 4) 3 1)
  (put (kern-mk-obj blood_moss 12) 4 1)
  (put (kern-mk-obj black_pearl 14) 2 5)
  (put (kern-mk-obj spider_silk 21) 3 5)
  (put (kern-mk-obj garlic 18) 4 5)
  (put (kern-mk-obj ginseng 17) 1 4)
  (put (kern-mk-obj sulphorous_ash 26) 1 3)
  (put (kern-mk-obj t_mana_potion 8) 1 2)
  )

 nil ; hooks
 (list  ;; edge entrances
  (list east  0 9)
  (list south 9 0) 
  (list north 9 18)
  (list west  18 9)
  )
 )


(mk-place-music p_necromancers_lair 'ml-creepy-area)

