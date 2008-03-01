;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------

(kern-mk-map
 'm_old_absalot 31 31 pal_expanded
 (list
		"rn rn rn rn rn rn rn rn rn xx xx xx xx xx xx rn rn rn rn rn rn r8 r8 r8 rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn xx xx ,T ,E ,M ,P ,L ,E xx xx rn rn rr ,, ,, ,, r2 rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rr xx !! cc cc cc cc cc cc !! xx xx rn rr ,, ,, ,, r2 rn rn rn rn rn rn "
		"rn rn rn rn rn rn rr rr !! !! cc aa cc cc aa cc !! !! xx xx rr ,, ,, ,, r2 rn rn rn rn rn rn "
		"rn rn rn rn r8 rn xx !! !! !! cc cc cc cc cc cc !! !! !! xx rr ,, ,, ,, r2 rn rn rn rn rn rn "
		"rn rn rn r4 ,, r2 xx ,, ,, pp ,, ,, ,, ,, ,, ,, pp ,, ,, xx rr ,, ,, ,, r2 rn rn rn rn rn rn "
		"rn rn rn rc ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr ,, rr rr rr rn rn rn rn rn rn "
		"rn rn r4 ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr ,, rr ,M ,E ,A ,T rn rn rn rn "
		"rn rn r4 ,, ,, ,, ,, ,, ,, pp ,, ,, ,, ,, ,, ,, pp ,, ,, ,, ,, ,, rr ,, ,, ,, ,, r2 rn rn rn "
		"rn rn xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr ,, ,, ,, ,, r2 rn rn rn "
		"xx xx xx r1 xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn r4 ,, ,, ,, ,, ,, ,, r2 rn rn rn "
		"rn ,S ,L ,A ,V ,E ,S xx rr xx ,, pp ,, ,, pp ,, r3 xx xx rn r4 ,, xx xx xx xx xx xx xx xx xx "
		"xx ,, ,, ,, ,, ,, ,, xx rn xx ,, ,, ,, ,, ,, ,, xx rn rn rn r4 ,, xx ,F ,L ,E ,S ,H ,L ,Y xx "
		"xx ,, ,, ,, ,, ,, ,, xx rn xx ,, ,, ,, ,, ,, ,, xx rn rn rn r4 ,, re ,, ,, ,, ,, ,, ,, ,, xx "
		"xx ,, ,, ,, ,, ,, ,, xx rn xx ,, pp ,, ,, pp ,, xx rn rn rn r4 ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
		"xx xx rd ,, ,, xx xx xx rn xx ,, ,, ,, ,, ,, ,, xx rn rn rn rn r1 xx ,, ,, ,, aa ,, ,, ,, xx "
		"xx ,, ,, ,, ,, ,, ,, rr rn xx xx xx ,, ,, xx xx xx rn rn rn rn rn xx ,, ,, ,, ,, ,, ,, ,, xx "
		"xx xx xx ,, ,, xx rr rr rn rn rn xx ,, ,, rr rn rn rn rn xx xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
		"xx ,, ,, ,, ,, ,, ,, xx rn rn rn xx ,, ,, xx rn rn rn xx ,, ,, ,, xx ,, ,, [[ @@ ]] ,, ,, xx "
		"xx xx xx ,, ,, xx xx xx xx xx xx rr ,, ,, xx xx rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx ,F ,E ,A ,S ,T xx xx "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr xx xx xx xx rr rr xx "
		"xx xx xx ,, xx xx xx xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx xx xx xx rr cc cc cc cc cc r2 "
		"xx xx xx ,, xx xx xx xx rn xx ,, ,, ,, xx xx ,, ,, ,, xx ,B ,L ,O ,O ,D xx cc cc ~~ ~~ cc r2 "
		"xx ,, xx ,, xx xx xx xx rr xx ,, ,, ,, rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, cc ~~ ~~ ~~ cc r2 "
		"xx xx xx ,, xx xx xx xx rr ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc ~~ aa cc cc r2 "
		"xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, cc ~~ ~~ ~~ cc xx "
		"xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx rr rr xx ,B ,A ,T ,H ,S xx cc cc ~~ ~~ cc xx "
		"xx ,, ,, ,, ,, ,, xx xx xx xx ,, ,, ,, xx rr rn rn rn xx xx xx xx xx xx xx cc cc cc cc cc xx "
		"xx xx xx r1 r1 r1 xx xx rn rn xx xx xx xx rn rn rn rn rn rn rn rn rn rn rr xx xx xx xx xx xx "
))

;;----------------------------------------------------------------------------
;; NPC's
;;----------------------------------------------------------------------------
(kern-load "silas.scm")
(kern-load "dennis.scm")
(kern-load "selene.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_old_absalot     ; tag
 "Old Absalot"      ; name
 nil          ; sprite
 m_old_absalot      ; map
 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects
  (put (mk-ladder-up 'p_absalot 2 2) 11 27)

  ;; npc's
  (put (mk-silas) 9 9)
  (put (mk-dennis) 9 9)
  (put (mk-selene) 9 9)

  ;; slave prison
  (put (kern-tag 'oa-cp1 (mk-portcullis)) 2 16)
  (put (kern-tag 'oa-cp2 (mk-portcullis)) 2 18)
  (put (kern-tag 'oa-cp3 (mk-portcullis)) 5 16)
  (put (kern-tag 'oa-cp4 (mk-portcullis)) 5 18)
  (put (mk-corpse) 1 16)
  (put (kern-tag 'oa-ps1 (mk-portcullis)) 3 19)
  (put (kern-tag 'oa-ps2
                 (mk-connected-portcullis 'oa-ps1)) 4 19)
  (put (mk-lever 'oa-ps2) 2 20)
  (put (mk-lever 'oa-cp1) 1 14)
  (put (mk-lever 'oa-cp2) 2 14)
  (put (mk-lever 'oa-cp3) 5 14)
  (put (mk-lever 'oa-cp4) 6 14)

  ;; treasure room
  (put (mk-magic-locked-door) 3 23)
  (put (kern-mk-obj F_poison_perm 1) 3 24)
  (put (kern-mk-obj F_sleep_perm 1) 3 25)
  (put (kern-mk-obj F_energy_perm 1) 3 26)
  (put (kern-mk-obj F_fire_perm 1) 3 27)
  (put (kern-mk-obj F_poison_perm 1) 3 28)
  (put (kern-mk-obj F_poison_perm 1) 2 27)
  (put (kern-mk-obj F_poison_perm 1) 4 27)
  (put (kern-mk-obj F_sleep_perm 1) 1 27)
  (put (kern-mk-obj F_sleep_perm 1) 2 28)
  (put (kern-mk-obj F_sleep_perm 1) 3 29)
  (put (kern-mk-obj F_sleep_perm 1) 4 28)
  (put (kern-mk-obj F_sleep_perm 1) 5 27)
  (put (kern-mk-obj F_energy_perm 1) 1 28)
  (put (kern-mk-obj F_energy_perm 1) 2 29)
  (put (kern-mk-obj F_energy_perm 1) 4 29)
  (put (kern-mk-obj F_energy_perm 1) 5 28)
  (put (kern-mk-obj F_fire_perm 1) 1 29)
  (put (kern-mk-obj F_fire_perm 1) 5 29)
  
  (put (mk-hidden 't_rune_s 1) 4 29)

  ;; meat locker
  (put (mk-locked-door) 22 14)
  (put (mk-corpse) 26 8)
  (put (mk-corpse) 26 9)
  (put (mk-corpse) 26 10)
  (put (mk-corpse) 25 8)
  (put (mk-corpse) 25 9)
  (put (mk-corpse) 24 8)

  ;; Silas's room
  (put (mk-bed) 4 8)
  (put (mk-locked-door) 6 8)
  (put (mk-magic-locked-door) 4 6)
  (put (mk-chest 'bomb-trap
                 '((1 t_demon_gate_book)
		   (1 t_spell_book_gate_magick)
		   (1 t_spell_book_summoning)
		   (1 t_spell_book_enchantment_curses)
		   ))
       4 5)

  ;; Bunkroom
  (put (mk-door) 19 8)
  (put (mk-door) 21 6)
  (put (mk-bed) 21 1)
  (put (mk-bed) 23 1)
  (put (mk-bed) 23 3)
  (put (mk-bed) 23 5)

  )

 nil ; hooks
 nil ; edge entrances
 )

(mk-place-music p_old_absalot 'ml-dungeon-town)
