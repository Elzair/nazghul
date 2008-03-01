
;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_prison 32 9 pal_expanded
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn rn "
  "xx ,, ,, ,, xx xx xx xx xx ,, xx ,, xx rn xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx rn rn rn rn rn "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx ee xx xx ,, xx xx xx ,, xx xx xx ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx ,, ,, ,, x! ,, ,, ,, ee ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx ee xx xx ,, xx xx xx ,, xx xx xx ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx rn xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx xx xx xx xx xx "
  "xx ,, ,, ,, xx rn xx xx xx ,, xx ,, xx rn xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx rn rn rn rn rn "
  "xx xx xx xx xx rn xx xx xx xx xx xx xx rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn rn "
  ))

;;----------------------------------------------------------------------------
;; NPC's
;;----------------------------------------------------------------------------
(kern-load "gholet.scm")
(kern-load "valus.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_prison     ; tag
 "Prison"      ; name
 s_hamlet      ; sprite
 m_prison      ; map
 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects

  (put (mk-monman) 0 0)

  (put (mk-ladder-up 'p_glasdrin 2 2) 6 4)
  
  ;; store room
  (put (mk-locked-door) 6 2)
  (put (mk-chest
        'burn-trap
        '(( 10 t_food)
          )) 1 1)
  (put (mk-chest
        'burn-trap
        '((1 t_cure_potion)
          (1 t_heal_potion)
          )) 2 1)
  (put (mk-chest
        'burn-trap
        '((10 t_arrow)
          )) 1 2)
  (put (mk-chest
        'burn-trap
        '((10 t_bolt)
          )) 2 7)
  (put (mk-chest
        'burn-trap
        '((1 t_vas_mani_scroll)
          )) 3 7)
  (put (mk-chest
        'burn-trap
        '((10 sulphorous_ash)
          (5 garlic)
          (5 ginseng)
          )) 3 6)

  ;; rats in store room
  (put (spawn-pt 'rat) 1 1)
  (put (spawn-pt 'rat) 1 3)
  (put (spawn-pt 'rat) 2 4)
  (put (spawn-pt 'rat) 3 7)
  (put (spawn-pt 'rat) 1 6)
  (put (spawn-pt 'rat) 4 2)

  ;; portcullises
  (put (kern-tag 'pp1 (mk-portcullis)) 12 4)
  (put (kern-tag 'pp2 (mk-portcullis)) 14 4)

  ;; energy fields for zorn
  (put (kern-mk-obj F_energy_perm 1) 26 4)
  (put (kern-mk-obj F_energy_perm 1) 28 3)
  (put (kern-mk-obj F_energy_perm 1) 28 4)
  (put (kern-mk-obj F_energy_perm 1) 28 5)
  (put (kern-mk-obj F_energy_perm 1) 29 3)
  (put (kern-mk-obj F_energy_perm 1) 29 5)
  (put (kern-mk-obj F_energy_perm 1) 30 3)
  (put (kern-mk-obj F_energy_perm 1) 30 4)
  (put (kern-mk-obj F_energy_perm 1) 30 5)

  ;; cell doors
  (put (mk-locked-windowed-door) 16 3)
  (put (mk-locked-windowed-door) 20 3)
  (put (mk-locked-windowed-door) 24 3)
  (put (mk-locked-windowed-door) 16 5)
  (put (mk-locked-windowed-door) 20 5)
  (put (mk-locked-windowed-door) 24 5)
  (put (mk-locked-door)  27 4)

  ;; levers
  (put (mk-lever 'pp1) 11 1)
  (put (mk-lever 'pp2) 11 7)

  ;; prisoners
  (put (mk-gholet) 24 1)
  (put (mk-npc 'zorn 8) 29 4)
  (put (mk-npc 'footpad 8) 16 7)
  (put (mk-valus) 24 6)

  ;; guards
  (put (guard-pt 'halberdier) 9 1)
  (put (guard-pt 'halberdier) 9 7)

  )

 (list 'on-entry-to-dungeon-room) ; on-entry hook(s)
 nil ; edge entrances
 )

(mk-place-music p_prison 'ml-dungeon-town)
