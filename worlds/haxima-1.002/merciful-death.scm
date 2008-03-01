(kern-mk-map
 'm_merciful_death 19 19 pal_expanded
	(list
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. ~C ~3 ~1 ~1 ~1 ~5 ~A .. .. .. ~C ~3 ~1 ~1 ~1 ~5 ~A .. "
		"~~ ~~ ~~ bb -- -- ~~ ~~ ~1 ee ~1 ~~ ~~ -- -- -- ~~ ~5 ~A "
		"-- -- -- ~~ -- -- -- -- -- ee -- -- -- -- -- bb -- ~~ ~~ "
		"-- #e #a #a #a #a #a #a #a ee #a #a #f -- -- ~~ -- -- -- "
		"-- #b ee ee ee ee ee ee ee ee ee ee #F #a #a #f -- -- -- "
		"-- #b ee ee ee ee ee ee ee ee ee ee ee ee ee #F #f -- -- "
		"-- #b ee ee ee ee ee ee ee ee ee ee ee ee ee ee #F #f -- "
		"-- #b ee ee oo ee ee ee ee oo ee ee ee WW ee ee ee #c -- "
		"-- #b ee ee ee ee ee ee ee ee ee ee ee ee ee ee #H #h -- "
		"-- #b ee ee ee ee ee ee ee ee ee ee ee ee ~~ ~~ ~~ ~~ -- "
		"-- #b ee ~~ ~~ ~~ ee ee ~~ ~~ ee ee #H #d #d ~~ ~~ ~~ -- "
		"-- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ bb -- "
		"-- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ bb ~~ ~~ ~~ ~~ -- -- bb ~~ -- "
		"-- -- bb -- -- -- -- -- -- ~~ -- -- -- -- -- -- ~~ -- -- "
		"-- -- ~~ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
		"-- -- -- -- __ __ __ __ -- -- -- -- __ __ __ -- -- -- -- "
		"-- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- "
	)
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_merciful_death ; tag
 "Wreck of the Merciful Death (abovedecks)"   ; name
 s_ship              ; sprite
 m_merciful_death  ; map
 #f               ; wraps
 #f                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (mk-monman) 0 0)
  (put (mk-ladder-down 'p_merciful_death_l2 6 9) 6 9)
  (put (spawn-pt 'skeletal-warrior) 7 9)
  (put (spawn-pt 'skeletal-spear-thrower) 10 9)
  )

 (list 'on-entry-to-dungeon-room) ; hooks
 nil ; edge entrances
 )

(mk-place-music p_merciful_death 'ml-dungeon-adventure)

;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_merciful_death_l2 19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr -- -- -- -- -- rr rr rr rr rr -- -- -- -- -- rr rr "
      "-- -- -- bb -- -- -- -- -- -- -- -- -- -- -- -- -- -- rr "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- bb -- -- -- "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
      "-- -- #I #r #r #r #r #r #r #r #r #J -- -- -- -- -- -- -- "
      "-- -- #s ee ee ee ee ee ee ee ee #K #r #r #J -- -- -- -- "
      "-- -- #s ee ee ee ee ee ee ee ee ee ee ee #K #J -- -- -- "
      "-- -- #s ee ee ee ee ee ee ee ee ee ee ee ee #s -- -- -- "
      "-- -- #s ee ee ee ee ee ee ee ee ee ee ee #I #L -- -- -- "
      "-- -- #s ee ee ee ee ee ee ee ee #I #r #r #L -- -- -- -- "
      "-- -- #K #r #r #r #r #r #r #r #r #L -- -- -- -- -- -- -- "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- bb -- "
      "-- -- -- -- -- -- -- -- -- bb -- -- -- -- -- -- bb -- -- "
      "-- -- bb -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
      "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "
      "-- -- -- -- __ __ __ __ -- -- -- -- __ __ __ -- -- -- -- "
      "-- -- __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ -- -- "
    )
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_merciful_death_l2 ; tag
 "Wreck of the Merciful Death (belowdecks)"   ; name
 nil              ; sprite
 m_merciful_death_l2  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (mk-monman) 0 0)
  (put (mk-ladder-up 'p_merciful_death 6 9) 6 9)
  (put (mk-chest
        'poison-trap
        '((1   t_rune_c)
                     (342 t_gold_coins)
                     (4   t_gem)
                     (1 t_sextant)
                     ))
       14 9)
  (put (spawn-pt 'skeletal-warrior) 7 9)
  (put (spawn-pt 'skeletal-spear-thrower) 10 9)
  )

 (list 'on-entry-to-dungeon-room) ; hooks
 nil ; edge entrances
 )

(mk-place-music p_merciful_death 'ml-dungeon-adventure)
