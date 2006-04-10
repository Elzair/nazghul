(kern-mk-map
 'm_merciful_death 19 19 pal_expanded
	(list
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. ~C ~3 ~1 ~1 ~1 ~5 ~A .. .. .. ~C ~3 ~1 ~1 ~1 ~5 ~A .. "
		"~~ ~~ ~~ bb -- -- ~~ ~~ ~1 ee ~1 ~~ ~~ -- -- -- ~~ ~5 ~A "
		"-- -- -- ~~ -- -- -- -- -- ee -- -- -- -- -- bb -- ~~ ~~ "
		"-- ## ## ## ## ## ## ## ## ee ## ## ## -- -- ~~ -- -- -- "
		"-- ## ee ee ee ee ee ee ee ee ee ee ## ## ## ## -- -- -- "
		"-- ## ee ee ee ee ee ee ee ee ee ee ee ee ee ## ## -- -- "
		"-- ## ee ee ee ee ee ee ee ee ee ee ee ee ee ee ## ## -- "
		"-- ## ee ee oo ee ee ee ee oo ee ee ee WW ee ee ee ## -- "
		"-- ## ee ee ee ee ee ee ee ee ee ee ee ee ee ee ## ## -- "
		"-- ## ee ee ee ee ee ee ee ee ee ee ee ee ~~ ~~ ~~ ~~ -- "
		"-- ## ee ~~ ~~ ~~ ee ee ~~ ~~ ee ee ## ## ## ~~ ~~ ~~ -- "
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
      "-- -- #> #> #> #> #> #> #> #> #> #> -- -- -- -- -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee #> #> #> #> -- -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee ee ee ee #> #> -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee ee ee ee ee #> -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee ee ee ee #> #> -- -- -- "
      "-- -- #> ee ee ee ee ee ee ee ee #> #> #> #> -- -- -- -- "
      "-- -- #> #> #> #> #> #> #> #> #> #> -- -- -- -- -- -- -- "
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
        (mk-contents (add-content 1   t_rune_c)
                     (add-content 342 t_gold_coins)
                     (add-content 4   t_gem)
                     (add-content 1 t_sextant)
                     ))
       14 9)
  (put (spawn-pt 'skeletal-warrior) 7 9)
  (put (spawn-pt 'skeletal-spear-thrower) 10 9)
  )

 (list 'on-entry-to-dungeon-room) ; hooks
 nil ; edge entrances
 )
