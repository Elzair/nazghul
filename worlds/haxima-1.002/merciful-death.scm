;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_merciful_death 19 19 pal_expanded
    (list
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. ~~ ~~ ~~ ~~ ~~ .. .. .. .. .. ~~ ~~ ~~ ~~ ~~ .. .. "
      "~~ ~~ ~~ bb -- -- ~~ ~~ ~~ ee ~~ ~~ ~~ -- -- -- ~~ ~~ .. "
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
  (put (mk-ladder-down 'p_merciful_death_l2 6 9) 6 9)
  (put (mk-at-level 'mk-skeletal-warrior "1d3+5") 7 9)
  (put (mk-at-level 'mk-skeletal-warrior "1d3+5") 10 9)
  (put (mk-edge-gen 995 2 'is-skeletal-warrior? 'mk-at-level (list 'mk-skeletal-warrior "1d3+5"))
       6 9)
  )

 nil ; hooks
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

  (put (mk-ladder-up 'p_merciful_death 6 9) 6 9)
  (put (mk-chest
        'poison-trap
        (mk-contents (add-content 1   t_rune_c)
                     (add-content 342 t_gold_coins)
                     (add-content 4   t_gem)))
       14 9)
  (put (mk-at-level 'mk-skeletal-warrior "1d3+5") 7 9)
  (put (mk-at-level 'mk-skeletal-warrior "1d3+5") 10 9)
  (put (mk-edge-gen 990 2 'is-skeletal-warrior? 'mk-at-level (list 'mk-skeletal-warrior "1d3+5"))
       6 9)
  )

 nil ; hooks
 nil ; edge entrances
 )
