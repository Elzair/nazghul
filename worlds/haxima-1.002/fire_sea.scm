;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_fire_sea 19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr {{ {{ rr "
      "rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr {{ ~! {{ rr "
      "rr {{ !! !! {{ bb {{ {{ {{ {{ {{ !! {{ {{ {{ ~! !_ ~! rr "
      "rr {{ !! !_ !! !! {{ !! !! {{ !! !_ !! {{ {{ {{ ~! {{ rr "
      "rr {{ !! !! {{ == {{ !! !_ !! !_ !_ !! {{ {{ !! ~! {{ rr "
      "rr {{ {{ {{ {{ !! !! !! !_ rr rr !_ !_ !! {{ == {{ {{ rr "
      "rr {{ {{ {{ !! !_ !_ !_ rr rr rr rr !_ !_ !! !! !! {{ rr "
      "rr {{ {{ {{ !! !_ !_ !_ rr {{ {{ rr rr !_ !_ !! {{ {{ rr "
      "rr {{ {{ {{ {{ !! !! rr rr {{ {{ {{ rr !_ !_ !! {{ {{ rr "
      "rr {{ {{ {{ {{ {{ !! rr rr {{ bb {{ {{ {{ == {{ {{ {{ rr "
      "rr {{ !! {{ !! =| !! rr rr {{ {{ {{ rr !_ !_ !! {{ {{ rr "
      "rr !! !_ !! !! {{ !! !_ rr rr rr {{ rr rr !_ !_ !! {{ rr "
      "rr {{ !! {{ {{ {{ {{ !! !_ !_ rr rr rr rr !_ !_ !_ !! rr "
      "rr rr {{ {{ {{ {{ {{ !! !_ !_ !_ !_ rr rr rr !_ !_ !_ rr "
      "rr rr rr {{ {{ {{ {{ {{ !! !! !_ !_ !_ !_ !_ !_ !_ !_ rr "
      "rr rr rr {{ bb {{ {{ {{ {{ {{ !! !_ !! !! !_ !_ !_ !_ rr "
      "rr rr {{ {{ {{ {{ bb {{ {{ {{ {{ !! {{ {{ !! !_ !_ !_ rr "
      "rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ bb !! !! ~! rr "
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
 'p_fire_sea ; tag
 "Fire Sea"   ; name
 nil              ; sprite
 m_fire_sea  ; map
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
  (put (kern-mk-obj t_rune_w 1) 11 12)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 8)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 8)
  (put (kern-mk-obj t_iron_helm_4 1) 10 8)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 9)
  (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 10 9)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 9)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 10)
  (put (kern-mk-obj t_sword_4 1) 9 10)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 10)
  (put (kern-mk-obj t_armor_plate_4 1) 11 11)
  (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 9 11)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 11)
  (put (kern-mk-obj t_shield_4 1) 10 11)
  (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 11 11)
  (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 12)
  )

 nil ; hooks
 nil ; edge entrances
 )
