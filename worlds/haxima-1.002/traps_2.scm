;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_traps_2 19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx ,, ,, ,, ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, xx xx "
      "xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx "
      "xx xx xx xx ,, xx xx ,, ,, ,, ,, ,, xx xx ,, xx xx xx xx "
      "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx xx xx "
      "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx xx xx "
      "xx xx xx xx ,, xx xx xx xx ~x xx xx xx xx ,, xx xx xx xx "
      "xx xx xx xx ,, xx xx .C .H ~O .O .S .E xx ,, xx xx xx xx "
      "xx xx xx xx ,, xx xx !! !! ~! !! !! !! xx ,, xx xx xx xx "
      "xx xx xx xx ,, xx xx ,, ,, ,, ,, ,, ,, xx ,, xx xx xx xx "
      "xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx "
      "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx !! !! !! !! !! !! xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx .W .I .S .E .L .Y xx xx xx xx xx xx "
    )
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Other dungeon rooms
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_traps_2 ; tag
 "The Thief's Ladder II"   ; name
 nil              ; sprite
 m_traps_2  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 nil ;; neighbors 
 ;; objects
 (list
  (put (mk-step-clue "Doesn't the other way look better?") 13 15)
  (put (mk-step-clue "Doesn't the other way look better?") 6 15)

  ;; secret path through wall
  (put (mk-secret-path) 9 14)
  (put (mk-secret-path) 9 13)
  (put (mk-secret-path) 9 12)
  (put (mk-secret-path) 9 11)
  (put (mk-secret-path) 9 10)
  (put (mk-secret-path) 9 9)

  ;; ladders "down" that link to each other
  (put (mk-ladder-down 'p_traps_2 14 3) 4 3)
  (put (mk-ladder-down 'p_traps_2 4 3) 14 3)

  ;; monster generators
  (put (mk-mongen2 1 5 'is-bandit? 'mk-bandit nil) 4 3)
  (put (mk-mongen2 1 5 'is-skeleton? 'mk-skeletal-warrior nil) 14 3)

  ;; doors
  (put (mk-door) 14 8)
  (put (mk-door) 4 8)

  ;; true ladder down
  (put (mk-ladder-down 'p_traps_3 9 9) 9 9)

  )
 nil ; hooks
 nil ; edge entrances
 )
