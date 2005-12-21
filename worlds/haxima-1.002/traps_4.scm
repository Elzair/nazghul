;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_traps_4 19 19 pal_expanded
    (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ,, ,, xx "
      "xx ,, pp ~, pp ~, pp ~, pp ~, pp ~, pp ,, pp ~, pp ,, xx "
      "xx ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
      "xx ,, pp ,, pp ~, pp ~, pp ~, pp ,, pp ~, pp ,, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
      "xx ,, pp ,, pp ~, pp ,, pp ~, ~p ~, pp ,, pp ~, pp ,, xx "
      "xx ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ~, ,, xx "
      "xx ,, pp ~, pp ,, pp ,, pp ~, pp ~, pp ~, pp ,, pp ,, xx "
      "xx ,, ~, ,, ,, ,, ~, ,, ,, ,, ,, ,, ,, ,, ~, ,, ~, ,, xx "
      "xx ,, pp ,, pp ~, pp ~, pp ~, pp ~, pp ,, pp ,, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, pp ,, pp ,, pp ,, pp ~, pp ~, pp ,, pp ~, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
      "xx ,, pp ,, pp ,, pp ,, pp ,, pp ,, pp ~, pp ~, pp ,, xx "
      "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,, pp ,, pp ~, pp ~, pp ~, pp ~, pp ~, pp ~, pp ,, xx "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
    )
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Special Objects
;;----------------------------------------------------------------------------
;(kern-load "traps_4_mechs.scm")

;;----------------------------------------------------------------------------
;; Other dungeon rooms
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_traps_4 ; tag
 "Labyrinth of Burning Glass"   ; name
 nil              ; sprite
 m_traps_4  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 nil ;; neighbors 
 ;; objects
 (list
  (put (mk-ladder-up 'p_traps_3 9 3) 9 9)
  (put (mk-ladder-down 'p_thiefs_den 9 9) 9 7)

  )
 nil ; hooks
 nil ; edge entrances
 )
