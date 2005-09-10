;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_troll_den 19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr {{ .. ,, ,, {{ rr rr rr rr rr rr rr "
      "rr {{ {{ {{ rr rr rr {{ .. ,, {{ {{ rr rr rr rr rr rr rr "
      "rr {{ {{ {{ {{ rr rr {{ .. .. ,, {{ rr rr rr rr rr rr rr "
      "rr {{ {{ {{ {{ {{ rr rr {{ .. ,, bb rr rr rr rr rr rr rr "
      "rr rr {{ {{ rr {{ {{ rr {{ ,, .. {{ rr rr rr rr rr rr rr "
      "rr rr {{ rr rr rr {{ {{ {{ .. {{ {{ rr rr rr rr rr rr rr "
      "rr rr {{ {{ rr {{ {{ rr {{ .. .. {{ rr {{ {{ rr rr rr rr "
      "rr {{ {{ {{ .. {{ rr rr rr {{ .. .. .. .. {{ {{ {{ rr rr "
      "rr {{ {{ .. bb .. {{ rr {{ .. .. rr {{ .. .. {{ .. .. rr "
      "rr {{ {{ {{ .. {{ {{ {{ {{ .. rr rr rr {{ .. .. .. .. rr "
      "rr rr {{ {{ {{ {{ rr {{ .. .. {{ rr {{ {{ .. .. .. && rr "
      "rr rr rr {{ {{ rr rr rr .. {{ {{ {{ .. .. .. .. .. .. rr "
      "rr rr rr {{ .. {{ rr {{ .. rr rr .. bb .. .. {{ .. .. rr "
      "rr rr {{ bb .. .. {{ .. .. rr rr rr .. {{ {{ {{ {{ rr rr "
      "rr {{ .. .. .. .. .. .. {{ {{ rr rr rr rr {{ {{ {{ rr rr "
      "rr {{ .. .. .. bb {{ {{ {{ {{ rr rr rr rr rr rr rr rr rr "
      "rr {{ {{ .. .. bb {{ {{ rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
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
 'p_troll_den ; tag
 "Troll Den"   ; name
 nil              ; sprite
 m_troll_den  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 nil

 ;; objects
 (list
  (put (mk-mongen2 500 3 'is-troll? 'mk-troll nil) 16 10)
  )

 nil ; hooks
 nil ; edge entrances
 )
