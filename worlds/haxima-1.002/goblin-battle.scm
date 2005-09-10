;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_goblin_battle 19 19 pal_expanded
    (list
            "rr rr rr rr rr rr rr {{ {{ ,, ,, {{ rr rr rr rr rr rr rr "
            "rr rr rr rr rr rr rr rr {{ ,, {{ {{ {{ {{ {{ rr rr rr rr "
            "rr rr rr rr rr rr rr rr {{ {{ ,, .. {{ rr {{ {{ {{ rr rr "
            "rr rr rr rr rr rr rr rr rr {{ ,, bb .. {{ {{ {{ {{ rr rr "
            "rr rr rr rr rr rr rr rr rr ,, {{ .. .. .. .. {{ {{ rr rr "
            "rr rr rr rr rr rr rr rr rr rr {{ {{ {{ .. bb .. {{ {{ rr "
            "rr rr rr rr rr rr rr rr rr rr bb {{ {{ {{ .. {{ {{ {{ rr "
            "{{ {{ {{ rr rr rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ "
            ",, {{ {{ .. bb rr rr rr rr rr rr {{ {{ {{ {{ {{ ,, {{ ,, "
            ",, {{ ,, {{ .. {{ rr rr rr rr rr {{ {{ ,, ,, ,, {{ ,, ,, "
            ",, ,, ,, .. ,, ,, {{ rr rr rr {{ {{ {{ {{ ,, ,, ,, {{ ,, "
            "{{ {{ {{ .. bb .. .. {{ {{ {{ {{ {{ .. {{ {{ {{ {{ {{ {{ "
            "rr {{ {{ {{ .. .. bb .. {{ {{ {{ .. bb .. {{ {{ {{ rr rr "
            "rr rr {{ rr rr {{ .. .. .. {{ {{ .. .. {{ {{ {{ rr rr rr "
            "rr rr rr rr {{ {{ {{ .. .. .. .. .. {{ {{ {{ {{ rr rr rr "
            "rr rr rr rr {{ {{ {{ {{ {{ .. bb .. {{ {{ rr rr rr rr rr "
            "rr rr rr rr rr {{ {{ {{ {{ {{ .. {{ {{ {{ {{ rr rr rr rr "
            "rr rr rr rr rr rr rr rr {{ {{ {{ rr rr rr rr rr rr rr rr "
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
(load "grey-goblin-village.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_goblin_battle ; tag
 "Intersection"   ; name
 nil              ; sprite
 m_goblin_battle  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list (list p_grey_goblin_village east)
       )
 
 ;; objects
 (list
  (put (mk-mongen2 250 5 'is-goblin? 'mk-goblin-hunter nil) 18 8)
  (put (mk-mongen2 500 5 'is-goblin? 'mk-goblin-raider nil) 18 10)
  )

 nil ; hooks
 nil ; edge entrances
 )
