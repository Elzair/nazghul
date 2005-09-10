;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_grey_goblin_village 19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr {{ {{ {{ rr {{ {{ {{ {{ {{ {{ {{ rr rr rr {{ {{ {{ rr "
      "rr {{ .. {{ rr {{ ,, ,, ,, ,, {{ {{ {{ rr {{ ,, ,, {{ rr "
      "rr {{ ,, ,, {{ ,, ,, ,, && ,, ,, bb {{ {{ {{ ,, ,, {{ rr "
      "rr {{ .. ,, .. .. ,, ,, ,, ,, ,, ,, .. .. .. ,, ,, {{ rr "
      "rr {{ {{ {{ {{ {{ .. .. ,, ,, {{ {{ ,, {{ {{ ,, ,, {{ rr "
      "rr rr rr rr rr {{ {{ {{ .. {{ bb {{ ,, rr {{ .. ,, {{ rr "
      "{{ {{ {{ {{ bb {{ bb {{ .. .. {{ ,, ,, rr {{ .. ,, {{ rr "
      "{{ {{ {{ {{ bb {{ {{ .. ,, ,, ,, ,, {{ rr rr {{ .. {{ rr "
      ",, {{ ,, .. .. .. .. ,, .. ,, ,, ,, {{ {{ rr rr {{ {{ rr "
      ",, ,, .. {{ bb {{ .. ,, ,, ,, ,, .. {{ {{ {{ rr {{ {{ rr "
      "{{ {{ {{ {{ bb {{ {{ ,, ,, ,, ,, ,, .. .. {{ rr rr rr rr "
      "rr rr rr rr rr {{ bb {{ ,, ,, ,, {{ ,, .. .. ~~ ~~ rr rr "
      "rr rr rr rr rr {{ {{ {{ {{ ,, {{ {{ {{ .. ~~ -- -- ~~ rr "
      "rr rr rr rr rr rr rr bb bb .. bb bb rr {{ ~~ -- -- ~~ rr "
      "rr rr rr rr rr rr rr {{ {{ .. {{ {{ rr {{ {{ ~~ ~~ {{ rr "
      "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr {{ {{ {{ {{ rr "
      "rr rr rr rr rr rr rr {{ {{ {{ .. {{ rr rr {{ {{ {{ rr rr "
      "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr rr rr rr rr rr "
    )
  )
;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Other dungeon rooms
;;----------------------------------------------------------------------------
(load "troll-den.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_grey_goblin_village ; tag
 "Grey Goblin Village"  ; name
 nil                    ; sprite
 m_grey_goblin_village  ; map
 #f                     ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list (list p_troll_den south))
 
 ;; objects
 (list
  (put (mk-mongen2 100 10 'is-goblin? 'mk-goblin-hunter nil) 8 4)
  (put (mk-mongen2 100 10 'is-goblin? 'mk-goblin-raider nil) 9 9)
  )

 nil ; hooks
 nil ; edge entrances
 )
