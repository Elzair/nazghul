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
 nil                    ; neighbors
 
 ;; objects
 (list
  (put (mk-generator t_goblin_hunter_generator) 8 4)
  (put (mk-generator t_goblin_raider_generator) 9 9)
  )

 nil ; hooks
 nil ; edge entrances
 )
