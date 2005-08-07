;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
(kern-mk-map 
 'm_grey_goblin_village 19 19 pal_expanded
 (list
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr" ; 0
  "rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. .. .. .. rr" ; 1
  "rr rr rr rr rr .. .. && .. .. rr rr .. .. .. .. .. .. rr" ; 2
  "rr rr rr rr bb .. .. .. .. .. bb rr rr .. rr .. .. .. rr" ; 3
  "rr bb .. .. bb .. .. .. .. .. bb .. .. .. rr .. .. .. rr" ; 4
  "rr bb .. .. .. bb bb .. bb bb .. .. .. .. .. .. .. .. rr" ; 5
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr" ; 6
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr .. .. rr" ; 7
  ".. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr .. .. rr" ; 8
  ".. .. .. .. .. .. .. .. .. .. .. .. rr .. .. rr .. .. rr" ; 9
  ".. .. .. .. .. .. .. rr rr .. .. rr rr .. .. rr .. .. rr" ; 0
  ".. .. rr .. rr bb rr rr rr rr .. rr rr .. rr rr .. .. rr" ; 1
  "rr %% rr .. .. .. rr .. .. rr .. rr rr .. rr rr .. .. rr" ; 2
  "~~ bb bb rr .. .. rr .. .. .. .. .. .. .. rr rr .. .. rr" ; 3
  "rr ~~ bb ~~ .. .. rr rr .. rr rr .. rr rr rr rr .. .. rr" ; 4
  "rr rr %% ~~ ~~ ~~ bb rr rr rr rr .. rr rr rr rr .. .. rr" ; 5
  "rr .. .. .. .. ~~ bb bb rr rr .. .. .. rr rr rr .. .. rr" ; 6
  "rr .. rr .. %% rr bb ~~ bb %% .. .. .. rr rr rr .. .. rr" ; 7
  "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr" ; 8
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
  (put (mk-ladder-up 'p_shard 39 75) 9 9)
  )

 nil ; hooks
 nil ; edge entrances
 )
