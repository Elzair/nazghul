;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
  (kern-mk-map
    'm_traps_1 19 19 pal_expanded
    (list
      "rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr "
      "rr rr rr xx xx xx xx xx ,, ,, ,, xx xx xx xx xx rr rr rr "
      "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr xx x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! rr rr rr "
      "rr rr xx xx ,, xx xx xx xx ,, xx xx xx xx ,, xx xx rr rr "
      "rr rr xx xx ,, xx xx xx xx ,, xx xx xx xx ,, xx xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx rr rr "
      "rr rr xx xx ,, xx xx xx xx ,, xx xx xx xx ,, xx xx rr rr "
      "rr rr rr x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! rr rr rr "
      "rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr "
      "rr rr rr xx xx xx xx xx ,, ,, ,, xx xx xx xx xx rr rr rr "
      "rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr "
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
 'p_traps_1 ; tag
 "The Thief's Ladder I"   ; name
 nil              ; sprite
 m_traps_1  ; map
 #f               ; wraps
 #t                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 nil ;; neighbors 
 ;; objects
 (list
  (put (mk-step-clue "Ready to give up? Enter here!") 8 15)
  (put (mk-riddle 'few t_lava 3 5 3 9 
                  "All who would pass must answer the riddle:\n\n"
                  "  I know a word of letters three.\n"
                  "  Add two, and fewer there will be.") 4 14)
  (put (mk-riddle 'few t_lava 8 5 3 9
                  "All who would pass must answer the riddle:\n\n"
                  "  I know a word of letters three.\n"
                  "  Add two, and fewer there will be.") 9 14)
  (put (mk-riddle 'few t_lava 13 5 3 9
                  "All who would pass must answer the riddle:\n\n"
                  "  I know a word of letters three.\n"
                  "  Add two, and fewer there will be.") 14 14)
  )
 nil ; hooks
 nil ; edge entrances
 )
