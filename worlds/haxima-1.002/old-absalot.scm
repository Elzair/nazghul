;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------

(kern-mk-map
 'm_old_absalot 31 31 pal_expanded
 (list
      "rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr xx xx ,T ,E ,M ,P ,L ,E xx xx rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr xx !! cc cc cc cc cc cc !! xx xx rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr !! !! cc aa cc cc aa cc !! !! xx xx rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr xx !! !! !! cc cc cc cc cc cc !! !! !! xx rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr xx ,, ,, pp ,, ,, ,, ,, ,, ,, pp ,, ,, xx rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr xx ,, ,, pp ,, ,, ,, ,, ,, ,, pp ,, ,, rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx rr rr rr rr rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr xx ,, pp ,, ,, pp ,, rr xx xx ,, ,, ,, xx xx xx xx xx xx xx xx xx "
      "rr rr rr rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, xx rr xx ,, ,, ,, xx ,F ,L ,E ,S ,H ,L ,Y xx "
      "rr rr rr rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, xx rr rr xx ,, rr xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr xx ,, pp ,, ,, pp ,, xx rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, xx rr xx ,, ,, ,, ,, ,, ,, ,, aa ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr rr rr xx ,, ,, rr rr rr rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr rr rr xx ,, ,, xx rr rr rr xx ,, ,, ,, xx ,, ,, [[ @@ ]] ,, ,, xx "
      "xx xx xx rr xx xx xx xx xx xx xx rr ,, ,, xx xx rr xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,S ,L ,A ,V ,E ,S xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx ,F ,E ,A ,S ,T xx xx "
      "rr ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr xx xx xx xx rr rr xx "
      "xx ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx xx xx xx rr ,, ,, ,, ,, ,, rr "
      "xx xx rr ,, ,, xx xx xx rr xx ,, ,, ,, xx xx ,, ,, ,, xx ,B ,L ,O ,O ,D xx ,, ,, ~~ ~~ ,, rr "
      "xx ,, ,, ,, ,, ,, ,, xx rr xx ,, ,, ,, rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ~~ ~~ ~~ ,, rr "
      "xx xx xx ,, ,, xx xx xx rr ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~~ ~~ ~~ ,, rr "
      "xx ,, ,, ,, ,, ,, ,, xx xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ~~ ~~ ~~ ,, xx "
      "xx xx xx ,, ,, xx xx xx xx ,, ,, ,, ,, ,, xx xx rr rr xx ,B ,A ,T ,H ,S xx ,, ,, ~~ ~~ ,, xx "
      "xx ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx rr rr rr rr xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx "
      "xx xx xx rr rr rr xx xx rr rr xx xx xx xx rr rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx "
))

;;----------------------------------------------------------------------------
;; NPC's
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_old_absalot     ; tag
 "Old Absalot"      ; name
 nil          ; sprite
 m_old_absalot      ; map
 #f              ; wraps
 #t              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 nil ; objects
 nil ; hooks
 nil ; edge entrances
 )

