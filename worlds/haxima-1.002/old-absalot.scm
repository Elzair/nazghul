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
      "rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr rr rr ,M ,E ,A ,T rr rr rr rr "
      "rr rr rr rr rr rr xx ,, ,, pp ,, ,, ,, ,, ,, ,, pp ,, ,, rr rr rr rr ,, ,, ,, ,, rr rr rr rr "
      "rr rr rr rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr ,, ,, ,, ,, rr rr rr rr "
      "rr rr rr rr rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr ,, ,, ,, ,, ,, ,, rr rr rr rr "
      "rr rr rr rr rr rr rr rr rr xx ,, pp ,, ,, pp ,, rr xx xx rr rr ,, xx xx xx xx xx xx xx xx xx "
      "rr rr rr rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, xx rr rr rr rr ,, xx ,F ,L ,E ,S ,H ,L ,Y xx "
      "rr rr rr rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, xx rr rr rr rr ,, rr ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr xx ,, pp ,, ,, pp ,, xx rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, xx rr rr rr rr rr xx ,, ,, ,, aa ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx rr rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr rr rr xx ,, ,, rr rr rr rr rr xx xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr rr rr rr rr rr rr rr rr rr rr xx ,, ,, xx rr rr rr xx ,, ,, ,, xx ,, ,, [[ @@ ]] ,, ,, xx "
      "xx xx xx rr xx xx xx xx xx xx xx rr ,, ,, xx xx rr xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
      "xx ,S ,L ,A ,V ,E ,S xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx "
      "rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx ,F ,E ,A ,S ,T xx xx "
      "rr ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr xx xx xx xx rr rr xx "
      "xx ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx xx xx xx rr cc cc cc cc cc rr "
      "xx xx rr ,, ,, xx xx xx rr xx ,, ,, ,, xx xx ,, ,, ,, xx ,B ,L ,O ,O ,D xx cc cc ~~ ~~ cc rr "
      "xx ,, ,, ,, ,, ,, ,, xx rr xx ,, ,, ,, rr xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, cc ~~ ~~ ~~ cc rr "
      "xx xx xx ,, ,, xx xx xx rr ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc ~~ aa cc cc rr "
      "xx ,, ,, ,, ,, ,, ,, xx xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, cc ~~ ~~ ~~ cc xx "
      "xx xx xx ,, ,, xx xx xx xx ,, ,, ,, ,, ,, xx xx rr rr xx ,B ,A ,T ,H ,S xx cc cc ~~ ~~ cc xx "
      "xx ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx rr rr rr rr xx xx xx xx xx xx xx cc cc cc cc cc xx "
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

 (list ; objects
  (put (mk-ladder-down 'p_absalot 2 2) 11 27)

  ;; slave prison
  (put (mk-locked-windowed-door) 2 25)
  (put (mk-locked-windowed-door) 2 27)
  (put (mk-locked-windowed-door) 5 29)
  (put (mk-locked-windowed-door) 5 27)
  (put (mk-locked-windowed-door) 5 25)
  (put (mk-corpse) 1 25)
  (put (mk-corpse) 1 27)
  (put (mk-corpse) 1 29)
  (put (mk-corpse) 6 25)
  (put (mk-corpse) 6 27)
  (put (mk-corpse) 6 29)

  ;; meat locker
  (put (mk-locked-door) 22 14)
  (put (mk-corpse) 26 8)
  (put (mk-corpse) 26 9)
  (put (mk-corpse) 26 10)
  (put (mk-corpse) 25 8)
  (put (mk-corpse) 25 9)
  (put (mk-corpse) 24 8)


  )

 nil ; hooks
 nil ; edge entrances
 )

