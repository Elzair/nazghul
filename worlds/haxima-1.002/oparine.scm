;;----------------------------------------------------------------------------
;; Oparine
;;
;; Main trade port for the peninsula.
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_oparine 31 55 pal_expanded
 (list
  ".. .. .. .. .. .. .. .. .. .. .. .. cc cc cc cc cc cc .. .. .. .. .. .. .. .. .. .. .. .. .. "
  ".. .. xx xx xx xx xx xx xx xx xx xx cc cc cc cc cc cc xx xx xx xx xx xx xx xx xx xx xx xx .. "
  ".. .. xx .C .H .E .E .R .F .U .L xx cc cc cc cc cc cc xx ,, xx .B .I .L .G .E xx ,, ,, xx .. "
  ".. .. xx .B .U .N .K .M .A .T .E xx cc cc cc cc cc cc xx ,, xx .W .A .T .E .R xx ,, ,, xx .. "
  ".. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, xx cc cc cc cc cc cc xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. "
  ".. .. xx [[ @@ @@ @@ @@ @@ @@ ]] xx cc cc cc cc cc cc xx ,, xx [[ @@ @@ @@ ]] xx xx xx xx .. "
  ".. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ws cc cc cc cc cc cc xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
  ".. .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, cc cc cc cc cc cc ws ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
  ".. .. xx xx xx xx ,, ,, xx xx xx xx cc cc cc cc cc xx xx [[ @@ ]] ,, ,, ,, [[ @@ ]] xx xx .. "
  ".. .. xx ,, ,, xx ,, ,, ,, ,, ,, xx cc cc cc cc cc xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. "
  ".. .. xx ,, ,, ,, ,, ,, xx ,, ,, ws cc cc cc cc cc xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. "
  ".. .. xx xx xx xx ,, ,, xx xx xx xx cc cc cc cc cc xx xx [[ @@ ]] ,, ,, ,, [[ @@ ]] xx xx .. "
  ".. .. xx ,, ,, xx ,, ,, ,, ,, ,, xx cc cc cc cc cc cc ws ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
  ".. .. xx ,, ,, ,, ,, ,, xx ,, ,, ws cc cc cc cc cc cc xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
  ".. .. xx xx xx xx xx ,, xx xx xx xx cc cc cc cc cc cc xx xx xx xx xx ,, xx xx xx xx xx .. .. "
  ".. .. xx ,, ,, xx ,, ,, xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .. .. "
  ".. .. xx ,, ,, xx ,, ,, xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .. .. "
  ".. .. xx ,, ws xx xx xx xx cc cc cc cc cc cc ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ## _! _! _! _! _! _! _! _! _! _! _! _! _! _! _! "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc oo _! _! _! _! _! oo _! _! _! _! _! oo _! _! "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
  ".. .. .. .. xx xx xx xx xx cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
  ".. .. .. .. xx ,, ,, ,, xx cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
  ".. .. .. .. xx ,, ,, ,, ,, cc cc cc cc cc cc cc oo _! _! ee ee ee oo _! _! _! _! _! oo _! _! "
  ".. .. .. .. xx ,, ,, ,, xx cc cc cc ## ## ## ## _! _! _! ee ee ee _! _! _! _! _! _! _! _! _! "
  ".. xx xx xx xx xx xx xx xx cc cc cc ## _! _! _! _! _! _! ee ee ee _! _! _! ## ## ## _! _! _! "
  ".. xx ,A ,L ,K ,E ,M ,Y xx cc cc cc ## _! _! _! _! _! _! ee ee ee _! _! ## ## ee ## ## _! _! "
  ".. xx ,, ,, ,, ,, ,, ,, ws cc cc cc ## _! xx xx xx xx oo ee ee ee oo _! ## ee ee ee ## _! _! "
  ".. xx ,, ,, ,, ,, ,, ,, xx cc cc cc ## _! xx ee ee xx ~~ ee ee ee _! ## ## ee ee ee ## ## _! "
  ".. xx ,, ,, aa && ,, ,, ,, cc cc cc ## _! xx ee ee ee ee ee ee ee _! ## ee ee ee ee ee ## _! "
  ".. xx ,, ,, ,, ,, ,, ,, xx cc cc cc ## _! xx ee ee xx _! ee ee ee ee ee ee ee ee ee ee ## _! "
  ".. xx ,, ,, ,, ,, ,, ,, ws cc cc cc ## _! xx xx xx xx _! ee ee ee _! ## vv vv ee ee vv ## _! "
  ".. xx [[ @@ @@ @@ @@ ]] xx cc cc cc ## _! _! _! _! _! _! ee ee ee _! ## ee ee ee ee ee ## _! "
  ".. xx xx xx xx xx xx xx xx cc cc cc ## ~~ ~~ ~~ _! _! oo ee ee ee oo ## vv vv vv vv vv ## _! "
  ".. .. .. .. .. .. .. .. bb .. .. .. bb .. .. ~~ -- _! _! ee ee ee _! ## ee ee ee ee ee ## _! "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ~~ ~~ -- _! _! _! _! _! ## ## vv vv vv ## ## _! "
  ".. .. .. .. .. .. .. .. bb .. .. .. bb .. .. .. ~~ ~~ -- _! _! _! _! _! ## ee ee ee ## _! _! "
  ".. xx xx xx xx xx xx xx .. .. .. .. .. .. .. .. ~~ ~~ -- -- _! _! _! _! ## ## vv ## ## _! _! "
  ".. xx [[ .S .E .A ]] xx bb .. .. .. bb .. .. .. .. ~~ ~~ -- _! _! _! _! _! ## ## ## _! _! _! "
  ".. xx .W .I .T .C .H xx .. .. .. .. .. .. .. .. .. ~~ ~~ -- -- _! _! _! _! _! _! _! _! _! _! "
  ".. xx ,, ,, ,, ,, ,, ws bb .. .. .. bb .. .. .. ~~ ~~ ~~ -- -- _! _! _! _! _! _! _! _! _! _! "
  ".. xx ,, ,, ,, ,, ,, ,, .. .. .. .. .. .. .. .. ~~ ~~ ~~ -- -- _! _! _! _! _! _! _! _! _! _! "
  ".. xx ,, [[ @@ ]] ,, ws bb .. .. .. bb .. .. ~~ ~~ ~~ -- -- -- -- _! _! _! _! _! _! _! _! _! "
  ".. xx xx ,, ,, ,, xx xx .. .. .. .. .. .. .. ~~ ~~ ~~ ~~ ~~ -- -- -- _! _! _! _! _! _! _! _! "
  ".. .. xx xx ,, xx xx .. bb .. .. .. ee .. ee ~~ ~~ ~~ ~~ ~~ -- -- -- _! _! _! _! _! _! _! _! "
  ".. .. xx ,, ,, ,, xx .. .. .. .. .. ee ee ee .. .. bb ~~ ~~ ~~ -- -- _! _! _! _! _! _! _! _! "
  ".. .. xx ,, ,, ,, ws .. bb .. .. .. ee ee ee .. .. .. ~~ ~~ ~~ -- -- _! _! _! _! _! _! _! _! "
  ".. .. xx xx ,, xx xx .. .. .. .. .. ee ee ee .. .. .. bb ~~ ~~ -- -- _! _! _! _! _! _! _! _! "
  ".. .. .. bb .. bb .. .. .. .. bb .. ee ~~ ee .. .. .. ~~ ~~ ~~ -- -- _! _! _! _! _! _! _! _! "
  ".. .. .. .. .. .. .. .. .. .. .. .. .. ~~ .. .. .. .. ~~ ~~ ~~ -- -- _! _! _! _! _! _! _! _! "
  ".. .. bb ~~ ~~ ~~ bb .. .. .. .. .. ~~ ~~ ee ee ee ee ee ~~ ~~ -- -- -- _! _! _! _! _! _! _! "
  ".. ~~ ~~ ~~ ~~ ~~ ~~ ~~ .. .. .. ~~ ~~ ~~ ~~ ee ee ee ~~ ~~ ~~ -- -- -- -- _! _! _! _! _! _! "
  "~~ ~~ -- -- -- -- -- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ee ee ee ee ee ~~ ~~ ~~ -- -- -- _! _! _! _! _! _! "
  "-- -- -- _! _! _! -- -- -- -- -- ~~ ~~ .. .. .. .. .. .. .. ~~ ~~ ~~ -- -- _! _! _! _! _! _! "
  "-- -- -- _! _! _! -- -- -- -- -- ~~ .. .. .. .. .. .. .. .. .. ~~ ~~ -- -- -- _! _! _! _! _! "

  ))

;;----------------------------------------------------------------------------
;; Characters
;;----------------------------------------------------------------------------
(kern-load "alchemist.scm")
(kern-load "oscar.scm")
(kern-load "henry.scm")
(kern-load "bart.scm")
(kern-load "lia.scm")
(kern-load "fing.scm")
(kern-load "ghertie.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_oparine     ; tag
 "Oparine"      ; name
 s_town          ; sprite
 m_oparine      ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors
 (list ;; objects

  ;; npc's
  (put (mk-alchemist) 0 0)
  (put (mk-oscar) 0 0)
  (put (mk-henry) 0 0)
  (put (mk-bart) 0 0)
  (put (mk-lia) 0 0)
  (put (mk-fing) 0 0)
  (put (mk-ghertie) 0 0)
  (put (mk-kalcifax) 0 0)

  ;; inn
  (put (mk-locked-door) 2 4)
  (put (mk-windowed-door) 11  7)
  (put (mk-bed)  3  9)
  (put (mk-bed)  3 12)
  (put (mk-bed)  6 15)
  (put (mk-bed) 10 12)
  (put (mk-bed) 10  9)
  (put (kern-tag 'oparine-inn-room-1-door (mk-locked-door))  5 10)
  (put (kern-tag 'oparine-inn-room-2-door (mk-locked-door))  5 13)
  (put (kern-tag 'oparine-inn-room-3-door (mk-magic-locked-door))  7 14)
  (put (kern-tag 'oparine-inn-room-4-door (mk-locked-door))  8 12)
  (put (kern-tag 'oparine-inn-room-5-door (mk-locked-door))  8  9)

  ;; innkeepers room
  (put (mk-door) 3 17)
  (put (mk-bed) 4 15)

  ;; bart's
  (put (mk-door) 17 29)

  ;; sea witch
  (put (mk-windowed-door) 7 41)
  (put (mk-locked-door) 4 47)  
  (put (mk-door) 4 44)
  (put (mk-bed) (zone-x sea-witch-bed) (zone-y sea-witch-bed))

  ;; Alkemysts
  (put (mk-door) 8 29)
  (put (mk-magic-locked-door) 8 23)
  (put (mk-bed) 5 22)

  ;; tavern
  (put (mk-locked-door) 19 5)
  (put (mk-windowed-door) 23 14)
  (put (mk-door) 20 4)
  (put (mk-door) 26 4)
  (put (mk-bed) 27 2)

  )
 (list 'lock-inn-room-doors)  ;; On-entry hook
 (list  ;; edge entrances
  (list east 0 19)
  (list north 16 54)
  (list west 30 21)
  )
)





