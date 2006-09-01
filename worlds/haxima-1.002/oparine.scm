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
		".. .. xx xx xx xx ,, ,, xx xx xx sI cc cc cc cc cc xx xx [[ @@ ]] ,, ,, ,, [[ @@ ]] xx xx .. "
		".. .. xx ,, ,, xx ,, ,, ,, ,, ,, xx cc cc cc cc cc xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. "
		".. .. xx ,, ,, ,, ,, ,, xx ,, ,, ws cc cc cc cc cc xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx .. "
		".. .. xx xx xx xx ,, ,, xx xx xx xx cc cc cc cc cc xx xx [[ @@ ]] ,, ,, ,, [[ @@ ]] xx xx .. "
		".. .. xx ,, ,, xx ,, ,, ,, ,, ,, xx cc cc cc cc cc cc ws ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
		".. .. xx ,, ,, ,, ,, ,, xx ,, ,, ws cc cc cc cc cc cc xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. "
		".. .. xx xx xx xx xx ,, xx xx xx xx cc cc cc cc cc cc xx xx xx xx sT ,, xx xx xx xx xx .. .. "
		".. .. xx ,, ,, xx ,, ,, xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .. .. "
		".. .. xx ,, ,, xx ,, ,, xx cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc .. .. "
		".. .. xx ,, ws xx xx xx xx cc cc cc cc cc cc ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## "
		"cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ## _3 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 _1 __ "
		"cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc oo __ __ __ __ __ oo __ __ __ __ __ oo __ __ "
		"cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
		".. .. .. .. xx xx xx xx xx cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
		".. .. .. .. xx ,, ,, ,, xx cc cc cc cc cc cc cc ee ee ee ee ee ee ee ee ee ee ee ee ee ee ee "
		".. .. .. .. xx ,, ,, ,, ,, cc cc cc cc cc cc cc oo __ __ ee ee ee oo __ __ __ __ __ oo __ __ "
		".. .. .. .. xx ,, ,, ,, xx cc cc cc ## ## ## ## __ __ __ ee ee ee __ __ __ __ _8 __ __ __ __ "
		".. xx xx xx xx xx xx xx xx cc cc cc ## _3 _1 __ __ __ __ ee ee ee __ __ _c ## ## ## _a __ __ "
		".. xx ,A ,L ,K ,E ,M ,Y xx cc cc cc ## _2 __ __ __ __ __ ee ee ee __ __ ## ## ee ## ## __ __ "
		".. xx ,, ,, ,, ,, ,, ,, ws cc cc cc ## _2 xx xx xx xx oo ee ee ee oo _c ## ee ee ee ## _a __ "
		".. xx ,, ,, ,, ,, ,, ,, sP cc cc cc ## _2 xx ee ee xx ~~ ee ee ee __ ## ## ee ee ee ## ## __ "
		".. xx ,, ,, aa && ,, ,, ,, cc cc cc ## _2 xx ee ee ee ee ee ee ee __ ## ee ee ee ee ee ## _2 "
		".. xx ,, ,, ,, ,, ,, ,, xx cc cc cc ## _2 xx ee ee xx __ ee ee ee ee ee ee ee ee ee ee ## _2 "
		".. xx ,, ,, ,, ,, ,, ,, ws cc cc cc ## _2 xx xx xx xx __ ee ee ee __ ## vv vv ee ee vv ## _2 "
		".. xx [[ @@ @@ @@ @@ ]] xx cc cc cc ## _2 __ __ __ __ __ ee ee ee _c ## ee ee ee ee ee ## _2 "
		".. xx xx xx xx xx xx xx xx cc cc cc ## ~a ~~ ~~ __ __ oo ee ee ee oo ## vv vv vv vv vv ## _2 "
		".. .. .. .. .. .. .. .. bb .. .. .. bb .. ~% ~~ -- __ __ ee ee ee _5 ## ee ee ee ee ee ## _2 "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ~a ~~ -- __ __ __ __ __ ## ## vv vv vv ## ## __ "
		".. .. .. .. .. .. .. .. bb .. .. .. bb .. .. ~% ~~ ~~ -- __ __ __ __ _5 ## ee ee ee ## _3 __ "
		".. xx xx xx xx xx xx xx .. .. .. .. .. .. .. .. ~a ~~ -- -- __ __ __ __ ## ## vv ## ## __ __ "
		".. xx [[ .S .E .A ]] xx bb .. .. .. bb .. .. .. ~% ~~ ~~ -- __ __ __ __ _5 ## ## ## _3 __ __ "
		".. xx .W .I .T .C .H sR .. .. .. .. .. .. .. .. ~C ~~ ~~ -- -- __ __ __ __ __ _1 __ __ __ __ "
		".. xx ,, ,, ,, ,, ,, ws bb .. .. .. bb .. .. .. ~3 ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ "
		".. xx ,, ,, ,, ,, ,, ,, .. .. .. .. .. .. .. ~C ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ __ __ "
		".. xx ,, [[ @@ ]] ,, ws bb .. .. .. bb .. .. ~3 ~~ ~~ -- -- -- -- __ __ __ __ __ __ __ __ __ "
		".. xx xx ,, ,, ,, xx xx .. .. .. .. .. .. .. ~2 ~~ ~~ ~~ ~~ -- -- -- __ __ __ __ __ __ __ __ "
		".. .. xx xx ,, xx xx .. bb .. .. .. oo .. oo ~8 ~8 ~~ ~~ ~~ -- -- -- __ __ __ __ __ __ __ __ "
		".. .. xx ,, ,, ,, xx .. .. .. .. .. ee ee ee .. .. bb ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
		".. .. xx ,, ,, ,, ws .. bb .. .. .. ee ee ee .. .. .. ~a ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
		".. .. xx xx ,, xx xx .. .. .. .. .. ee ee ee .. .. .. bb ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
		".. .. .. bb .. bb .. .. .. .. bb .. oo ~~ oo .. .. .. ~3 ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
		".. .. .. .. .. .. .. .. .. .. .. .. .. ~6 .. .. .. .. ~~ ~~ ~~ -- -- __ __ __ __ __ __ __ __ "
		".. .. bb ~3 ~1 ~5 bb .. .. .. .. ~C ~3 ~~ oo ee ee ee oo ~~ ~~ -- -- -- __ __ __ __ __ __ __ "
		"~C ~3 ~~ ~~ ~~ ~~ ~~ ~5 ~A .. ~C ~3 ~~ ~~ ~~ ee ee ee ~~ ~~ ~~ -- -- -- -- __ __ __ __ __ __ "
		"~~ ~~ -- -- -- -- -- ~~ ~~ ~1 ~~ ~~ ~~ ~c oo ee ee ee oo ~a ~~ ~~ -- -- -- __ __ __ __ __ __ "
		"-- -- -- __ __ __ -- -- -- -- -- ~~ ~c ~# .. .. .. .. .. ~% ~a ~~ ~~ -- -- __ __ __ __ __ __ "
		"-- -- -- __ __ __ -- -- -- -- -- ~~ ~# .. .. .. .. .. .. .. ~% ~~ ~~ -- -- -- __ __ __ __ __ "
	)
)

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
  (put (mk-mirror s_mirror_bg_flagstones) 5 45)

  ;; Alkemysts
  (put (mk-door) 8 29)
  (put (mk-magic-locked-door) 8 23)
  (put (mk-bed) 5 22)

  ;; tavern
  (put (mk-locked-door) 19 5)
  (put (mk-windowed-door) 23 14)
  (put (mk-clock) 27 13)
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





