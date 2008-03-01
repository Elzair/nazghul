;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_engineers_hut 19 19 pal_expanded
	(list
		"** *. *8 *8 *8 *8 *8 *8 *8 ** ** ee ee *. ** *8 ** ** ** "
		"*c rr rr rr rr ws rr rr rr rr *4 ee ee *2 rr rr rr ** ** "
		".. rr rr [[ @@ @@ @@ ]] rr rr *6 ee ee *e rr ,, *b rr *. "
		"|| rr 00 ,, ,, ,, ,, ,, ,, rr *e /0 /d /d ,, ,, ,, *a ** "
		"*d rr 00 ,, ,, ,, ,, ,, ,, rr .. /7 .. .. rr ,, ,, rr *a "
		".. rr 00 ,, ,, [[ ]] ,, ,, ,, /d /6 .. .. rr ,, bb rr .. "
		"~~ rr ,, ,, ,, ,, ,, ,, ,, rr .. /7 .. .. rr .. ,, rr .. "
		"~~ bb ~~ ,, ,, ,, ,, ,, ,, rr .. /7 .. .. rr ,, ,, rr .. "
		"bb ~~ ~~ ,, ,, ,, [[ @@ ]] rr .. /7 .. .. rr rr rr rr .. "
		"~~ rr ,, ,, ,, ,, rr rr rr rr .. /7 .. bb .. .. .. tC t3 "
		".. rr ,, ,, ,, ,, 00 rr .. .. .. /7 .. .. .. .. .. t3 tt "
		".. rr ,, ,, ,, ,, 00 rr .. /0 /d /9 /d /d /d /2 .. ta tt "
		".. rr ,, rr && rr rr rr .. /7 .. .. .. .. .. /7 .. t% ta "
		"bb .. .. rr rr rr ~~ .. .. /7 .. .. .. rr rr ,, rr rr .. "
		"bb .. .. .. .. .. ~2 bb .. /7 .. .. rr rr ,, ,, ,, rr .. "
		"bb .. .. .. .. .. ~a ~~ ~9 =| ~9 ~~ rr && ,, ,, ,, rr .. "
		"bb .. .. .. .. .. .. bb .. /7 .. ~~ rr rr ,, ,, ,, rr .. "
		"bb .. .. .. .. .. bb .. .. /7 .. ~a ~~ rr rr rr rr rr .. "
		"t5 bb bb bb bb bb t7 .. .. /7 .. .. ~6 .. .. .. .. .. .. "
	)
)

;;----------------------------------------------------------------------------
;; NPC's
;;----------------------------------------------------------------------------
(kern-load "engineer.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_engineers_hut     ; tag
 "Engineers Hut"      ; name
 s_hamlet      ; sprite
 m_engineers_hut      ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects

  (put (mk-engineer) 9 9)

  (put (mk-npc 'bull 8) 3 15)
  (put (mk-door) 15 13)
  (put (mk-bed) 16 15)
  (put (mk-chest
        nil
        '((5 t_food))) 14 16)

  (put (mk-door) 14  3)

  (put (mk-door) 9 5)
  (put (mk-windowed-door) 2 12)
  (put (mk-clock) 5 9)
  (put (mk-broken-clock s_clock_stopped s_clock_stopped "The internals of this clock are spread across the table") 8 7)

  (put (kern-mk-obj t_voidship_plans 1) 16 7)
  )

 nil ; hooks
 (list  ;; edge entrances
  (list east  6 18)
  (list south 11 0) 
  (list southwest 11 0) 
  (list southeast 11 0) 
  (list northeast 6 18)
  (list north 9 18)
  (list west  18 9)
  )
 )

(mk-place-music p_engineers_hut 'ml-small-town)
