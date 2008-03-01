;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(kern-mk-map
 'm_poor_house 19 19 pal_expanded
	(list
		".. .. .. .. .. .. .. .. .. /7 .. bb bb bb bb bb bb bb ta "
		".. /0 /d /d /d /d /d /d /d /a bb .. .. .. .. t3 tt t5 bb "
		".. /7 .. .. .. .. .. .. .. .. bb .. .. .. .. te bb tt bb "
		".. /7 .. rr rr rr rr rr rr bb rr .. .. .. .. t% tb tc bb "
		".. /7 .. rr .P .O .O .R ]] rr .. .. .. .. .. .. .. .. bb "
		".. /7 .. rr .H .O .U .S .E rr .. .. .. .. .. .. .. .. bb "
		".. /7 .. ,, ,, ,, ,, ,, ,, rr rr rr ,, rr bb rr .. .. bb "
		".. /7 .. bb ,, ,, ,, ,, ,, rr ,, ,, ,, ,, .. rr .. .. bb "
		".. /7 .. rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr bb bb .. "
		"/d /6 .. rr rr rr ,, rr rr bb ,, ,, 00 ,, ,, wr .. /0 /d "
		".. /7 .. rr ,, ,, ,, ,, ,, rr ,, ,, 00 ,, ,, rr .. /7 .. "
		".. /7 .. rr ,, ,, ,, ,, ,, rr ,, ,, 00 ,, ,, rr .. /7 .. "
		".. /7 .. wr ,, ,, ,, ,, ,, rr ,, ,, ,, ,, ,, rr .. /7 .. "
		".. /7 .. rr ,, .. ,, ,, ,, rr ,, ,, ,, ,, ,, wr .. /7 .. "
		".. /7 .. rr ,, ,, .. ,, ,, rr ,, bb && bb ,, rr .. /7 .. "
		".. /7 .. rr rr rr bb rr rr rr rr rr rr rr rr rr .. /7 .. "
		".. /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. /7 .. "
		".. /8 /d /d /d /d /d /d /d /1 /d /d /d /d /d /d /d /a .. "
		".. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. .. .. "
	))

;;----------------------------------------------------------------------------
;; NPC's
;;----------------------------------------------------------------------------
(kern-load "meaney.scm")
(kern-load "amy.scm")

;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place
 'p_poor_house     ; tag
 "The Poor House"      ; name
 s_hamlet      ; sprite
 m_poor_house      ; map
 #f              ; wraps
 #f              ; underground
 #f              ; large-scale (wilderness)
 #f              ; tmp combat place
 nil ; subplaces
 nil ; neighbors

 (list ; objects

  ;; npcs
  (put (mk-npc 'bull 6) 14 3)
  (put (mk-meaney) 9 9)
  (put (mk-amy) 9 9)

  ;; beds
  (put (mk-bed) 4 10)
  (put (mk-bed)  4 12)
  (put (mk-bed)  4 14)
  (put (mk-bed)  8 10)
  (put (mk-bed)  8 12)
  (put (mk-bed)  8 14)

  ;; doors
  (put (mk-door-in-rock) 3 6)
  (put (mk-door-in-rock) 12 6)
  
  ;empty doorways
  (put (mk-archway-rock) 6 9)
  (put (mk-archway-rock) 9 8)
  
  ;; chests
  (put (mk-chest
        nil
        '((1 t_food))) 10 14)
  (put (mk-chest
        nil
        '((1 t_gold_coins))) 14 14)

  )

 nil ; hooks
 (list  ; edge entrances
 	(list southwest 10 0)
 )
 )


(mk-place-music p_poor_house 'ml-small-town)
